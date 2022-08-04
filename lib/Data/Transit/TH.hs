{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Transit.TH (deriveTransit, deriveTransitWithCamelCasing) where

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Transit
import qualified Data.Vector as Vector
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Text.Casing (camel, kebab)

deriveTransit :: Name -> DecsQ
deriveTransit = deriveTransitWithCasing kebab

deriveTransitWithCamelCasing :: Name -> DecsQ
deriveTransitWithCamelCasing = deriveTransitWithCasing camel

deriveTransitWithCasing :: (String -> String) -> Name -> DecsQ
deriveTransitWithCasing casing name = do
  Just typeName <- lookupTypeName $ show name
  datatype <- reifyDatatype typeName
  let result = case datatypeCons datatype of
                  [singleConstructor] -> 
                    case constructorVariant singleConstructor of
                      RecordConstructor [_] -> deriveNewtypeTransit typeName singleConstructor
                      _ -> deriveProductTransit casing typeName singleConstructor
                  multipleConstructors -> deriveSumTransit typeName multipleConstructors
  runIO $ writeFile ("code-gen/" <> nameBase name <> "_Transit.hs") (show $ ppr result)
  pure result

deriveNewtypeTransit :: Name -> ConstructorInfo -> [Dec]
deriveNewtypeTransit typeName constructor =
  let fromTransit = InstanceD Nothing [] (AppT (ConT ''FromTransit) (ConT typeName)) [fromTransitDefinition]
      toTransit = InstanceD Nothing [] (AppT (ConT ''ToTransit) (ConT typeName)) [toTransitDefinition]
  in [ fromTransit, toTransit ]
  where
    fromTransitDefinition =
      FunD 'fromTransit
        [ Clause [ VarP valueName ]
            (NormalB $ multiAppE (VarE 'fmap) [ConE $ constructorName constructor, AppE (VarE 'fromTransit) (VarE valueName)]) [] ]

    toTransitDefinition =
      FunD 'toTransit
        [ Clause [ ConP (constructorName constructor) [VarP valueName] ]
            (NormalB $ AppE (VarE 'toTransit) (VarE valueName)) [] ]

    valueName = mkName "value"


deriveSumTransit :: Name -> [ConstructorInfo] -> [Dec]
deriveSumTransit typeName constructors =
  let fromTransit = deriveSumFromTransit typeName constructors
      toTransit = deriveSumToTransit typeName constructors
  in [ fromTransit, toTransit ]

deriveSumFromTransit :: Name -> [ConstructorInfo] -> Dec
deriveSumFromTransit typeName constructors =
  InstanceD Nothing [] (AppT (ConT ''FromTransit) (ConT typeName)) [fromTransitDefinition]
  where
    fromTransitDefinition =
      FunD 'fromTransit $ snoc (fmap (\constructor ->
        case constructorFields constructor of
          [] ->
            Clause [ ConP 'Keyword [toTransitNameLiteral constructor] ]
              (NormalB $ AppE (VarE 'pure) (ConE $ constructorName constructor)) []
          fields ->
            Clause [ ConP 'Array [ListP $ cons (ConP 'Keyword [toTransitNameLiteral constructor]) (VarP . makeFieldVariableName <$> fields) ] ]
              (NormalB $ DoE Nothing $ snoc (bindExpressions fields) (returnExpression constructor fields)) []
        ) constructors) failurePattern

    bindExpressions = fmap (\field -> BindS (VarP $ makeFieldVariableParseResultName field) (AppE (VarE 'fromTransit) (VarE $ makeFieldVariableName field)))

    returnExpression constructor fields =
      NoBindS $ AppE (VarE 'pure) (multiAppE (ConE $ constructorName constructor) (VarE . makeFieldVariableParseResultName <$> fields))

    failurePattern = Clause [ VarP otherName ] (NormalB $ multiAppE (VarE 'typeMismatch) [ LitE (StringL "Array or Keyword"), VarE otherName ] ) []

    otherName = mkName "other"

    makeFieldVariableParseResultName = mkName . (<> "Result") . show . makeFieldVariableName

    makeFieldVariableName field = mkName $
      case nameBase $ headOfType field of
        "[]" -> "list"
        other -> camel other

    toTransitNameLiteral constructor = LitP $ StringL (toTransitName $ constructorName constructor)


deriveSumToTransit :: Name -> [ConstructorInfo] -> Dec
deriveSumToTransit typeName constructors =
  InstanceD Nothing [] (AppT (ConT ''ToTransit) (ConT typeName)) [toTransitDefinition]
  where
    toTransitDefinition =
      FunD 'toTransit $ fmap (\constructor ->
        case constructorFields constructor of
          []     -> zeroFieldDefinition constructor
          fields -> nonZeroFieldDefinition constructor fields
        ) constructors

    toTransitNameLiteral constructor = LitE $ StringL (toTransitName $ constructorName constructor)

    zeroFieldDefinition constructor =
      Clause [ ConP (constructorName constructor) [] ]
        (NormalB $ AppE (ConE 'Keyword) (toTransitNameLiteral constructor)) []

    nonZeroFieldDefinition constructor fields =
      Clause [ ConP (constructorName constructor) ((\(i, _) -> VarP $ mkName $ "field" <> show i) <$> zip [1 ..] fields) ]
        (NormalB $ AppE (ConE 'Array) (AppE (VarE 'Vector.fromList)
          (ListE $ cons
            (AppE (ConE 'Keyword) (toTransitNameLiteral constructor))
            ((\(i, _) -> AppE (VarE 'toTransit) (VarE $ mkName $ "field" <> show i)) <$> zip [1 ..] fields)))) []

toHaskellName :: Name -> String
toHaskellName = camel . T.unpack . last . T.split (== '_') . T.pack . nameBase

toTransitName :: Name -> String
toTransitName = kebab . T.unpack . last . T.split (== '_') . T.pack . nameBase

deriveProductTransit :: (String -> String) -> Name -> ConstructorInfo -> [Dec]
deriveProductTransit casing typeName constructor =
  let fields = productFields constructor
      fromTransit = deriveProductFromTransit casing typeName (constructorName constructor) fields
      toTransit = deriveProductToTransit casing typeName fields
  in [ fromTransit, toTransit ]

deriveProductFromTransit :: (String -> String) -> Name -> Name -> [FieldInfo] -> Dec
deriveProductFromTransit casing typeName constructorName fields =
  InstanceD Nothing [] (AppT (ConT ''FromTransit) (ConT typeName)) [fromTransitDefinition]
  where
    fromTransitDefinition =
      FunD 'fromTransit
        [ Clause [] (NormalB $ LamCaseE
          [ Match (ConP 'Map [VarP mapName]) (NormalB matchMapBody) []
          , Match (VarP (mkName "other")) (NormalB matchOtherBody) []
          ]
        ) []]

    matchMapBody =
      DoE Nothing $ snoc mkFieldExpressions returnExpression

    matchOtherBody =
      AppE (AppE (VarE 'typeMismatch) (LitE $ StringL "Map")) (VarE otherName)

    mkFieldExpressions =
      fmap (mkFieldExpression . shortName) fields

    mkFieldExpression fieldName =
      BindS (VarP $ mkName fieldName) $ AppE (AppE (VarE 'mapGetKeyword) (LitE $ StringL $ casing fieldName)) (VarE mapName)

    returnExpression =
      NoBindS $ AppE (VarE 'pure) (multiAppE (ConE constructorName) (VarE . mkName . shortName <$> fields))

    mapName = mkName "map"
    otherName = mkName "other"

deriveProductToTransit :: (String -> String) -> Name -> [FieldInfo] -> Dec
deriveProductToTransit casing typeName fields =
  InstanceD Nothing [] (AppT (ConT ''ToTransit) (ConT typeName)) [toTransitDefinition]
  where
    toTransitDefinition =
      FunD 'toTransit
        [ Clause [VarP objectName] (NormalB body)  [] ]

    body = AppE (ConE 'Map) (AppE (VarE 'Map.fromList) (ListE mkFieldExpressions))

    mkFieldExpressions =
      fmap (\field ->
        case fieldType field of
          AppT (ConT f) _ ->
            if f == ''Maybe
              then mkOptionalFieldExpression (shortName field) (accessorName field)
              else mkRequiredFieldExpression (shortName field) (accessorName field)
          _ -> mkRequiredFieldExpression (shortName field) (accessorName field)) fields

    mkRequiredFieldExpression =
      mkFieldExpression

    mkOptionalFieldExpression =
      mkFieldExpression

    mkFieldExpression shortName accessorName =
      TupE [ Just $ AppE (ConE 'Keyword) (LitE $ StringL $ casing shortName)
           , Just $ AppE (VarE 'Data.Transit.toTransit) (AppE (VarE $ mkName accessorName) (VarE objectName))
           ]

    objectName = mkName "object"

data FieldInfo = FieldInfo
  { accessorName :: String
  , shortName    :: String
  , fieldType    :: Type
  }

productFields :: ConstructorInfo -> [FieldInfo]
productFields consInfo =
  case constructorVariant consInfo of
    RecordConstructor names ->
      zipWith (\name type' ->
        FieldInfo
          (nameBase name)
          (T.unpack . last . T.split (== '_') . T.pack $ nameBase name)
          type'
        ) names (constructorFields consInfo)
    _ -> []


multiAppE :: Exp -> [Exp] -> Exp
multiAppE = foldl' AppE

headOfType :: Type -> Name
headOfType (ForallT _ _ ty)  = headOfType ty
headOfType (VarT name)       = name
headOfType (ConT name)       = name
headOfType (TupleT n)        = tupleTypeName n
headOfType ArrowT            = ''(->)
headOfType ListT             = ''[]
headOfType (AppT t _)        = headOfType t
headOfType (SigT t _)        = headOfType t
headOfType (UnboxedTupleT n) = unboxedTupleTypeName n
headOfType other             = error $ show other

cons :: a -> [a] -> [a]
cons = (:) 

snoc :: [a] -> a -> [a]
snoc as a =
  as <> [a]