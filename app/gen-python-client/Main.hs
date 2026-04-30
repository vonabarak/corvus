{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Generate the Python Client class from the Haskell 'Request' and
-- 'Response' types.
--
-- For each success 'Response' variant and each inner record type
-- (e.g. 'VmInfo', 'StatusInfo') we emit a frozen @\@dataclass@. Each
-- 'Client' method pipes the raw response dict through a generated
-- @_decode@ helper that looks up the matching dataclass by tag,
-- recursively decoding nested records and lists. The Request→Response
-- mapping is maintained as a small static table below.
--
-- Invocation: @stack exec gen-python-client > python/corvus_client/_generated.py@
module Main (main) where

import Corvus.Protocol
  ( ApplyCreated
  , ApplyResult
  , BuildOne
  , BuildResult
  , CloudInitInfo
  , DiskImageInfo
  , DriveInfo
  , NetIfInfo
  , NetworkInfo
  , Request
  , Response
  , SharedDirInfo
  , SnapshotInfo
  , SshKeyInfo
  , StatusInfo
  , TaskInfo
  , TemplateDetails
  , TemplateDriveInfo
  , TemplateNetIfInfo
  , TemplateSshKeyInfo
  , TemplateVmInfo
  , VmDetails
  , VmInfo
  )
import Corvus.Protocol.Aeson (camelToSnake)
import Corvus.Protocol.Schema (ConstructorInfo (..), FieldInfo (..), GConstructors, datatypeConstructors)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Char (isAsciiLower, isDigit)
import Data.FileEmbed (embedStringFile)
import Data.List (intercalate, nub, sort, (\\))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable (TypeRep, tyConName, typeRepArgs, typeRepTyCon)
import GHC.Generics (Generic, Rep)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Ginger (GVal, VarName, makeContextText, parseGinger, runGinger, toGVal)

-- ---------------------------------------------------------------------------
-- Request / Response success mapping
--
-- Each 'Request' constructor is paired with the list of 'Response'
-- constructor(s) it produces on the success channel (error responses
-- are translated into exceptions by client_base._call, so they never
-- appear in the return type). Most requests map to a single response;
-- apply + disk-import can return either a synchronous result or an
-- async task-started indicator, so they map to two.
-- ---------------------------------------------------------------------------

successResponses :: [(String, [String])]
successResponses =
  [ ("ReqPing", ["RespPong"])
  , ("ReqStatus", ["RespStatus"])
  , ("ReqShutdown", ["RespShutdownAck"])
  , ("ReqVmList", ["RespVmList"])
  , ("ReqVmShow", ["RespVmDetails"])
  , ("ReqVmCreate", ["RespVmCreated"])
  , ("ReqVmDelete", ["RespVmDeleted"])
  , ("ReqVmStart", ["RespVmStateChanged"])
  , ("ReqVmStop", ["RespVmStateChanged"])
  , ("ReqVmPause", ["RespVmStateChanged"])
  , ("ReqVmReset", ["RespVmStateChanged"])
  , ("ReqVmEdit", ["RespVmEdited"])
  , ("ReqVmCloudInit", ["RespVmEdited"])
  , ("ReqDiskCreate", ["RespDiskCreated"])
  , ("ReqDiskRegister", ["RespDiskCreated"])
  , ("ReqDiskCreateOverlay", ["RespDiskCreated"])
  , ("ReqDiskRefresh", ["RespDiskInfo"])
  , ("ReqDiskDelete", ["RespDiskOk"])
  , ("ReqDiskResize", ["RespDiskOk"])
  , ("ReqDiskList", ["RespDiskList"])
  , ("ReqDiskShow", ["RespDiskInfo"])
  , ("ReqDiskClone", ["RespDiskCreated"])
  , ("ReqDiskRebase", ["RespDiskOk"])
  , ("ReqSnapshotCreate", ["RespSnapshotCreated"])
  , ("ReqSnapshotDelete", ["RespSnapshotOk"])
  , ("ReqSnapshotRollback", ["RespSnapshotOk"])
  , ("ReqSnapshotMerge", ["RespSnapshotOk"])
  , ("ReqSnapshotList", ["RespSnapshotList"])
  , ("ReqDiskAttach", ["RespDiskAttached"])
  , ("ReqDiskDetach", ["RespDiskOk"])
  , ("ReqSharedDirAdd", ["RespSharedDirAdded"])
  , ("ReqSharedDirRemove", ["RespSharedDirOk"])
  , ("ReqSharedDirList", ["RespSharedDirList"])
  , ("ReqNetIfAdd", ["RespNetIfAdded"])
  , ("ReqNetIfRemove", ["RespNetIfOk"])
  , ("ReqNetIfList", ["RespNetIfList"])
  , ("ReqSshKeyCreate", ["RespSshKeyCreated"])
  , ("ReqSshKeyDelete", ["RespSshKeyOk"])
  , ("ReqSshKeyList", ["RespSshKeyList"])
  , ("ReqSshKeyAttach", ["RespSshKeyOk"])
  , ("ReqSshKeyDetach", ["RespSshKeyOk"])
  , ("ReqSshKeyListForVm", ["RespSshKeyList"])
  , ("ReqTemplateCreate", ["RespTemplateCreated"])
  , ("ReqTemplateDelete", ["RespTemplateDeleted"])
  , ("ReqTemplateList", ["RespTemplateList"])
  , ("ReqTemplateShow", ["RespTemplateInfo"])
  , ("ReqTemplateInstantiate", ["RespTemplateInstantiated"])
  , ("ReqTemplateUpdate", ["RespTemplateUpdated"])
  , ("ReqNetworkCreate", ["RespNetworkCreated"])
  , ("ReqNetworkDelete", ["RespNetworkDeleted"])
  , ("ReqNetworkStart", ["RespNetworkStarted"])
  , ("ReqNetworkStop", ["RespNetworkStopped"])
  , ("ReqNetworkList", ["RespNetworkList"])
  , ("ReqNetworkShow", ["RespNetworkDetails"])
  , ("ReqNetworkEdit", ["RespNetworkEdited"])
  , ("ReqGuestExec", ["RespGuestExecResult"])
  , ("ReqDiskImportUrl", ["RespDiskCreated", "RespDiskImportStarted"])
  , ("ReqApply", ["RespApplyResult", "RespApplyStarted"])
  , ("ReqBuild", ["RespBuildResult", "RespBuildStarted"])
  , ("ReqTaskList", ["RespTaskList"])
  , ("ReqTaskShow", ["RespTaskInfo"])
  , ("ReqTaskListChildren", ["RespTaskList"])
  , ("ReqCloudInitSet", ["RespCloudInitOk"])
  , ("ReqCloudInitGet", ["RespCloudInitConfig"])
  , ("ReqCloudInitDelete", ["RespCloudInitOk"])
  , ("ReqSerialConsole", ["RespSerialConsoleOk"])
  , ("ReqSerialConsoleFlush", ["RespSerialConsoleFlushed"])
  , ("ReqHmpMonitor", ["RespHmpMonitorOk"])
  , ("ReqHmpMonitorFlush", ["RespHmpMonitorFlushed"])
  , ("ReqDiskImport", ["RespDiskCreated", "RespDiskImportStarted"])
  , ("ReqVmViewGrant", ["RespVmViewGrant"])
  , ("ReqVmSendCtrlAltDel", ["RespOk"])
  ]

-- ---------------------------------------------------------------------------
-- Template data
-- ---------------------------------------------------------------------------

data RequestSpec = RequestSpec
  { rsCtor :: !Text
  , rsOp :: !Text
  , rsFields :: ![FieldSpec]
  , rsReturn :: !Text
  -- ^ Python return-type expression, e.g. @VmCreated@ or
  -- @Union[ApplyResult, ApplyStarted]@.
  }

data ResponseSpec = ResponseSpec
  { rspCtor :: !Text
  -- ^ Haskell constructor, e.g. @RespVmCreated@.
  , rspClass :: !Text
  -- ^ Python dataclass name, e.g. @VmCreated@ (Resp prefix stripped).
  , rspTag :: !Text
  -- ^ Wire tag, e.g. @vm_created@.
  , rspFields :: ![FieldSpec]
  }

data FieldSpec = FieldSpec
  { fsHaskell :: !Text
  , fsPython :: !Text
  , fsPyType :: !Text
  , fsOptional :: !Bool
  , fsDefault :: !Text
  -- ^ Python default-value suffix: @= None@ for optional fields, else
  -- empty. Precomputed so the Ginger template can emit it as a plain
  -- expression — inline @{% if %}{% endif %}@ strips surrounding
  -- whitespace and collapses multi-field blocks onto one line.
  , fsDecoder :: !Text
  -- ^ Python decoder expression used to rebuild this field from a raw
  -- response dict @p@: e.g. @p["id"]@, @_decode_vm_info(p["vm"])@,
  -- @[_decode_drive_info(x) for x in p["drives"]]@.
  }

data InnerSpec = InnerSpec
  { isName :: !Text
  -- ^ Haskell type name, which is also the Python class name (e.g.
  -- @VmInfo@, @StatusInfo@).
  , isSnake :: !Text
  -- ^ @snake_case@ form used for decoder function names.
  , isFields :: ![FieldSpec]
  }

instance ToJSON RequestSpec where
  toJSON r =
    object
      [ "ctor" .= rsCtor r
      , "op" .= rsOp r
      , "fields" .= rsFields r
      , "return" .= rsReturn r
      ]

instance ToJSON ResponseSpec where
  toJSON r =
    object
      [ "ctor" .= rspCtor r
      , "className" .= rspClass r
      , "tag" .= rspTag r
      , "fields" .= rspFields r
      ]

instance ToJSON FieldSpec where
  toJSON f =
    object
      [ "haskell" .= fsHaskell f
      , "python" .= fsPython f
      , "pyType" .= fsPyType f
      , "optional" .= fsOptional f
      , "default" .= fsDefault f
      , "decoder" .= fsDecoder f
      ]

instance ToJSON InnerSpec where
  toJSON s =
    object
      [ "name" .= isName s
      , "snake" .= isSnake s
      , "fields" .= isFields s
      ]

-- ---------------------------------------------------------------------------
-- Haskell TypeRep → Python annotation
-- ---------------------------------------------------------------------------

data PyType
  = PyInt
  | PyStr
  | PyBool
  | PyAny
  | -- | Named inner dataclass, e.g. @VmInfo@. Rendered as the class name
    -- and decoded via @_decode_<snake>(...)@.
    PyDataclass !String
  | PyOptional PyType
  | PyList PyType
  | PyTuple [PyType]
  deriving (Show)

haskellToPy :: TypeRep -> PyType
haskellToPy tr = case tyConName (typeRepTyCon tr) of
  "Int" -> PyInt
  "Int64" -> PyInt
  "Word8" -> PyInt
  "Text" -> PyStr
  "Bool" -> PyBool
  "Ref" -> PyStr
  "UTCTime" -> PyStr -- aeson renders as ISO-8601 string
  "Maybe" -> case typeRepArgs tr of
    [inner] -> PyOptional (haskellToPy inner)
    _ -> PyAny
  -- Haskell list. In GHC 9.x 'tyConName' returns @"List"@; older
  -- versions returned @"[]"@. Accept both.
  "List" -> listOrAny
  "[]" -> listOrAny
  tyCon
    | isTupleTyCon tyCon -> PyTuple (map haskellToPy (typeRepArgs tr))
    | tyCon `elem` enumTypeNames -> PyStr
    | tyCon `Set.member` innerTypeNameSet -> PyDataclass tyCon
    | otherwise -> PyAny
  where
    listOrAny = case typeRepArgs tr of
      [inner] -> PyList (haskellToPy inner)
      _ -> PyAny

-- | True for tuple type constructors. GHC 9.x reports them as
-- @Tuple2@, @Tuple3@, @…@; older versions used the punctuation form
-- @(,)@, @(,,)@, @…@. Match both.
isTupleTyCon :: String -> Bool
isTupleTyCon s = case s of
  '(' : rest@(_ : _) -> all (== ',') (init rest) && last rest == ')'
  'T' : 'u' : 'p' : 'l' : 'e' : n -> not (null n) && all isDigit n
  _ -> False

-- | Hand-maintained list of Corvus enum types that JSON-serialise as
-- their lower-cased constructor name (see 'Corvus.Model.EnumText').
enumTypeNames :: [String]
enumTypeNames =
  [ "DriveFormat"
  , "DriveInterface"
  , "DriveMedia"
  , "CacheType"
  , "NetInterfaceType"
  , "SharedDirCache"
  , "TaskSubsystem"
  , "TaskResult"
  , "TemplateCloneStrategy"
  , "VmStatus"
  ]

pyAnnotation :: PyType -> Text
pyAnnotation PyInt = "int"
pyAnnotation PyStr = "str"
pyAnnotation PyBool = "bool"
pyAnnotation PyAny = "Any"
pyAnnotation (PyDataclass n) = T.pack n
pyAnnotation (PyOptional t) = "Optional[" <> pyAnnotation t <> "]"
pyAnnotation (PyList t) = "list[" <> pyAnnotation t <> "]"
pyAnnotation (PyTuple ts) = "tuple[" <> T.intercalate ", " (map pyAnnotation ts) <> "]"

isOptional :: PyType -> Bool
isOptional (PyOptional _) = True
isOptional _ = False

-- | Decoder expression: given a value-access Python expression
-- (e.g. @p["foo"]@), produce the code that converts that raw JSON value
-- into the typed Python representation. Primitives/@Any@/tuples pass
-- through; dataclass types invoke @_decode_<snake>(...)@; lists map
-- the decoder; @Optional@ guards the inner decoder with @... if ... is
-- not None else None@ but only when the inner decoder is non-trivial.
decodeValue :: PyType -> Text -> Text
decodeValue PyInt e = e
decodeValue PyStr e = e
decodeValue PyBool e = e
decodeValue PyAny e = e
decodeValue (PyTuple _) e = e
decodeValue (PyDataclass n) e =
  "_decode_" <> T.pack (camelToSnake n) <> "(" <> e <> ")"
decodeValue (PyList inner) e
  | isTrivial inner = e
  | otherwise = "[" <> decodeValue inner "x" <> " for x in " <> e <> "]"
decodeValue (PyOptional inner) e
  | isTrivial inner = e
  | otherwise = decodeValue inner e <> " if " <> e <> " is not None else None"

-- | A Python type that needs no runtime conversion — already the right
-- shape in the JSON dict.
isTrivial :: PyType -> Bool
isTrivial PyInt = True
isTrivial PyStr = True
isTrivial PyBool = True
isTrivial PyAny = True
isTrivial (PyTuple _) = True
isTrivial (PyList inner) = isTrivial inner
isTrivial (PyOptional inner) = isTrivial inner
isTrivial (PyDataclass _) = False

-- ---------------------------------------------------------------------------
-- Constructor enumeration
-- ---------------------------------------------------------------------------

requestCtors :: [ConstructorInfo]
requestCtors = datatypeConstructors (Proxy :: Proxy Request)

responseCtors :: [ConstructorInfo]
responseCtors = datatypeConstructors (Proxy :: Proxy Response)

-- | Lookup by constructor name (for quick Response-by-name resolution).
responsesByName :: Map.Map String ConstructorInfo
responsesByName = Map.fromList [(ciName ci, ci) | ci <- responseCtors]

-- | A single-constructor inner record: returns (type name, fields).
innerTypeFields
  :: forall a
   . (Generic a, GConstructors (Rep a))
  => Proxy a
  -> (String, [FieldInfo])
innerTypeFields p = case datatypeConstructors p of
  [ci] -> (ciName ci, ciFields ci)
  _ -> error "innerTypeFields: expected a single-constructor record"

-- | Hand-maintained list of inner record types to emit as Python
-- dataclasses. Each entry is reflected via 'datatypeConstructors' to
-- obtain field names and type reps — the name/prefix only matters in
-- the 'ToJSON' 'innerOptions' which strips per-type lowercase prefixes
-- uniformly.
innerTypes :: [(String, [FieldInfo])]
innerTypes =
  [ innerTypeFields (Proxy :: Proxy StatusInfo)
  , innerTypeFields (Proxy :: Proxy VmInfo)
  , innerTypeFields (Proxy :: Proxy DriveInfo)
  , innerTypeFields (Proxy :: Proxy NetIfInfo)
  , innerTypeFields (Proxy :: Proxy VmDetails)
  , innerTypeFields (Proxy :: Proxy DiskImageInfo)
  , innerTypeFields (Proxy :: Proxy SnapshotInfo)
  , innerTypeFields (Proxy :: Proxy NetworkInfo)
  , innerTypeFields (Proxy :: Proxy CloudInitInfo)
  , innerTypeFields (Proxy :: Proxy SshKeyInfo)
  , innerTypeFields (Proxy :: Proxy TemplateVmInfo)
  , innerTypeFields (Proxy :: Proxy TemplateDriveInfo)
  , innerTypeFields (Proxy :: Proxy TemplateNetIfInfo)
  , innerTypeFields (Proxy :: Proxy TemplateSshKeyInfo)
  , innerTypeFields (Proxy :: Proxy TemplateDetails)
  , innerTypeFields (Proxy :: Proxy SharedDirInfo)
  , innerTypeFields (Proxy :: Proxy TaskInfo)
  , innerTypeFields (Proxy :: Proxy ApplyCreated)
  , innerTypeFields (Proxy :: Proxy ApplyResult)
  , innerTypeFields (Proxy :: Proxy BuildOne)
  , innerTypeFields (Proxy :: Proxy BuildResult)
  ]

-- | Set of known inner type names (for 'haskellToPy' to recognise and
-- map to @PyDataclass@).
innerTypeNameSet :: Set.Set String
innerTypeNameSet = Set.fromList (map fst innerTypes)

-- | Outer field spec — for @Request@ and @Response@ constructors whose
-- Aeson 'fieldLabelModifier' is just 'camelToSnake' (no prefix strip).
toOuterFieldSpec :: FieldInfo -> FieldSpec
toOuterFieldSpec = mkFieldSpec id

-- | Inner field spec — for records like 'VmInfo' whose Aeson
-- 'fieldLabelModifier' is @camelToSnake . dropLowerPrefix@, so e.g.
-- @viId@ → @id@, @vdNetIfs@ → @net_ifs@.
toInnerFieldSpec :: FieldInfo -> FieldSpec
toInnerFieldSpec = mkFieldSpec stripLowerPrefix

mkFieldSpec :: (String -> String) -> FieldInfo -> FieldSpec
mkFieldSpec nameXform fi =
  let ty = haskellToPy (fiTypeRep fi)
      opt = isOptional ty
      key = T.pack (camelToSnake (nameXform (fiName fi)))
      access = (if opt then "p.get(\"" else "p[\"") <> key <> (if opt then "\")" else "\"]")
   in FieldSpec
        { fsHaskell = T.pack (fiName fi)
        , fsPython = key
        , fsPyType = pyAnnotation ty
        , fsOptional = opt
        , fsDefault = if opt then " = None" else ""
        , fsDecoder = decodeValue ty access
        }

-- | Strip leading lowercase characters. Mirrors
-- 'Corvus.Protocol.Aeson.dropLowerPrefix' so field names generated here
-- match what the daemon emits in JSON.
stripLowerPrefix :: String -> String
stripLowerPrefix = dropWhile isAsciiLower

-- | Field type-reps we couldn't map to a concrete Python annotation.
-- Returned so 'main' can print a stderr warning; silently falling back
-- to @Any@ would let a new field type land in the protocol without
-- anyone noticing the codegen regression.
unknownTypes :: [(String, String, TypeRep)]
unknownTypes =
  [ (owner, fiName fi, fiTypeRep fi)
  | (owner, fields) <-
      [(ciName ci, ciFields ci) | ci <- requestCtors ++ responseCtors]
        ++ innerTypes
  , fi <- fields
  , isAny (haskellToPy (fiTypeRep fi))
  ]
  where
    isAny PyAny = True
    isAny (PyOptional t) = isAny t
    isAny (PyList t) = isAny t
    isAny (PyTuple ts) = any isAny ts
    isAny _ = False

dropReqPrefix, dropRespPrefix :: String -> String
dropReqPrefix = dropPrefix "Req"
dropRespPrefix = dropPrefix "Resp"

dropPrefix :: String -> String -> String
dropPrefix p s
  | take (length p) s == p = drop (length p) s
  | otherwise = s

-- | The set of Response constructors we need to emit TypedDicts for —
-- just the success responses used by at least one Request.
neededResponses :: [String]
neededResponses = nub (sort (concatMap snd successResponses))

responseSpecs :: [ResponseSpec]
responseSpecs =
  [ ResponseSpec
    { rspCtor = T.pack (ciName ci)
    , rspClass = responseClassName (ciName ci)
    , rspTag = T.pack (camelToSnake (dropRespPrefix (ciName ci)))
    , -- Required fields before optional: dataclasses refuse
      -- required-after-default, and optional fields get @= None@.
      rspFields = orderForDataclass (map toOuterFieldSpec (ciFields ci))
    }
  | name <- neededResponses
  , Just ci <- [Map.lookup name responsesByName]
  ]

-- | Python class name for a @Resp*@ constructor. Strips the @Resp@
-- prefix and appends @Response@ — the suffix both avoids collisions
-- with inner dataclass names (@VmDetails@, @TaskInfo@, @ApplyResult@
-- are both response constructors and payload records) and makes the
-- role of these classes explicit at the call site.
responseClassName :: String -> Text
responseClassName name = T.pack (dropRespPrefix name) <> "Response"

innerSpecs :: [InnerSpec]
innerSpecs =
  [ InnerSpec
    { isName = T.pack name
    , isSnake = T.pack (camelToSnake name)
    , isFields = orderForDataclass (map toInnerFieldSpec fields)
    }
  | (name, fields) <- innerTypes
  ]

-- | Stable partition: non-optional fields first, optional fields last.
-- Required for @\@dataclass@ so fields with defaults don't precede
-- fields without.
orderForDataclass :: [FieldSpec] -> [FieldSpec]
orderForDataclass fs = filter (not . fsOptional) fs ++ filter fsOptional fs

requestSpecs :: [RequestSpec]
requestSpecs =
  [ RequestSpec
    { rsCtor = T.pack ctor
    , rsOp = T.pack (camelToSnake (dropReqPrefix ctor))
    , -- Required args before optional args: Python refuses
      -- default-before-required, and optional args get @= None@.
      rsFields = orderForDataclass (map toOuterFieldSpec (ciFields ci))
    , rsReturn = returnTypeFor ctor
    }
  | ci <- requestCtors
  , let ctor = ciName ci
  ]

-- | Python return-type expression for a given Request constructor.
-- Joins multiple success responses with @Union[...]@.
returnTypeFor :: String -> Text
returnTypeFor ctor = case lookup ctor successResponses of
  Nothing -> "Any"
  Just [single] -> responseClassName single
  Just many ->
    "Union["
      <> T.intercalate ", " (map responseClassName many)
      <> "]"

-- ---------------------------------------------------------------------------
-- Template
-- ---------------------------------------------------------------------------

-- | Ginger template embedded at compile time from
-- @python/template.py.ginger@. The template lives next to the
-- @python/@ package it generates into, not next to the Haskell
-- generator — keeping it with its output neighbours makes the
-- docstring/indentation conventions easier to keep in sync with the
-- hand-written Python (@client_base.py@, @exceptions.py@).
template :: Text
template = T.pack $(embedStringFile "python/template.py.ginger")

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  -- Sanity-check: every reflected Request constructor must appear in
  -- the successResponses table. If a new Req* is added to Protocol.hs
  -- and forgotten here, fail loudly rather than emit an @Any@ return.
  let reflectedCtors = map ciName requestCtors
      mappedCtors = map fst successResponses
      missing = reflectedCtors \\ mappedCtors
      extra = mappedCtors \\ reflectedCtors
  case (missing, extra) of
    ([], []) -> pure ()
    _ -> do
      hPutStrLn stderr "gen-python-client: successResponses out of sync with Request:"
      mapM_ (\c -> hPutStrLn stderr $ "  missing from successResponses: " <> c) missing
      mapM_ (\c -> hPutStrLn stderr $ "  extra in successResponses:     " <> c) extra
      exitFailure

  -- Warn (non-fatally) about field types we couldn't map to a concrete
  -- Python annotation. Anything in this list becomes @Any@ in the
  -- generated client — usually a new enum that needs adding to
  -- 'enumTypeNames', or a new nested record worth its own TypedDict.
  let warnUnknown (ctor, field, tr) =
        hPutStrLn stderr $
          "gen-python-client: WARNING: "
            <> ctor
            <> "."
            <> field
            <> " has unmapped type "
            <> show tr
            <> " — falling back to Any"
  mapM_ warnUnknown unknownTypes

  parsed <- parseGinger (const (pure Nothing)) Nothing (T.unpack template)
  case parsed of
    Left err -> do
      hPutStrLn stderr ("Template parse error: " <> show err)
      exitFailure
    Right tpl -> do
      let lookupVar :: VarName -> GVal m
          lookupVar v = case v of
            "requests" -> toGVal (toJSON requestSpecs)
            "responses" -> toGVal (toJSON responseSpecs)
            "inners" -> toGVal (toJSON innerSpecs)
            _ -> toGVal ()
          ctx = makeContextText lookupVar
          out = runGinger ctx tpl :: Text
      TIO.putStr out
