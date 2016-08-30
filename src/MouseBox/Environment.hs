{-# LANGUAGE OverloadedStrings, TemplateHaskell, PackageImports #-}

module MouseBox.Environment(
                           CapturedEnvironment(..),
                           mouseBoxPlace_CE,
                           environment_CE,
                           justCreated_CE,

                           SerializedEnvironment(..),
                           disambiguator_SE,
                           businessName_SE,
                           country_SE,

                           captureEnvironment,
                           persistentCARegistryFromFile,
                           savePersistentCARegistryToFile
                           ) where


import           Control.Monad                                         (--when ,
                                                                         unless
                                                                        )
import           Control.Lens
import           Data.Maybe                                            (
                                                                        fromMaybe
                                                                       )

import qualified Data.ByteString                                       as B
import qualified Data.ByteString.Lazy                                  as LB
import qualified Data.Binary                                           as Bn
import           Data.ByteString.Char8                                 (pack, unpack)
import           Data.Aeson                                            (decode, encode)
import qualified Data.Aeson                                            as Ae
import           Data.Aeson
import           Data.Aeson.TH

--import qualified System.Posix.Env.ByteString                           as SPE
import           System.Environment
--import           System.Posix.Types                                    (FileMode)
import           System.FilePath                                       ((</>))
import qualified System.PosixCompat.Files                              as SPF
--import qualified System.Posix.Directory.ByteString                     as SPD

--import qualified "crypto-api" Crypto.Random                            as CR
--import           Control.Monad.CryptoRandom

import           MouseBox.JSONHelpers
import           MouseBox.CertificationAuthority
import           MouseBox.Utils                                        (shortRandomName, recursivelyCreateDirectory)



-- Using String here due to JSON serialization
data SerializedEnvironment = SerializedEnvironment {
    _disambiguator_SE :: String,

    -- The fields below are only required to produce a "nice"
    -- certificate signing request that can be used in other places

    _businessName_SE   :: String,
    _country_SE        :: String
    }
    deriving Show

makeLenses ''SerializedEnvironment


data CapturedEnvironment = CapturedEnvironment {
    -- Directory under home dir...
    _mouseBoxPlace_CE :: B.ByteString

  , _environment_CE  :: SerializedEnvironment

  , _justCreated_CE :: Bool
    }
    deriving Show

makeLenses ''CapturedEnvironment


-- $(deriveJSON
--       defaultOptions
--       {fieldLabelModifier= unpack . extractPropertyName . pack }
--       ''SerializedEnvironment
--  )
instance Ae.FromJSON SerializedEnvironment where
    parseJSON (Object v) = SerializedEnvironment
        <$>
        (v .: "disambiguator") <*>
        (v .:? "company" .!= "Widgets LTD" )
        (v .:? "country" .!= "FR") <*>


instance Ae.ToJSON SerializedEnvironment where
    toJSON (SerializedEnvironment d b c) =
        Ae.object [
            "disambiguator" Ae..= d,
            "company" Ae..= b,
            "country" Ae..= c,
               ]




newSerializedEnvironment :: IO SerializedEnvironment
newSerializedEnvironment = do
    srn <- shortRandomName 6
    business_name <- fromMaybe "Widgets" <$> lookupEnv "MOUSEBOX__BUSINESS_NAME"
    country <- fromMaybe "FR" <$> lookupEnv "MOUSEBOX__COUNTRY"
    return SerializedEnvironment {
        _disambiguator_SE = unpack srn,
        _businessName_SE = business_name,
        _country_SE = country
        }


missingEnvironmnetVariable :: B.ByteString -> IO B.ByteString
missingEnvironmnetVariable v = do
    maybe_varvalue <- lookupEnv $ unpack v
    case maybe_varvalue of
        Nothing -> do
            B.putStr $ "Missing variable " `mappend` v `mappend` " in environment"
            error "MissingVariable"

        Just value -> return . pack $  value


captureEnvironment :: IO CapturedEnvironment
captureEnvironment = do
    home_directory <- missingEnvironmnetVariable "HOME"
    let
        mouse_box_place = unpack home_directory </> ".config" </> "mousebox"
        mouse_box_config = mouse_box_place </> "mousebox.conf"
    recursivelyCreateDirectory $ pack mouse_box_place
    mouse_box_config_exists <- {-# SCC fileExist #-} SPF.fileExist mouse_box_config
    se <- {-# SCC ifPart #-} if mouse_box_config_exists
      then do
        contents <- B.readFile $  mouse_box_config
        case decode . LB.fromStrict $ contents of
            Just se'  -> return se'
            Nothing   -> error "Invalid mousebox.conf file"
      else do
        se' <- newSerializedEnvironment
        LB.writeFile ( mouse_box_config) (encode se')
        return se'

    let
        ce = CapturedEnvironment {
            _mouseBoxPlace_CE = pack mouse_box_place
          , _environment_CE   = se
          , _justCreated_CE = not mouse_box_config_exists
          }

    unless mouse_box_config_exists $ newCertificationAuthorityToFile ce

    return ce


-- | Writes both the registry and the certificate to the place in
--   /home/user/.config/mousebox/
newCertificationAuthorityToFile :: CapturedEnvironment -> IO ()
newCertificationAuthorityToFile captured_environment = do
    let
        mouse_box_place = captured_environment ^. mouseBoxPlace_CE
        ca_persistent_registry_file = unpack mouse_box_place </> "ca_persistent_registry.bin"
        ca_root_cert_fname = unpack mouse_box_place </> ("mousebox_ca_root_" `mappend` unpack disambiguator `mappend` ".pem")
        disambiguator = pack $ captured_environment ^. ( environment_CE . disambiguator_SE )
    persistent_registry <- newPersistentCARegistry disambiguator
    B.writeFile (ca_persistent_registry_file) (LB.toStrict $ Bn.encode persistent_registry)
    -- And while we are at that, we can also get a new root certificate
    let
        pem_ca_root_cert   = createCACertificate persistent_registry
    B.writeFile ca_root_cert_fname pem_ca_root_cert


persistentCARegistryFromFile :: CapturedEnvironment -> IO PersistentCARegistry
persistentCARegistryFromFile captured_environment =  do
    let
        mouse_box_place = captured_environment ^. mouseBoxPlace_CE
        ca_persistent_registry_file = unpack mouse_box_place </> "ca_persistent_registry.bin"
    -- Read this way, we don't want to keep the file open and locked
    bs <- LB.fromStrict <$> B.readFile ca_persistent_registry_file
    -- And while we are at that, we can also get a new root certificate
    let
        persistent_registry = Bn.decode bs
    return persistent_registry


savePersistentCARegistryToFile :: CapturedEnvironment -> PersistentCARegistry -> IO ()
savePersistentCARegistryToFile ce ca_registry = do
    let
        mouse_box_place = ce ^. mouseBoxPlace_CE
        ca_persistent_registry_file = unpack mouse_box_place </> "ca_persistent_registry.bin"
    B.writeFile ca_persistent_registry_file (LB.toStrict . Bn.encode $ ca_registry)
