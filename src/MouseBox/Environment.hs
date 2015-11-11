{-# LANGUAGE OverloadedStrings, TemplateHaskell, PackageImports #-}

module MouseBox.Environment(
                           CapturedEnvironment(..),
                           mouseBoxPlace_CE,
                           environment_CE,

                           SerializedEnvironment(..),
                           disambiguator_SE,

                           captureEnvironment,
                           persistentCARegistryFromFile,
                           savePersistentCARegistryToFile
                           ) where


import           Control.Monad                                         (--when ,
                                                                         unless
                                                                        )
import           Control.Lens

import qualified Data.ByteString                                       as B
import qualified Data.ByteString.Lazy                                  as LB
import qualified Data.Binary                                           as Bn
import           Data.ByteString.Char8                                 (pack, unpack)
import           Data.Aeson                                            (decode, encode)
import           Data.Aeson.TH

import qualified System.Posix.Env.ByteString                           as SPE
--import           System.Posix.Types                                    (FileMode)
import           System.Posix.FilePath
import qualified System.Posix.Files.ByteString                         as SPF
--import qualified System.Posix.Directory.ByteString                     as SPD

--import qualified "crypto-api" Crypto.Random                            as CR
--import           Control.Monad.CryptoRandom

import           MouseBox.JSONHelpers
import           MouseBox.CertificationAuthority
import           MouseBox.Utils                                        (shortRandomName, recursivelyCreateDirectory)



data SerializedEnvironment = SerializedEnvironment {
    _disambiguator_SE :: String
    }
    deriving Show

makeLenses ''SerializedEnvironment


data CapturedEnvironment = CapturedEnvironment {
    -- Directory under home dir...
    _mouseBoxPlace_CE :: B.ByteString

    ,_environment_CE  :: SerializedEnvironment
    }
    deriving Show

makeLenses ''CapturedEnvironment


$(deriveJSON defaultOptions{fieldLabelModifier= unpack . extractPropertyName . pack } ''SerializedEnvironment)


newSerializedEnvironment :: IO SerializedEnvironment
newSerializedEnvironment = do
    srn <- shortRandomName 6
    return SerializedEnvironment {
        _disambiguator_SE = unpack srn
        }


missingEnvironmnetVariable :: B.ByteString -> IO B.ByteString
missingEnvironmnetVariable v = do
    maybe_varvalue <- SPE.getEnv v
    case maybe_varvalue of
        Nothing -> do
            B.putStr $ "Missing variable " `mappend` v `mappend` " in environment"
            error "MissingVariable"

        Just value -> return value


captureEnvironment :: IO CapturedEnvironment
captureEnvironment = do
    home_directory <- missingEnvironmnetVariable "HOME"
    let
        mouse_box_place = home_directory </> ".config" </> "mousebox"
        mouse_box_config = mouse_box_place </> "mousebox.conf"
    recursivelyCreateDirectory mouse_box_place
    mouse_box_config_exists <- {-# SCC fileExist #-} SPF.fileExist mouse_box_config
    se <- {-# SCC ifPart #-} if mouse_box_config_exists
      then do
        contents <- B.readFile $ unpack mouse_box_config
        case decode . LB.fromStrict $ contents of
            Just se'  -> return se'
            Nothing   -> error "Invalid mousebox.conf file"
      else do
        putStrLn "No user-configuration found, creating"
        se' <- newSerializedEnvironment
        LB.writeFile (unpack  mouse_box_config) (encode se')
        return se'

    let
        ce = CapturedEnvironment {
            _mouseBoxPlace_CE = mouse_box_place
          , _environment_CE   = se
          }

    unless mouse_box_config_exists $ newCertificationAuthorityToFile ce

    return ce


-- | Writes both the registry and the certificate to the place in
--   /home/user/.config/mousebox/
newCertificationAuthorityToFile :: CapturedEnvironment -> IO ()
newCertificationAuthorityToFile captured_environment = do
    let
        mouse_box_place = captured_environment ^. mouseBoxPlace_CE
        ca_persistent_registry_file = mouse_box_place </> "ca_persistent_registry.bin"
        ca_root_cert_fname = mouse_box_place </> ("mousebox_ca_root_" `mappend` disambiguator `mappend` ".pem")
        disambiguator = pack $ captured_environment ^. ( environment_CE . disambiguator_SE )
    persistent_registry <- newPersistentCARegistry disambiguator
    B.writeFile (unpack ca_persistent_registry_file) (LB.toStrict $ Bn.encode persistent_registry)
    -- And while we are at that, we can also get a new root certificate
    let
        pem_ca_root_cert   = createCACertificate persistent_registry
    B.writeFile (unpack ca_root_cert_fname) pem_ca_root_cert


persistentCARegistryFromFile :: CapturedEnvironment -> IO PersistentCARegistry
persistentCARegistryFromFile captured_environment =  do
    let
        mouse_box_place = captured_environment ^. mouseBoxPlace_CE
        ca_persistent_registry_file = mouse_box_place </> "ca_persistent_registry.bin"
    -- Read this way, we don't want to keep the file open and locked
    bs <- LB.fromStrict <$> B.readFile (unpack ca_persistent_registry_file)
    -- And while we are at that, we can also get a new root certificate
    let
        persistent_registry = Bn.decode bs
    return persistent_registry


savePersistentCARegistryToFile :: CapturedEnvironment -> PersistentCARegistry -> IO ()
savePersistentCARegistryToFile ce ca_registry = do
    let
        mouse_box_place = ce ^. mouseBoxPlace_CE
        ca_persistent_registry_file = mouse_box_place </> "ca_persistent_registry.bin"
    B.writeFile (unpack ca_persistent_registry_file) (LB.toStrict . Bn.encode $ ca_registry)
