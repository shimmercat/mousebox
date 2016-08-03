{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Simple high-level functions
module MouseBox.Wrapped(
                 WrappedConfigForMouseboxf(..)
               , gotoDir_WCM
               , mouseboxf_WCM
               , outputRel_WCM
               , outputCert_WCM
               , outputPrivKey_WCM

               , newDefaultWrappedConfigForMouseboxf
               , mouseBoxPerform
               , mouseBoxPerformWithDomains

               , mouseBoxMain
       ) where



import           Control.Lens

import qualified Data.ByteString                                       as B
--import qualified Data.ByteString.Lazy                                  as LB
--import qualified Data.Binary                                           as Bn
import           Data.ByteString.Char8                                 ( pack, unpack)
import qualified Data.Yaml                                             as Y (decodeFile)
import qualified Data.Text                                             as Tx
import           Data.PEM

import           System.Directory

import           System.IO                                             (stderr)
import           System.FilePath                                       ((</>))


import qualified Options.Applicative                                   as OA
import           Options.Applicative                                   ( (<>) )

import           MouseBox.Environment
import           MouseBox.LeafCertificate                              (makeLeafCertificate, DomainList, makeCertificateSigningRequest)
import           MouseBox.Mouseboxf
import           MouseBox.Utils                                        (recursivelyCreateDirectory)



type RawFilePath = B.ByteString


data WrappedConfigForMouseboxf = WrappedConfigForMouseboxf {
         _gotoDir_WCM            :: RawFilePath
       , _mouseboxf_WCM          :: RawFilePath
       , _outputRel_WCM          :: RawFilePath
       , _outputCert_WCM         :: RawFilePath
       , _outputPrivKey_WCM      :: RawFilePath
       , _csr_WCM                :: RawFilePath
       , _outputPrivKeyPKCS8_WCM :: RawFilePath
    }

makeLenses ''WrappedConfigForMouseboxf


newDefaultWrappedConfigForMouseboxf :: IO WrappedConfigForMouseboxf
newDefaultWrappedConfigForMouseboxf = do
    working_dir <- getCurrentDirectory
    return WrappedConfigForMouseboxf {
          _gotoDir_WCM            = pack working_dir
        , _mouseboxf_WCM          = "mouseboxf"
        , _outputRel_WCM          = "_priv"
        , _outputCert_WCM         = "cert.pem"
        , _outputPrivKey_WCM      = "privkey.pem"
        , _csr_WCM                = "cert.csr"
        , _outputPrivKeyPKCS8_WCM = "privkey.unencrypted-pkcs8.pem"
        }


mouseBoxPerform :: CapturedEnvironment -> WrappedConfigForMouseboxf ->  IO ()
mouseBoxPerform captured_environment wrapped_config = do
    let
        working_dir            = unpack $ wrapped_config ^. gotoDir_WCM
        output_dir             = unpack $ wrapped_config ^. outputRel_WCM
        output_dir_bas         = working_dir </> output_dir
        output_cert_place      = working_dir </> output_dir </> unpack (wrapped_config ^. outputCert_WCM)
        privkey_place          = working_dir </> output_dir </> unpack (wrapped_config ^. outputPrivKey_WCM)
        privkey_pkcs8_place    = working_dir </> output_dir </> unpack (wrapped_config ^. outputPrivKeyPKCS8_WCM)
        mousebox_file          = working_dir </> unpack (wrapped_config ^. mouseboxf_WCM)

    -- Try to read the mousebox file and parse it
    maybe_msfbx <- Y.decodeFile  mousebox_file
    case maybe_msfbx of

        Nothing -> do
            B.hPutStr stderr "Could not read <<MOUSEBOXF>> file"

        Just msfbx -> do
            let
                domain_list =  (msfbx ^. domains_Mf)
            persistent_registry <- persistentCARegistryFromFile captured_environment
            (new_persistent_ca_registry, pem_encoded, privkey_pem_encoded, privkey_pkcs8_pem_encoded, _my_key_pair) <- makeLeafCertificate persistent_registry domain_list

            recursivelyCreateDirectory $ pack output_dir_bas

            B.writeFile output_cert_place pem_encoded
            B.writeFile privkey_place privkey_pem_encoded
            B.writeFile privkey_pkcs8_place privkey_pkcs8_pem_encoded
            savePersistentCARegistryToFile captured_environment new_persistent_ca_registry


-- | Entry point used by ShimmerCat
mouseBoxPerformWithDomains ::  WrappedConfigForMouseboxf -> [Tx.Text] ->  IO Bool
mouseBoxPerformWithDomains  wrapped_config  domain_list = do
    captured_environment <- captureEnvironment
    let
        working_dir            = unpack $ wrapped_config ^. gotoDir_WCM
        output_dir             = unpack $ wrapped_config ^. outputRel_WCM
        output_dir_bas         = working_dir </> output_dir
        output_cert_place      = working_dir </> output_dir </> unpack (wrapped_config ^. outputCert_WCM)
        privkey_place          = working_dir </> output_dir </> unpack (wrapped_config ^. outputPrivKey_WCM)
        csr_place              = working_dir </> output_dir </> unpack (wrapped_config ^. csr_WCM)
        privkey_pkcs8_place    = working_dir </> output_dir </> unpack (wrapped_config ^. outputPrivKeyPKCS8_WCM)

        persistent_environment = captured_environment ^. environment_CE
    persistent_registry <- persistentCARegistryFromFile captured_environment

    -- my_key_pair has an internal type
    (new_persistent_ca_registry, pem_encoded, privkey_pem_encoded, privkey_pkcs8_pem_encoded, my_key_pair) <- makeLeafCertificate persistent_registry domain_list
    csr_pem_encoded <- makeCertificateSigningRequest my_key_pair persistent_environment domain_list

    recursivelyCreateDirectory $ pack output_dir_bas

    B.writeFile output_cert_place pem_encoded
    B.writeFile privkey_place privkey_pem_encoded
    B.writeFile privkey_pkcs8_place privkey_pkcs8_pem_encoded
    B.writeFile csr_place $ pemWriteBS csr_pem_encoded
    savePersistentCARegistryToFile captured_environment new_persistent_ca_registry
    return $ captured_environment ^. justCreated_CE


mouseBoxMain :: IO ()
mouseBoxMain = do
    opts <- mouseboxMainOptsParser
    wrapped_config <- OA.execParser opts
    captured_environment <- captureEnvironment
    mouseBoxPerform captured_environment wrapped_config


mouseboxMainOptsParser :: IO (OA.ParserInfo WrappedConfigForMouseboxf)
mouseboxMainOptsParser = do
    current_directory <- getCurrentDirectory
    let
        parser = WrappedConfigForMouseboxf
            <$> OA.option OA.auto (OA.short 'w'
                                   <> OA.long "working-directory"
                                   <> OA.value (pack current_directory)
                                   <> OA.metavar "WORKING_DIRECTORY"
                                   <> OA.help "Directory to consider \"current\""
                                  )
            <*> OA.option  OA.auto (OA.long "mousebox-filename"
                                   <> OA.short   'f'
                                   <> OA.value   "mouseboxf"
                                   <> OA.metavar "MOUSEBOXF"
                                   <> OA.help    "Name of mouseboxf file to find in working dir."
                                  )
            <*> OA.option OA.auto  (OA.long       "output-directory"
                                   <> OA.short   'd'
                                   <> OA.value   "_priv"
                                   <> OA.metavar "OUTPUT_DIR"
                                   <> OA.help    "Output directory"
                                  )
            <*> OA.option OA.auto (OA.long       "cert-filename"
                                   <> OA.short   'c'
                                   <> OA.value   "cert.pem"
                                   <> OA.metavar "CERTIFICATE"
                                   <> OA.help    "Name of the PEM certificate to put at output dir."
                                  )
            <*> OA.option OA.auto (OA.long       "privkey-filename"
                                   <> OA.short   'p'
                                   <> OA.value   "privkey.pem"
                                   <> OA.metavar "PRIVKEY"
                                   <> OA.help    "Name of the PEM private key to put at output dir."
                                  )
            <*> OA.option OA.auto (OA.long       "csr-filename"
                                   <> OA.short   'r'
                                   <> OA.value   "cert.csr"
                                   <> OA.metavar "CSR"
                                   <> OA.help    "Name of the PEM file where to put a certificate signing request (which you shouldn't need, unless...)"
                                  )
            <*> OA.option OA.auto (OA.long       "privkey-pkcs8-filename"
                                   <> OA.short   '8'
                                   <> OA.value   "privkey.unencrypted-pkcs8.pem"
                                   <> OA.metavar "PRIVKEY-PKCS8"
                                   <> OA.help    "Name of the PKCS8 unencrypted private key file"
                                  )
        opts = OA.info (OA.helper <*> parser)
                       (OA.fullDesc
                           <> OA.progDesc "Creates a set of PEM certificates suitable for developing websites over HTTPS"
                           <> OA.header   "Creates a certificate for a set of sites. You can invoke this program without any parameters"
                       )

    return opts
