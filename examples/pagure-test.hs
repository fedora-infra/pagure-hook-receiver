{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Text as T
import Shelly
--import Web.Pagure.HookReceiver.Fedmsg
import Web.Pagure.HookReceiver.Post
import Web.Pagure.HookReceiver.StandardHooks (githubMirror, cloneRepo)

-- No, this is not a real key. ;)
-- However, this is how you can define deploy keys. Of course, each project
-- can have their own.
key :: String
key =
  "-----BEGIN EC PRIVATE KEY-----\
  \MHcCAQEEIAQfTmIbj/skzqawp36VZ+mxDfOrhYAee/fle3j2yPSzoAoGCCqGSM49\
  \AwEHoUQDQgAEUXD06CCVGvJF4f321v4H7i8n42HIc7TwoCushS8OIRHDkknePCpX\
  \mFJwWGgySziWJeAeVjyaCLOGxhRWT2R3Bw==\
  \-----END EC PRIVATE KEY-----"

projectMapping :: M.Map String [String -> IO ()]
projectMapping = M.fromList
                 [ ("pagure-haskell", [githubMirror "relrod/test-ignore" key, updatePagureHaskellDocs])
                 , ("pagure-hook-receiver", [githubMirror "fedora-infra/pagure-hook-receiver" key])
                 ]

updatePagureHaskellDocs :: MonadIO m => String -> m ()
updatePagureHaskellDocs name = do
  cloneRepo name
  shelly $ chdir (fromText (T.pack name)) (run_ "/bin/bash" ["deploydocs.sh"])

main :: IO ()
main = runPagureListener 3000 projectMapping
