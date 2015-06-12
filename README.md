# pagure-hook-receiver

Right now, Pagure's `POST` hook isn't very useful -- it doesn't actually give us
any data with which to act upon. However, the hook *does* fire, and so we can
use this to do things when a push occurs.

In an ideal world, we would listen for fedmsg events and act on those, but they
don't seem to currently be sent for pushes. So for now we listen for
project-specific `POST` receive hook URLs and act on those.

This lets us do things like mirroring to GitHub every time a push occurs.

This project is written in a library-like way. You write a hook receiver by
importing the library, defining a mapping between projects and functions that
run when a `POST` is received, and calling a function which runs the listener.

For example:

```haskell
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
-- can have its own.
key :: String
key = unlines
  [ "-----BEGIN EC PRIVATE KEY-----"
  , "MHcCAQEEIAQfTmIbj/skzqawp36VZ+mxDfOrhYAee/fle3j2yPSzoAoGCCqGSM49"
  , "AwEHoUQDQgAEUXD06CCVGvJF4f321v4H7i8n42HIc7TwoCushS8OIRHDkknePCpX"
  , "mFJwWGgySziWJeAeVjyaCLOGxhRWT2R3Bw=="
  , "-----END EC PRIVATE KEY-----"
  ]

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
```

This will (should) listen for `POST` hooks pointed at the `/pagure-haskell` and
`/pagure-hook-receiver` endpoints and fire off the list of handler functions
associated with the respective projects in `projectMapping`. We define a custom
listener to handle the deployment of project documentation.

The `StandardHooks` module contains hooks and utility functions that are useful
in the general case. For example `githubMirror :: String -> IO ()` is a hook
that takes a GitHub repo (`username/reponame`) and a private (deployment) key
and mirrors the code (and branches and tags) from the Pagure repo to it.

`cloneRepo :: String -> IO ()` is a utility function that will clone the repo if
necessary. Many hooks will likely call this, especially if they wish to make use
of files within the git repo, or the git repo itself. Internally, after all
hooks are called, if there is a clone of the repository, we delete it
automatically.

## License

(c) Red Hat, Inc. BSD-2.
