-- | Backend for generating Agda abstract syntax trees as s-expressions.

module Agda.Interaction.Highlighting.Sexp.Backend
  ( sexpBackend
  ) where

import Agda.Interaction.Highlighting.Sexp.Base

import Control.DeepSeq
import Control.Monad.Trans ( MonadIO )
import Control.Monad.Except ( MonadError(throwError) )

import Data.Map (Map)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import GHC.Generics (Generic)

import Agda.Interaction.Options
    ( ArgDescr(ReqArg, NoArg)
    , OptDescr(..)
    , Flag
    )
import Agda.Compiler.Backend (Backend(..), Backend'(..), Recompile(..))
import Agda.Compiler.Common (IsMain(..), curIF)

import Agda.Syntax.TopLevelModuleName (TopLevelModuleName)

import Agda.TypeChecking.Monad
  ( MonadDebug
  , ReadTCState
  , Definition
  , reportS
  )


-- | Command-line options for s-expression generation

data SexpFlags = SexpFlags
  { sexpFlagEnabled              :: Bool
  , sexpFlagDir                  :: FilePath
  } deriving (Eq, Generic)

instance NFData SexpFlags

data SexpEnv = SexpEnv
  { sexpDir :: FilePath }

data SexpModuleEnv = SexpModuleEnv
  { moduleName :: TopLevelModuleName
  }

sexpBackend :: Backend
sexpBackend = Backend sexpBackend'

sexpBackend' :: Backend' SexpFlags SexpEnv () () Definition
sexpBackend' = Backend'
  { backendName           = "S-expression backend options"
  , backendVersion        = Nothing
  , options               = initialSexpFlags
  , commandLineFlags      = sexpFlags
  , isEnabled             = sexpFlagEnabled
  , preCompile            = preCompileSexp
  , preModule             = preModuleSexp
  , compileDef            = compileDefSexp
  , postModule            = postModuleSexp
  , postCompile           = postCompileSexp
  -- --only-scope-checking works, but with the caveat that cross-module links
  -- will not have their definition site populated.
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return False
  }

defaultSexpDir :: String
defaultSexpDir = "sexp"

initialSexpFlags :: SexpFlags
initialSexpFlags = SexpFlags
  { sexpFlagEnabled   = False
  , sexpFlagDir       = defaultSexpDir
  }

sexpOptsOfFlags :: SexpFlags -> SexpOptions
sexpOptsOfFlags flags = SexpOptions
  { sexpOptDir = sexpFlagDir flags
  }

sexpFlags :: [OptDescr (Flag SexpFlags)]
sexpFlags =
    [ Option []     ["sexp"] (NoArg sexpFlag)
                    "generate internal abstract syntax trees as s-expressions"
    , Option []     ["sexp-dir"] (ReqArg sexpDirFlag "DIR")
                    ("directory in which s-expression files are placed (default: " ++
                     defaultSexpDir ++ ")")
    ]

sexpFlag :: Flag SexpFlags
sexpFlag o = return $ o { sexpFlagEnabled = True }

sexpDirFlag :: FilePath -> Flag SexpFlags
sexpDirFlag d o = return $ o { sexpFlagDir = d }

runLogSexpWithMonadDebug :: MonadDebug m => LogSexpT m a -> m a
runLogSexpWithMonadDebug = runLogSexpWith $ reportS "sexp" 1

preCompileSexp
  :: (MonadIO m, MonadDebug m)
  => SexpFlags
  -> m SexpEnv
preCompileSexp flags = runLogSexpWithMonadDebug $ do
  let sexpDir = sexpFlagDir flags
  prepareOutputDirectory sexpDir
  return $ SexpEnv sexpDir

preModuleSexp
  :: Applicative m
  => SexpEnv
  -> IsMain
  -> TopLevelModuleName
  -> Maybe FilePath
  -> m (Recompile () ())
preModuleSexp _env _isMain _modName _ifacePath = pure $ Recompile ()

compileDefSexp
  :: Applicative m
  => SexpEnv
  -> ()
  -> IsMain
  -> Definition
  -> m Definition
compileDefSexp _env _menv _isMain def = pure def

postModuleSexp
  :: (MonadIO m, MonadDebug m, ReadTCState m)
  => SexpEnv
  -> ()
  -> IsMain
  -> TopLevelModuleName
  -> [Definition]
  -> m ()
postModuleSexp env menv _isMain modName defs = do
  sexpSrc <- srcFileOfInterface modName <$> curIF
  runLogSexpWithMonadDebug $ defaultSexpGen opts sexpSrc defs
    where
      opts = SexpOptions (sexpDir env)

postCompileSexp
  :: Applicative m
  => SexpEnv
  -> IsMain
  -> Map TopLevelModuleName ()
  -> m ()
postCompileSexp _cenv _isMain _modulesByName = pure ()
