-- | Backend for generating Agda abstract syntax trees as s-expressions.

module Agda.Interaction.Highlighting.SexpDump.Backend
  ( sexpDumpBackend
  ) where

import Agda.Interaction.Highlighting.SexpDump.Base

import Control.DeepSeq
import Control.Monad.Trans ( MonadIO )
import Control.Monad.Except ( MonadError(throwError) )

import Data.Map (Map)

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

-- | Command-line options for AST generation

data SexpDumpFlags = SexpDumpFlags
  { astFlagEnabled              :: Bool
  , astFlagDir                  :: FilePath
  } deriving (Eq, Generic)

instance NFData SexpDumpFlags

data SexpDumpEnv = SexpDumpEnv
  { astDir :: FilePath }

data SexpDumpModuleEnv = SexpDumpModuleEnv
  { moduleName :: TopLevelModuleName
  }

sexpDumpBackend :: Backend
sexpDumpBackend = Backend sexpDumpBackend'

sexpDumpBackend' :: Backend' SexpDumpFlags SexpDumpEnv () () Definition
sexpDumpBackend' = Backend'
  { backendName           = "Abstract syntax dump as s-expressions"
  , backendVersion        = Nothing
  , options               = initialSexpDumpFlags
  , commandLineFlags      = astFlags
  , isEnabled             = astFlagEnabled
  , preCompile            = preCompileSexpDump
  , preModule             = preModuleSexpDump
  , compileDef            = compileDefSexpDump
  , postModule            = postModuleSexpDump
  , postCompile           = postCompileSexpDump
  -- --only-scope-checking works, but with the caveat that cross-module links
  -- will not have their definition site populated.
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return False
  }

defaultSexpDumpDir :: String
defaultSexpDumpDir = "ast-dump"

initialSexpDumpFlags :: SexpDumpFlags
initialSexpDumpFlags = SexpDumpFlags
  { astFlagEnabled   = False
  , astFlagDir       = defaultSexpDumpDir
  }

astOptsOfFlags :: SexpDumpFlags -> SexpDumpOptions
astOptsOfFlags flags = SexpDumpOptions
  { astOptDir = astFlagDir flags
  }

astFlags :: [OptDescr (Flag SexpDumpFlags)]
astFlags =
    [ Option []     ["ast-dump"] (NoArg astFlag)
                    "generate AST files"
    , Option []     ["ast-dir"] (ReqArg astDirFlag "DIR")
                    ("directory in which AST files are placed (default: " ++
                     defaultSexpDumpDir ++ ")")
    ]

astFlag :: Flag SexpDumpFlags
astFlag o = return $ o { astFlagEnabled = True }

astDirFlag :: FilePath -> Flag SexpDumpFlags
astDirFlag d o = return $ o { astFlagDir = d }

runLogSexpDumpWithMonadDebug :: MonadDebug m => LogSexpDumpT m a -> m a
runLogSexpDumpWithMonadDebug = runLogSexpDumpWith $ reportS "ast-dump" 1

preCompileSexpDump
  :: (MonadIO m, MonadDebug m)
  => SexpDumpFlags
  -> m SexpDumpEnv
preCompileSexpDump flags = runLogSexpDumpWithMonadDebug $ do
  return (SexpDumpEnv $ astFlagDir flags)

preModuleSexpDump
  :: Applicative m
  => SexpDumpEnv
  -> IsMain
  -> TopLevelModuleName
  -> Maybe FilePath
  -> m (Recompile () ())
preModuleSexpDump _env _isMain _modName _ifacePath = pure $ Recompile ()

compileDefSexpDump
  :: Applicative m
  => SexpDumpEnv
  -> ()
  -> IsMain
  -> Definition
  -> m Definition
compileDefSexpDump _env _menv _isMain def = pure def

postModuleSexpDump
  :: (MonadIO m, MonadDebug m, ReadTCState m)
  => SexpDumpEnv
  -> ()
  -> IsMain
  -> TopLevelModuleName
  -> [Definition]
  -> m ()
postModuleSexpDump env menv _isMain modName defs = do
  astSrc <- srcFileOfInterface modName <$> curIF
  runLogSexpDumpWithMonadDebug $ defaultPageGen opts astSrc defs
    where
      opts = SexpDumpOptions (astDir env)

postCompileSexpDump
  :: Applicative m
  => SexpDumpEnv
  -> IsMain
  -> Map TopLevelModuleName ()
  -> m ()
postCompileSexpDump _cenv _isMain _modulesByName = pure ()
