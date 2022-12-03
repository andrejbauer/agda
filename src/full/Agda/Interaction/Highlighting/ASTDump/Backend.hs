-- | Backend for generating highlighted, hyperlinked HTML from Agda sources.

module Agda.Interaction.Highlighting.ASTDump.Backend
  ( astDumpBackend
  ) where

import Agda.Interaction.Highlighting.ASTDump.Base

import Prelude hiding ((!!), concatMap)

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

-- | Options for AST generation

data ASTDumpFlags = ASTDumpFlags
  { astFlagEnabled              :: Bool
  , astFlagDir                  :: FilePath
  } deriving (Eq, Generic)

instance NFData ASTDumpFlags

data ASTDumpEnv = ASTDumpEnv
  { astDir :: FilePath }

data ASTDumpModuleEnv = ASTDumpModuleEnv
  { moduleName :: TopLevelModuleName
  }

astDumpBackend :: Backend
astDumpBackend = Backend astDumpBackend'

astDumpBackend' :: Backend' ASTDumpFlags ASTDumpEnv () () Definition
astDumpBackend' = Backend'
  { backendName           = "AST Dump"
  , backendVersion        = Nothing
  , options               = initialASTDumpFlags
  , commandLineFlags      = astFlags
  , isEnabled             = astFlagEnabled
  , preCompile            = preCompileASTDump
  , preModule             = preModuleASTDump
  , compileDef            = compileDefASTDump
  , postModule            = postModuleASTDump
  , postCompile           = postCompileASTDump
  -- --only-scope-checking works, but with the caveat that cross-module links
  -- will not have their definition site populated.
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return False
  }

defaultASTDumpDir :: String
defaultASTDumpDir = "ast-dump"

initialASTDumpFlags :: ASTDumpFlags
initialASTDumpFlags = ASTDumpFlags
  { astFlagEnabled   = False
  , astFlagDir       = defaultASTDumpDir
  }

astOptsOfFlags :: ASTDumpFlags -> ASTDumpOptions
astOptsOfFlags flags = ASTDumpOptions
  { astOptDir = astFlagDir flags
  }

astFlags :: [OptDescr (Flag ASTDumpFlags)]
astFlags =
    [ Option []     ["ast-dump"] (NoArg astFlag)
                    "generate AST files"
    , Option []     ["ast-dir"] (ReqArg astDirFlag "DIR")
                    ("directory in which AST files are placed (default: " ++
                     defaultASTDumpDir ++ ")")
    ]

astFlag :: Flag ASTDumpFlags
astFlag o = return $ o { astFlagEnabled = True }

astDirFlag :: FilePath -> Flag ASTDumpFlags
astDirFlag d o = return $ o { astFlagDir = d }

runLogASTDumpWithMonadDebug :: MonadDebug m => LogASTDumpT m a -> m a
runLogASTDumpWithMonadDebug = runLogASTDumpWith $ reportS "ast-dump" 1

preCompileASTDump
  :: (MonadIO m, MonadDebug m)
  => ASTDumpFlags
  -> m ASTDumpEnv
preCompileASTDump flags = runLogASTDumpWithMonadDebug $ do
  return (ASTDumpEnv $ astFlagDir flags)

preModuleASTDump
  :: Applicative m
  => ASTDumpEnv
  -> IsMain
  -> TopLevelModuleName
  -> Maybe FilePath
  -> m (Recompile () ())
preModuleASTDump _env _isMain _modName _ifacePath = pure $ Recompile ()

compileDefASTDump
  :: Applicative m
  => ASTDumpEnv
  -> ()
  -> IsMain
  -> Definition
  -> m Definition
compileDefASTDump _env _menv _isMain def = pure def

postModuleASTDump
  :: (MonadIO m, MonadDebug m, ReadTCState m)
  => ASTDumpEnv
  -> ()
  -> IsMain
  -> TopLevelModuleName
  -> [Definition]
  -> m ()
postModuleASTDump env menv _isMain modName defs = do
  astSrc <- srcFileOfInterface modName <$> curIF
  runLogASTDumpWithMonadDebug $ defaultPageGen opts astSrc defs
    where
      opts = ASTDumpOptions (astDir env)

postCompileASTDump
  :: Applicative m
  => ASTDumpEnv
  -> IsMain
  -> Map TopLevelModuleName ()
  -> m ()
postCompileASTDump _cenv _isMain _modulesByName = pure ()
