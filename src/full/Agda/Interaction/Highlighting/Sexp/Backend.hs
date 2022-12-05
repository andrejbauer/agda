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


-- | Command-line options for AST generation

data SexpFlags = SexpFlags
  { astFlagEnabled              :: Bool
  , astFlagDir                  :: FilePath
  } deriving (Eq, Generic)

instance NFData SexpFlags

data SexpEnv = SexpEnv
  { astDir :: FilePath }

data SexpModuleEnv = SexpModuleEnv
  { moduleName :: TopLevelModuleName
  }

sexpBackend :: Backend
sexpBackend = Backend sexpBackend'

sexpBackend' :: Backend' SexpFlags SexpEnv () () Definition
sexpBackend' = Backend'
  { backendName           = "Abstract syntax dump as s-expressions"
  , backendVersion        = Nothing
  , options               = initialSexpFlags
  , commandLineFlags      = astFlags
  , isEnabled             = astFlagEnabled
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
defaultSexpDir = "ast-dump"

initialSexpFlags :: SexpFlags
initialSexpFlags = SexpFlags
  { astFlagEnabled   = False
  , astFlagDir       = defaultSexpDir
  }

astOptsOfFlags :: SexpFlags -> SexpOptions
astOptsOfFlags flags = SexpOptions
  { astOptDir = astFlagDir flags
  }

astFlags :: [OptDescr (Flag SexpFlags)]
astFlags =
    [ Option []     ["ast-dump"] (NoArg astFlag)
                    "generate AST files"
    , Option []     ["ast-dir"] (ReqArg astDirFlag "DIR")
                    ("directory in which AST files are placed (default: " ++
                     defaultSexpDir ++ ")")
    ]

astFlag :: Flag SexpFlags
astFlag o = return $ o { astFlagEnabled = True }

astDirFlag :: FilePath -> Flag SexpFlags
astDirFlag d o = return $ o { astFlagDir = d }

runLogSexpWithMonadDebug :: MonadDebug m => LogSexpT m a -> m a
runLogSexpWithMonadDebug = runLogSexpWith $ reportS "ast-dump" 1

preCompileSexp
  :: (MonadIO m, MonadDebug m)
  => SexpFlags
  -> m SexpEnv
preCompileSexp flags = runLogSexpWithMonadDebug $ do
  return (SexpEnv $ astFlagDir flags)

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
  astSrc <- srcFileOfInterface modName <$> curIF
  runLogSexpWithMonadDebug $ defaultPageGen opts astSrc defs
    where
      opts = SexpOptions (astDir env)

postCompileSexp
  :: Applicative m
  => SexpEnv
  -> IsMain
  -> Map TopLevelModuleName ()
  -> m ()
postCompileSexp _cenv _isMain _modulesByName = pure ()
