
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

-- | Options for HTML generation

data ASTDumpFlags = ASTDumpFlags
  { astFlagEnabled              :: Bool
  , astFlagDir                  :: FilePath
  } deriving (Eq, Generic)

instance NFData ASTDumpFlags

data HtmlCompileEnv = HtmlCompileEnv
  { htmlCompileEnvOpts :: ASTDumpOptions
  }

data HtmlModuleEnv = HtmlModuleEnv
  { htmlModEnvCompileEnv :: HtmlCompileEnv
  , htmlModEnvName       :: TopLevelModuleName
  }

data HtmlModule = HtmlModule

astDumpBackend :: Backend
astDumpBackend = Backend astDumpBackend'

astDumpBackend' :: Backend' ASTDumpFlags HtmlCompileEnv HtmlModuleEnv HtmlModule Definition
astDumpBackend' = Backend'
  { backendName           = "AST Dump"
  , backendVersion        = Nothing
  , options               = initialASTDumpFlags
  , commandLineFlags      = astFlags
  , isEnabled             = astFlagEnabled
  , preCompile            = preCompileHtml
  , preModule             = preModuleHtml
  , compileDef            = compileDefHtml
  , postModule            = postModuleHtml
  , postCompile           = postCompileHtml
  -- --only-scope-checking works, but with the caveat that cross-module links
  -- will not have their definition site populated.
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return False
  }

initialASTDumpFlags :: ASTDumpFlags
initialASTDumpFlags = ASTDumpFlags
  { astFlagEnabled   = False
  , astFlagDir       = defaultASTDumpDir
  }

htmlOptsOfFlags :: ASTDumpFlags -> ASTDumpOptions
htmlOptsOfFlags flags = ASTDumpOptions
  { htmlOptDir = astFlagDir flags
  }

-- | The default output directory for HTML.

defaultASTDumpDir :: FilePath
defaultASTDumpDir = "ast-dump"

astFlags :: [OptDescr (Flag ASTDumpFlags)]
astFlags =
    [ Option []     ["ast-dump"] (NoArg astFlag)
                    "generate AST files"
    , Option []     ["ast-dir"] (ReqArg htmlDirFlag "DIR")
                    ("directory in which AST files are placed (default: " ++
                     defaultASTDumpDir ++ ")")
    ]

astFlag :: Flag ASTDumpFlags
astFlag o = return $ o { astFlagEnabled = True }

htmlDirFlag :: FilePath -> Flag ASTDumpFlags
htmlDirFlag d o = return $ o { astFlagDir = d }

runLogHtmlWithMonadDebug :: MonadDebug m => LogHtmlT m a -> m a
runLogHtmlWithMonadDebug = runLogHtmlWith $ reportS "html" 1

preCompileHtml
  :: (MonadIO m, MonadDebug m)
  => ASTDumpFlags
  -> m HtmlCompileEnv
preCompileHtml flags = runLogHtmlWithMonadDebug $ do
  logHtml $ unlines
    [ "Warning: HTML is currently generated for ALL files which can be"
    , "reached from the given module, including library files."
    ]
  let opts = htmlOptsOfFlags flags
  prepareCommonDestinationAssets opts
  return $ HtmlCompileEnv opts

preModuleHtml
  :: Applicative m
  => HtmlCompileEnv
  -> IsMain
  -> TopLevelModuleName
  -> Maybe FilePath
  -> m (Recompile HtmlModuleEnv HtmlModule)
preModuleHtml cenv _isMain modName _ifacePath = pure $ Recompile (HtmlModuleEnv cenv modName)

compileDefHtml
  :: Applicative m
  => HtmlCompileEnv
  -> HtmlModuleEnv
  -> IsMain
  -> Definition
  -> m Definition
compileDefHtml _env _menv _isMain def = pure def

postModuleHtml
  :: (MonadIO m, MonadDebug m, ReadTCState m)
  => HtmlCompileEnv
  -> HtmlModuleEnv
  -> IsMain
  -> TopLevelModuleName
  -> [Definition]
  -> m HtmlModule
postModuleHtml _env menv _isMain _modName defs = do
  let generatePage = defaultPageGen . htmlCompileEnvOpts . htmlModEnvCompileEnv $ menv
  htmlSrc <- srcFileOfInterface (htmlModEnvName menv) <$> curIF
  runLogHtmlWithMonadDebug $ generatePage htmlSrc defs
  return HtmlModule

postCompileHtml
  :: Applicative m
  => HtmlCompileEnv
  -> IsMain
  -> Map TopLevelModuleName HtmlModule
  -> m ()
postCompileHtml _cenv _isMain _modulesByName = pure ()
