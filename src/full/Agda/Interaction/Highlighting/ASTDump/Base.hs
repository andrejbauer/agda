
-- | Function for generating highlighted AST

module Agda.Interaction.Highlighting.ASTDump.Base
  ( ASTDumpOptions(..)
  , srcFileOfInterface
  , defaultPageGen
  , MonadLogASTDump(logASTDump)
  , LogASTDumpT
  , runLogASTDumpWith
  ) where

import Prelude hiding ((!!), concatMap)

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans ( MonadIO(..), lift )
import Control.Monad.Trans.Reader ( ReaderT(runReaderT), ask )

import Data.Function ( on )
import Data.Foldable (toList, concatMap)
import Data.Maybe
import qualified Data.IntMap as IntMap
import qualified Data.List   as List
import Data.List.Split (splitWhen, chunksOf)
import Data.Text.Lazy (Text)
import Data.String (fromString)
import qualified Data.Text.Lazy as T

import GHC.Generics (Generic)

import qualified Network.URI.Encode

import System.FilePath
import System.Directory

import Paths_Agda

import Agda.Syntax.Common
import Agda.Syntax.TopLevelModuleName

import qualified Agda.TypeChecking.Monad as TCM
  ( Interface(..)
  , Definition
  )

import Agda.Utils.Function
import qualified Agda.Utils.IO.UTF8 as UTF8
import Agda.Utils.Pretty

import Agda.Utils.Impossible

dumpFileExt :: FileType -> String
dumpFileExt ft =
  case ft of
    AgdaFileType -> "agda-ast"
    MdFileType   -> "md-ast"
    RstFileType  -> "rst-ast"
    TexFileType  -> "tex-ast"
    OrgFileType  -> "org-ast"

-- | Options for AST dump

data ASTDumpOptions = ASTDumpOptions
  { astOptDir                  :: FilePath
  } deriving Eq

-- | Internal type bundling the information related to a module source file

data SourceFile = SourceFile
  { _srcFileModuleName :: TopLevelModuleName
  , _srcInterface :: TCM.Interface
  }

-- | Bundle up the highlighting info for a source file

srcFileOfInterface ::
  TopLevelModuleName -> TCM.Interface -> SourceFile
srcFileOfInterface m i = SourceFile m i

-- | Logging during AST generation

type ASTDumpLogMessage = String
type ASTDumpLogAction m = ASTDumpLogMessage -> m ()

class MonadLogASTDump m where
  logASTDump :: ASTDumpLogAction m

type LogASTDumpT m = ReaderT (ASTDumpLogAction m) m

instance Monad m => MonadLogASTDump (LogASTDumpT m) where
  logASTDump message = do
    doLog <- ask
    lift $ doLog message

runLogASTDumpWith :: Monad m => ASTDumpLogAction m -> LogASTDumpT m a -> m a
runLogASTDumpWith = flip runReaderT

renderSourceFile :: TopLevelModuleName -> TCM.Interface -> [TCM.Definition] -> Text
renderSourceFile _moduleName iface defs =
    fromString $ (show defs)

defaultPageGen :: (MonadIO m, MonadLogASTDump m) => ASTDumpOptions -> SourceFile -> [TCM.Definition] -> m ()
defaultPageGen opts (SourceFile moduleName iface) defs = do
  logASTDump $ render $ "Generating AST for"  <+> pretty moduleName <+> ((parens (pretty target)) <> ".")
  liftIO $ UTF8.writeTextToFile target sexps
  where
    ext = dumpFileExt (TCM.iFileType iface)
    target = (astOptDir opts) </> modToFile moduleName ext
    sexps = renderSourceFile moduleName iface defs

-- | Converts module names to the corresponding AST file names.

modToFile :: TopLevelModuleName -> String -> FilePath
modToFile m ext = Network.URI.Encode.encode $ render (pretty m) <.> ext
