
-- | Function for generating highlighted AST

module Agda.Interaction.Highlighting.SexpDump.Base
  ( SexpDumpOptions(..)
  , srcFileOfInterface
  , defaultPageGen
  , MonadLogSexpDump(logSexpDump)
  , LogSexpDumpT
  , runLogSexpDumpWith
  ) where

import Agda.Interaction.Highlighting.SexpDump.Sexp


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

import Agda.Syntax.TopLevelModuleName

import Agda.Syntax.Literal (Literal(..))

import Agda.Syntax.Common
import Agda.Syntax.Abstract as AS
import Agda.Syntax.Internal as AI
import Agda.Syntax.Internal.Elim as EL

import Agda.Syntax.Abstract.Name
  ( QName(..),
    Name(..),
    ModuleName(..),
    Suffix(..)
  )

import qualified Agda.TypeChecking.Monad as TCM
  ( Interface(..)
  , Definition(..)
  , Defn(..)
  , FunctionData(..)
  , funClauses
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

data SexpDumpOptions = SexpDumpOptions
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

type SexpDumpLogMessage = String
type SexpDumpLogAction m = SexpDumpLogMessage -> m ()

class MonadLogSexpDump m where
  logSexpDump :: SexpDumpLogAction m

type LogSexpDumpT m = ReaderT (SexpDumpLogAction m) m

instance Monad m => MonadLogSexpDump (LogSexpDumpT m) where
  logSexpDump message = do
    doLog <- ask
    lift $ doLog message

runLogSexpDumpWith :: Monad m => SexpDumpLogAction m -> LogSexpDumpT m a -> m a
runLogSexpDumpWith = flip runReaderT

renderSourceFile :: TopLevelModuleName -> TCM.Interface -> [TCM.Definition] -> Text
renderSourceFile moduleName iface defs =
    toText $ constr "module" (toSexp moduleName : map toSexp defs)

defaultPageGen :: (MonadIO m, MonadLogSexpDump m) => SexpDumpOptions -> SourceFile -> [TCM.Definition] -> m ()
defaultPageGen opts (SourceFile moduleName iface) defs = do
  logSexpDump $ render $ "Generating AST for"  <+> pretty moduleName <+> ((parens (pretty target)) <> ".")
  liftIO $ UTF8.writeTextToFile target sexps
  where
    ext = dumpFileExt (TCM.iFileType iface)
    target = (astOptDir opts) </> modToFile moduleName ext
    sexps = renderSourceFile moduleName iface defs

-- | Converts module names to the corresponding AST file names.

modToFile :: TopLevelModuleName -> String -> FilePath
modToFile m ext = Network.URI.Encode.encode $ render (pretty m) <.> ext

-- | Conversion to S-expressions

instance Sexpable Name where
    toSexp n = Atom (T.pack $ prettyShow $ nameConcrete n)

instance Sexpable ModuleName where
    toSexp (MName lst) = constr "mname" $ map toSexp lst

instance Sexpable QName where
    toSexp (QName (MName lst) nam) = constr "name" $ (map toSexp lst ++ [toSexp nam])

instance Sexpable Suffix where
    toSexp NoSuffix = Atom "none"
    toSexp (Suffix k) = Integer k

instance Sexpable a => Sexpable (EL.Elim' a) where
    toSexp (EL.Apply (Arg _ e)) = constr "arg" [toSexp e]
    toSexp (EL.Proj _ q) = constr "proj" [toSexp q]
    toSexp (EL.IApply x y r) = constr "interval-arg" [toSexp x, toSexp y, toSexp r]

-- instance Sexpable a => Sexpable (AI.Abs a) where
--     toSexp (AI.Abs n e) = constr "abs" [toSexp n, toSexp e]
--     toSexp (NoAbs n e) = constr "noabs" [toSexp n, toSexp e]

instance Sexpable t => Sexpable (AI.Dom t) where
    toSexp (AI.Dom _ _ _ _ t) = toSexp t

instance Sexpable Literal where
    toSexp (LitNat k) = toSexp k
    toSexp (LitWord64 w) = toSexp w
    toSexp (LitFloat x) = toSexp x
    toSexp (LitString s) = toSexp $ show s
    toSexp (LitChar c) = toSexp [c]
    toSexp (LitQName q) = toSexp q
    toSexp (LitMeta mdl (MetaId u (ModuleNameHash v))) = constr "meta" [toSexp mdl, toSexp u, toSexp v]

instance Sexpable AI.Term where
    toSexp (AI.Var k es) = constr "var" (Integer (toInteger k) : map toSexp es)
    toSexp (AI.Lam _ (AI.Abs x e)) = constr "lambda" [String x, toSexp e]
    toSexp (AI.Lam _ (AI.NoAbs x e)) = constr "lambda" [String x, toSexp e]
    toSexp (AI.Lit lit) = constr "literal" [toSexp lit]
    toSexp (AI.Def q es) = constr "apply" [toSexp q, Cons $ map toSexp es]
    toSexp (AI.Con (AI.ConHead q _ _ _) _ es) = constr "constr" (toSexp q : map toSexp es)
    toSexp (AI.Pi (AI.Dom _ _ _ _ t) (AI.Abs x e)) = constr "pi" [String x, toSexp t, toSexp e]
    toSexp (AI.Pi (AI.Dom _ _ _ _ t) (AI.NoAbs x e)) = constr "pi" [String x, toSexp t, toSexp e]
    toSexp (AI.Sort s) = constr "sort" [toSexp s]
    toSexp (AI.Level lvl) = constr "level" [toSexp lvl]
    toSexp (AI.MetaV mid es) = Atom "<meta>"
    toSexp (AI.DontCare e) = Atom "<dont-care>"
    toSexp (AI.Dummy s es) = Atom "<dummy>"


instance Sexpable AI.Type where
    toSexp (AI.El srt typ) = constr "type" [toSexp srt, toSexp typ]

instance Sexpable t => Sexpable (AI.Level' t) where
    toSexp (AI.Max k lvls) = constr "max" (toSexp k : map toSexp lvls)

instance Sexpable t => Sexpable (AI.PlusLevel' t) where
    toSexp (AI.Plus k t) = constr "plus" [toSexp k, toSexp t]

instance Sexpable t => Sexpable (AI.Sort' t) where
    toSexp (AI.Type lvl) = constr "set" [toSexp lvl]
    toSexp (AI.Prop lvl) = constr "prop" [toSexp lvl]
    toSexp (AI.Inf _ k) = constr "setω" [toSexp k]
    toSexp (AI.SSet lvl) = constr "sset" [toSexp lvl]
    toSexp AI.SizeUniv = Atom ":sizeuniv"
    toSexp AI.LockUniv = Atom ":lockuniv"
    toSexp AI.IntervalUniv = Atom ":interval"
    toSexp (AI.PiSort dom srt cod) = Atom "<pi-sort>"
    toSexp (AI.FunSort dom cod) = constr "funsort" [toSexp dom, toSexp cod]
    toSexp (AI.UnivSort srt) = constr "univsort" [toSexp srt]
    toSexp (AI.MetaS id es) = Atom "<metasort>"
    toSexp (AI.DefS q es) = constr "defsort" (toSexp q : map toSexp es)
    toSexp (AI.DummyS s) = constr "dummysort" [String s]

instance Sexpable TCM.Definition where
    toSexp d = constr "definition" [ toSexp (TCM.defName d),
                                     toSexp (TCM.defType d),
                                     toSexp (TCM.theDef d)
                                    ]

instance Sexpable TCM.Defn where
    toSexp (TCM.AxiomDefn ax) = Atom "<axiom>"
    toSexp (TCM.DataOrRecSigDefn dat) = Atom "<data-or-rec>"
    toSexp (TCM.GeneralizableVar) = Atom "generalizable-var"
    toSexp (TCM.AbstractDefn d) = constr "abstract" [toSexp d]
    toSexp (d@(TCM.FunctionDefn _)) = constr "function" (map toSexp $ TCM.funClauses d)
    toSexp (TCM.DatatypeDefn dat) = Atom "<data>"
    toSexp (TCM.RecordDefn dat) = Atom "<record>"
    toSexp (TCM.ConstructorDefn dat) = Atom "<constructor>"
    toSexp (TCM.PrimitiveDefn dat) = Atom "<primitive>"
    toSexp (TCM.PrimitiveSortDefn dat) = Atom "<primitive-sort>"

instance Sexpable AI.Clause where
    toSexp cls = constr "clause" [toSexp (clauseTel cls), toSexp (clauseBody cls)]

instance Sexpable (Maybe AI.Term) where
    toSexp Nothing = constr "absurd" []
    toSexp (Just e) = toSexp e

instance Sexpable AI.Telescope where
    toSexp tel = Cons $ process tel
        where process AI.EmptyTel = []
              process (AI.ExtendTel t (Abs n tel)) = (constr "arg" [toSexp n, toSexp t]) : process tel
              process (AI.ExtendTel t (NoAbs n tel)) = [constr "internal-error" []]

instance Sexpable TopLevelModuleName where
    toSexp (TopLevelModuleName rng (ModuleNameHash id) ps) = constr "hash" [toSexp id]
