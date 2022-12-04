
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
    toSexp (QName (MName lst) nam) = constr "qname" $ (map toSexp lst ++ [toSexp nam])

instance Sexpable Suffix where
    toSexp NoSuffix = Atom "none"
    toSexp (Suffix k) = Integer k

-- instance Sexpable AS.Expr where
--     toSexp (AS.Var x) = constr "bound" [toSexp x]
--     toSexp (AS.Def' n s) = constr "const" [toSexp n, toSexp s]
--     toSexp (AS.Proj _ _) = Atom "<projection>"
--     toSexp (AS.Con _) = Atom "<overloaded-constructor>"
--     toSexp (AS.PatternSyn s) = Atom "<pattern-synonym>"
--     toSexp (AS.Macro q) = constr "macro" [toSexp q]
--     toSexp (AS.Lit _ _) = Atom "<literal>"
--     toSexp (AS.QuestionMark _ _) = Atom "<?>"
--     toSexp (AS.Underscore _) = Atom "<_>"
--     toSexp (AS.Dot _ _) = Atom "<dot>"
--     toSexp (AS.App _ _ _) = Atom "<app>"
--     toSexp (AS.WithApp _ _ _) = Atom "<with-app>"
--     toSexp (AS.Lam _ _ _) = Atom "<lambda>"
--     toSexp (AS.AbsurdLam _ _) = Atom "<absurd-lambda>"
--     toSexp (AS.ExtendedLam _ _ _ _ _) = Atom "<extended-lambda>"
--     toSexp (AS.Pi _ _ _) = Atom "<pi>"
--     toSexp (AS.Generalized _ _) = Atom "<generalized>"
--     toSexp (AS.Fun _ _ _) = Atom "<fun>"
--     toSexp (AS.Let _ _ _) = Atom "<let>"
--     toSexp (AS.Rec _ _) = Atom "<record>"
--     toSexp (AS.RecUpdate _ _ _) = Atom "<record-update>"
--     toSexp (AS.ScopedExpr _ _) = Atom "<scoped-expr>"
--     toSexp (AS.Quote _) = Atom "<quote-qname>"
--     toSexp (AS.QuoteTerm _) = Atom "<quote-term>"
--     toSexp (AS.Unquote _) = Atom "<unquote-qname>"
--     toSexp (AS.DontCare _) = Atom "<dont-care>"

instance Sexpable ArgInfo where
    toSexp _ = Atom "<arginfo>"

instance Sexpable a => Sexpable (Arg a) where
    toSexp (Arg info e) = constr "arg" [toSexp e]

instance Sexpable a => Sexpable (EL.Elim' a) where
    toSexp (EL.Apply a) = toSexp a
    toSexp (EL.Proj _ q) = toSexp q
    toSexp (EL.IApply _ _ _) = Atom "<iapply>"

instance Sexpable a => Sexpable (AI.Abs a) where
    toSexp (AI.Abs n e) = constr "abs" [toSexp n, toSexp e]
    toSexp (NoAbs n e) = constr "noabs" [toSexp n, toSexp e]

instance Sexpable AI.Term where
    toSexp (AI.Var k es) = constr "var" (Integer (toInteger k) : map toSexp es)
    toSexp (AI.Lam args abs) = constr "lam" [toSexp args, toSexp abs]
    toSexp (AI.Lit lit) = Atom "<literal>"
    toSexp (AI.Def qname es) = Atom "<delta-iota-redex>"
    toSexp (AI.Con hd inf es) = Atom "<con>"
    toSexp (AI.Pi dom cod) = constr "pi" [toSexp dom, toSexp cod]
    toSexp (AI.Sort s) = Atom "<sort>"
    toSexp (AI.Level lvl) = Atom "<level>"
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
                                      toSexp (TCM.defArgInfo d),
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

instance Sexpable t => Sexpable (AI.Dom t) where
    toSexp (AI.Dom _ _ _ _ t) = toSexp t

instance Sexpable TopLevelModuleName where
    toSexp (TopLevelModuleName rng (ModuleNameHash id) ps) = constr "hash" [toSexp id]
