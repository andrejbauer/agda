
-- | Function for generating internal abstract syntax trees as s-expressions

module Agda.Interaction.Highlighting.Sexp.Base
  ( SexpOptions(..)
  , srcFileOfInterface
  , defaultSexpGen
  , prepareOutputDirectory
  , MonadLogSexp(logSexp)
  , LogSexpT
  , runLogSexpWith
  ) where

import Agda.Interaction.Highlighting.Sexp.Sexp


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

import Agda.TypeChecking.Monad as TCM

import Agda.Utils.Function
import qualified Agda.Utils.IO.UTF8 as UTF8
import Agda.Utils.Pretty

import Agda.Utils.Impossible

dumpFileExt :: FileType -> String
dumpFileExt ft =
  case ft of
    AgdaFileType -> "agda-sexp"
    MdFileType   -> "md-sexp"
    RstFileType  -> "rst-sexp"
    TexFileType  -> "tex-sexp"
    OrgFileType  -> "org-sexp"

-- | Options for AST dump

data SexpOptions = SexpOptions
  { sexpOptDir                  :: FilePath
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

type SexpLogMessage = String
type SexpLogAction m = SexpLogMessage -> m ()

class MonadLogSexp m where
  logSexp :: SexpLogAction m

type LogSexpT m = ReaderT (SexpLogAction m) m

instance Monad m => MonadLogSexp (LogSexpT m) where
  logSexp message = do
    doLog <- ask
    lift $ doLog message

runLogSexpWith :: Monad m => SexpLogAction m -> LogSexpT m a -> m a
runLogSexpWith = flip runReaderT

renderSourceFile :: TopLevelModuleName -> TCM.Interface -> [TCM.Definition] -> Text
renderSourceFile mdl iface defs =
    toText $ constr "module" (toSexp mdl : map toSexp defs)

defaultSexpGen :: (MonadIO m, MonadLogSexp m) => SexpOptions -> SourceFile -> [TCM.Definition] -> m ()
defaultSexpGen opts (SourceFile moduleName iface) defs = do
  logSexp $ render $ "Generating AST for"  <+> pretty moduleName <+> ((parens (pretty target)) <> ".")
  liftIO $ UTF8.writeTextToFile target sexps
  where
    ext = dumpFileExt (TCM.iFileType iface)
    target = (sexpOptDir opts) </> modToFile moduleName ext
    sexps = renderSourceFile moduleName iface defs

prepareOutputDirectory :: MonadIO m => FilePath -> m ()
prepareOutputDirectory sexpDir = liftIO $ do createDirectoryIfMissing True sexpDir



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

instance Sexpable MetaId where
    toSexp (MetaId u (ModuleNameHash v)) = Cons [toSexp u, toSexp v]

instance Sexpable Literal where
    toSexp (LitNat k) = toSexp k
    toSexp (LitWord64 w) = toSexp w
    toSexp (LitFloat x) = toSexp x
    toSexp (LitString s) = toSexp s
    toSexp (LitChar c) = toSexp [c]
    toSexp (LitQName q) = toSexp q
    toSexp (LitMeta mdl mid) = constr "meta" [toSexp mdl, toSexp mid]

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
    toSexp (AI.MetaV mid es) = constr "meta" (toSexp mid : map toSexp es)
    toSexp (AI.DontCare e) = constr "irrelevant" [toSexp e]
    toSexp (AI.Dummy s es) = constr "internal" (toSexp s : map toSexp es)

instance Sexpable AI.Type where
    toSexp (AI.El srt typ) = constr "type" [toSexp srt, toSexp typ]

instance Sexpable t => Sexpable (AI.Level' t) where
    toSexp (AI.Max k lvls) = constr "max" (toSexp k : map toSexp lvls)

instance Sexpable t => Sexpable (AI.PlusLevel' t) where
    toSexp (AI.Plus k t) = constr "plus" [toSexp k, toSexp t]

instance Sexpable AI.Sort where
    toSexp (AI.Type lvl) = constr "sort-set" [toSexp lvl]
    toSexp (AI.Prop lvl) = constr "sort-prop" [toSexp lvl]
    toSexp (AI.Inf _ k) = constr "sort-setω" [toSexp k]
    toSexp (AI.SSet lvl) = constr "sort-sset" [toSexp lvl]
    toSexp AI.SizeUniv = constr "sort-size" []
    toSexp AI.LockUniv = constr "sort-lock" []
    toSexp AI.IntervalUniv = constr "sort-interval" []
    toSexp (AI.PiSort (AI.Dom _ _ _ _ t) s (Abs n u)) = constr "sort-pi" [toSexp n, toSexp t, toSexp u]
    toSexp (AI.PiSort (AI.Dom _ _ _ _ t) s (NoAbs n u)) = constr "sort-pi" [toSexp n, toSexp t, toSexp u]
    toSexp (AI.FunSort t u) = constr "sort-fun" [toSexp t, toSexp u]
    toSexp (AI.UnivSort srt) = constr "sort-sort" [toSexp srt]
    toSexp (AI.MetaS mid es) = constr "sort-meta" (toSexp mid : map toSexp es)
    toSexp (AI.DefS q es) = constr "sort-def" (toSexp q : map toSexp es)
    toSexp (AI.DummyS s) = constr "sort-dummy" [String s]

instance Sexpable TCM.Definition where
    toSexp d = constr "definition" [ toSexp (TCM.defName d),
                                     toSexp (TCM.defType d),
                                     toSexp (TCM.theDef d)
                                    ]
instance Sexpable TCM.Defn where
    toSexp (TCM.Axiom {}) = constr "axiom" []
    toSexp (TCM.DataOrRecSig {}) = constr "data-or-record" []
    toSexp (TCM.GeneralizableVar) = constr "generalizable-var" []
    toSexp (TCM.AbstractDefn d) = constr "abstract" [toSexp d]
    toSexp (TCM.Function {funClauses=cls}) = constr "function" (map toSexp cls)
    toSexp (TCM.Datatype {dataCons=cns, dataSort=srt}) = constr "data" (toSexp srt : map toSexp cns)
    toSexp (TCM.Record {recFields=fds, recConHead=AI.ConHead{conName=q}}) = constr "record" (toSexp q : map toSexp fds)
    toSexp (TCM.Constructor {conData=d}) = constr "constructor" [toSexp d]
    toSexp (TCM.Primitive {primName=s, primClauses=cls}) = constr "primitive" (toSexp s : map toSexp cls)
    toSexp (TCM.PrimitiveSort {primSortName=q, primSortSort=s}) = constr "sort" [toSexp q, toSexp s]

instance Sexpable AI.Clause where
    toSexp (AI.Clause {clauseTel=tel, clauseBody=Nothing}) = constr "clause" [toSexp tel]
    toSexp (AI.Clause {clauseTel=tel, clauseBody=Just bdy}) = constr "clause" [toSexp tel, toSexp bdy]

instance Sexpable AI.Telescope where
    toSexp tel = Cons $ process tel
        where process AI.EmptyTel = []
              process (AI.ExtendTel t (Abs n tel)) = (constr "arg" [toSexp n, toSexp t]) : process tel
              process (AI.ExtendTel t (NoAbs n tel)) = (constr "arg" [toSexp n, toSexp t]) : process tel

instance Sexpable TopLevelModuleName where
    toSexp (TopLevelModuleName rng (ModuleNameHash id) ps) = Cons $ map (Atom . T.fromStrict) $ toList ps
