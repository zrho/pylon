{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.Pylon.Codegen.JS (mkProgram) where

-------------------------------------------------------------------------------

import Prelude                hiding (mapM_)
import Language.Pylon.Codegen.Monad
import Language.Pylon.STG.AST
import Data.Foldable                 (forM_, mapM_)
import Data.List                     (intercalate)
import Text.InterpolatedString.Perl6 (qq)

-------------------------------------------------------------------------------

endl :: String
endl = "\n"

type Name = String

-------------------------------------------------------------------------------
-- Programs and Expressions
-------------------------------------------------------------------------------

-- | Code for a STG program.
mkProgram :: Program -> Codegen ()
mkProgram p = do
  [qq|var stg = require('./stg');{endl}|]
  mapM_ mkBind p

-- | Code for a STG expression; binds the value of the expression to a fresh
-- | name and returns that name.
mkExp :: Exp -> Codegen Name
mkExp (EApp v as)    = mkEApp v as
mkExp (EPrim po as)  = mkEPrim po as
mkExp (ECase as d e) = mkECase as d e
mkExp (EAtom a)      = mkEAtom a
mkExp (ELet bs e)    = mkELet bs e

-------------------------------------------------------------------------------
-- Case Expressions
-------------------------------------------------------------------------------

-- | Code for a case expression:
-- |
-- | For algebratic alternatives, the scrutinee is forced first; then the
-- | index of its info structure is switched on. For primitive alternatives,
-- | the scrutinee is switched on directly.
-- |
-- | When no alternative but the default one is provided, the switch will be
-- | omitted, since it is not legal javascript to define a switch with only
-- | a default alternative.
mkECase :: Alts -> Default -> Exp -> Codegen Name
mkECase (AAlts []) (Default dv de) scr = do
  scrE <- mkExp scr
  [qq|stg.force({scrE});{endl}|]
  forM_ dv $ \v -> [qq|var {v} = {scrE};{endl}|]
  mkExp de
mkECase (AAlts as) def scr = withTemp $ \result -> do
  scrE <- mkExp scr
  [qq|stg.force({scrE});{endl}|]
  [qq|var {result};{endl}|]
  [qq|switch ({scrE}.index) \{{endl}|]
  indent $ do
    -- alternatives
    forM_ as $ \(AAlt c vs e) -> do
      [qq|case {c}:{endl}|]
      indent $ do
        -- bind cons fields
        forM_ (zip [0..] vs) $
          \(i, v) -> [qq|var {v} = {scrE}.data[{i}];{endl}|]
        -- make nested expression
        bindTo result $ mkExp e
        [qq|break;{endl}|]
    -- default alternative
    mkDefault scrE result def
  [qq|}{endl}|]
mkECase (PAlts []) (Default dv de) scr = do
  scrE <- mkExp scr
  forM_ dv $ \v -> [qq|var {v} = {scrE};{endl}|]
  mkExp de
mkECase (PAlts as) def scr = withTemp $ \result -> do
  scrE <- mkExp scr
  [qq|var {result};{endl}|]
  [qq|switch ({scrE}) \{{endl}|]
  indent $ do
    -- alternatives
    forM_ as $ \(PAlt l e) -> do
      [qq|case {mkLit l}:{endl}|]
      indent $ do
        bindTo result $ mkExp e
        [qq|break;{endl}|]
    -- default alternative
    mkDefault scrE result def
  [qq|}{endl}|]

-- | Code for the default case of a case expression.
-- | Shared between algebraic and primitive alternatives.
mkDefault :: Name -> Name -> Default -> Codegen Name
mkDefault scr result (Default dv de) = do
  [qq|default:{endl}|]
  indent $ do
    forM_ dv $ \v -> [qq|var {v} = {scr};{endl}|]
    bindTo result $ mkExp de
    [qq|break;{endl}|]
  return result

-------------------------------------------------------------------------------
-- Function and Primitive Application
-------------------------------------------------------------------------------

-- | Code for function application: the function and the arguments are passed
-- | to an apply function which handles the apply semantics.
mkEApp :: Var -> [Atom] -> Codegen Name
mkEApp v as = withTemp $ \result -> do
  let args = intercalate ", " $ v : fmap mkAtom as
  [qq|var $result = stg.apply($args);{endl}|]

-- | Code for primitive application: The arguments are intercalated by the
-- | primitive operator.
mkEPrim :: PrimOp -> [Exp] -> Codegen Name
mkEPrim (PForeign f _ _) as = withTemp $ \result -> do
  vs <- mapM mkExp as
  let args = intercalate ", " vs
  [qq|var $result = {f}({args});{endl}|]
mkEPrim po as = withTemp $ \result -> do
  vs <- mapM mkExp as
  let ops = intercalate (mkPrim po) vs
  [qq|var {result} = {ops};{endl}|]

-- | Primitive operators.
mkPrim :: PrimOp -> String
mkPrim (PPlus _)  = "+"
mkPrim (PMult _)  = "*"
mkPrim (PDiv _)   = "/"
mkPrim (PMinus _) = "-"
mkPrim (PEq _)    = "==="
mkPrim (PLt _)    = "<"
mkPrim (PLte _)   = "<="
mkPrim (PGt _)    = ">"
mkPrim (PGte _)   = ">="

----------------------------------------------------------------------------
-- Let Expressions and Bindings
-------------------------------------------------------------------------------

-- | Code for let expressions: First bind, then execute the inner code.
mkELet :: [Bind] -> Exp -> Codegen Name
mkELet bs e = mapM_ mkBind bs >> mkExp e

-- | Code for bindings: Binds the heap object generated by mkObject to the
-- | target variable.
mkBind :: Bind -> Codegen ()
mkBind (Bind v o) = do
  [qq|var {v} = \{{endl}|]
  indent $ mkObject o
  [qq|};{endl}|]

-- | Code for a heap object. The info table is merged with the data here.
mkObject :: Object -> Codegen ()
mkObject (Fun vs e) = do
  [qq|type : 0,{endl}|]
  [qq|arity: {length vs},{endl}|]
  [qq|code : function({intercalate "," vs}) \{{endl}|]
  withRet $ mkExp e
  [qq|}|]
mkObject (Pap v as) = do
  [qq|type: 1,{endl}|]
  [qq|code: {v},{endl}|]
  [qq|data: [{mkAtoms as}]{endl}|]
mkObject (Con c as) = do
  [qq|type : 2,{endl}|]
  [qq|index: {c},{endl}|]
  [qq|data : [{mkAtoms as}],{endl}|]
  [qq|code : function() \{}{endl}|]
mkObject (Thunk e) = do
  [qq|type : 3,{endl}|]
  [qq|code : stg.thunk(function() \{{endl}|]
  withRet $ mkExp e
  [qq|})|]
mkObject Error = do
  [qq|type : 4,{endl}|]
  [qq|code : function() \{ console.log("error"); }|]

-------------------------------------------------------------------------------
-- Atoms and Literals
-------------------------------------------------------------------------------

-- | Code for an atomic expression: Bind the value of the atom to a fresh var.
mkEAtom :: Atom -> Codegen Name
mkEAtom a = withTemp $ \r -> [qq|var {r} = {mkAtom a};{endl}|]

-- | Intercalates multiple atoms.
mkAtoms :: [Atom] -> String
mkAtoms = intercalate "," . fmap mkAtom

-- | Code for an atom.
mkAtom :: Atom -> String
mkAtom (AVar v) = v
mkAtom (ALit l) = mkLit l

-- | Code for an literal.
mkLit :: Lit -> String
mkLit (LInt i) = show i

-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

-- | Generates the code and returns the result.
withRet :: Codegen Name -> Codegen Name
withRet cg = indent $ do
  ret <- cg
  [qq|return {ret};{endl}|]
  return ret

-- | Introduces a fresh variable and supplies it to a code generator; then
-- | use that fresh variable as the result.
withTemp :: (Name -> Codegen a) -> Codegen Name
withTemp cg = do
  result <- freshName
  cg result
  return result

-- | Bind the result of a code generator to a variable-
bindTo :: Name -> Codegen Name -> Codegen Name
bindTo result cg = do
  result' <- cg
  [qq|{result} = {result'};{endl}|]
  return result
