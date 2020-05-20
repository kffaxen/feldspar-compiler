module Feldspar.Compiler.Imperative.DeadFunElim (deadFunElim) where

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Backend.C.Options(Options(..), Platform(..))

import qualified Data.Set as S

type NameSet = S.Set String

deadFunElim :: Options -> Module () -> Module ()
deadFunElim opts (Module ents) = Module $ snd $ foldr (dfeEnt opts) (S.empty, []) ents

dfeEnt :: Options -> Entity () -> (NameSet, [Entity ()]) -> (NameSet, [Entity ()])
dfeEnt opts ent@Proc{procName = name, procBody = body} (names, ents)
  | not $ S.member name names || null ents = (names, ents)
  | otherwise = (maybeNames blockNames body `S.union` S.delete name names, ent : ents)
dfeEnt opts ent (names, ents) = (names, ent : ents)

blockNames :: Block () -> NameSet
blockNames (Block ds p) = S.unions $ progNames p : map declNames ds
  where declNames d = maybeNames exprNames $ initVal d

progNames :: Program () -> NameSet
progNames = go
  where go (Assign lhs rhs)      = goE lhs `S.union` goE rhs
        go (ProcedureCall p aps) = S.unions $ S.singleton p : map goA aps
        go (Sequence ps)         = S.unions $ map go ps
        go (Switch e alts)       = S.unions $ goE e : map (goB . snd) alts
        go (SeqLoop c cc b)      = goE c `S.union` goB cc `S.union` goB b
        go (ParLoop _ _ s e i b) = S.unions [goE s, goE e, goE i, goB b]
        go (BlockProgram b)      = goB b
        go _                     = S.empty

        goE = exprNames
        goB = blockNames
        goA (ValueParameter e)  = goE e
        goA (FunParameter name) = S.singleton name
        goA _                   = S.empty

exprNames :: Expression () -> NameSet
exprNames = go
  where go (ArrayElem arr ixs) = S.unions $ go arr : map go ixs
        go (StructField e _)   = go e
        go (FunctionCall f es) = S.unions $ goF f : map go es
        go (Cast _ e)          = go e
        go (AddrOf e)          = go e
        go (Deref e)           = go e
        go _                   = S.empty

        goF (Function name _) = S.singleton name

maybeNames :: (a -> NameSet) -> Maybe a -> NameSet
maybeNames f m = maybe S.empty f m
