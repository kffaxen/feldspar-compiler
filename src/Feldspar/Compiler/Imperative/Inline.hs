--
-- Copyright (c) 2020, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module Feldspar.Compiler.Imperative.Inline (inline) where

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Backend.C.Options(Options(..), Platform(..))
import Feldspar.Compiler.Imperative.DeadFunElim

import qualified Data.Map as M
import Data.List (mapAccumL)
import Control.Monad.State
import Control.Monad.Writer

-- | The state type for the inlining monad for Expressions.
--   An integer for unique renamings of the inlined function or procedure bodies.
--   The inlined bodies, since an Expression can not contain statements.
data IState = IState {next :: Int, code :: [Block ()]}

type IE a = State IState a -- ^ Monad for inlining in Expressions
type IP a = State Int a    -- ^ Monad for inlining in Programs

-- | Inline function and procedure calls if the callee is defined in the program, main entry point.
inline :: Options -> Module () -> Module ()
inline opts m@Module{entities = ents}
  | pInline $ platform opts = deadFunElim opts m{entities = snd $ mapAccumL (inlineEnt opts) (M.empty,0) ents}
  | otherwise = m

type ProcEnv = M.Map String (Entity ()) -- ^ Mapping procedure names to bodies for inining
type VarEnv = M.Map String (Expression ()) -- ^ Mapping variable names to fresh variable names

-- | Inline calls in Proc entities and enter them into the procedure environment
inlineEnt :: Options -> (ProcEnv,Int) -> Entity () -> ((ProcEnv,Int), Entity ())
inlineEnt opts (env,k) e@Proc{} = ((if loopBody e then env else M.insert (procName e) e' env, k'), e')
  where e' = e{procBody = b'}
        (b',k') = runState (inlineBody opts env $ procBody e) k
inlineEnt opts st e = (st, e)

inlineBody :: Options -> ProcEnv -> Maybe (Block ()) -> IP (Maybe (Block ()))
inlineBody opts env (Just p) = Just <$> inlineBlock opts env p
inlineBody opts env Nothing = return Nothing

inlineBlock :: Options -> ProcEnv -> Block () -> IP (Block ())
inlineBlock opts env (Block ds p) = progToBlock <$> (emitExpansion $ BlockProgram <$> (Block <$> mds <*> mp))
  where mds = mapM (inlineDecl opts env) ds
        mp = toIM $ inlineProgram opts env p

-- | Intelligent wrapper for Block
progToBlock :: Program () -> Block ()
progToBlock (BlockProgram b) = b
progToBlock p                = Block [] p

-- | Inline calls in the initializations
inlineDecl :: Options -> ProcEnv -> Declaration () -> IE (Declaration ())
inlineDecl opts env (Declaration v (Just e)) = Declaration v . Just <$> inlineExp opts env e
inlineDecl opts env d                        = return d

-- | Given an entity environment env and a statement p, inline calls to entities in env occurring in e
inlineProgram :: Options -> ProcEnv -> Program () -> IP (Program ())
inlineProgram opts env = go
  where go (Assign lhs rhs)      = emitExpansion $ (\ ilhs -> inlineAssign opts env ilhs rhs) =<< goE lhs
        go (ProcedureCall p ps)  = emitExpansion $ toIM . inlineProcCall opts (M.lookup p env) p =<< (mapM goP ps)
        go (Sequence ps)         = Sequence <$> mapM go ps
        go (Switch e alts)       = emitExpansion $ Switch <$> goE e <*> toIM (mapM goA alts)
        go (SeqLoop c cc b)      = do cci <- goB cc
                                      bi  <- goB b
                                      seqLoop (goE c) cci bi
        go (ParLoop p n s e i b) = emitExpansion $ ParLoop p n <$> goE s <*> goE e <*> goE i <*> toIM (goB b)
        go (BlockProgram b)      = BlockProgram <$> goB b
        go p                     = return p

        goP (ValueParameter e) = ValueParameter <$> goE e
        goP p                  = pure p

        goE e = inlineExp opts env e
        goB b = inlineBlock opts env b
        goA (p,b) = (\ b -> (p,b)) <$> goB b

-- | Inline in the rhs of an assignement (the lhs is assumed to already be processed).
inlineAssign :: Options -> ProcEnv -> Expression () -> Expression () -> IE (Program ())
inlineAssign opts env ilhs rhs
  | FunctionCall f es <- rhs = go (M.lookup (funName f) env) f =<< mapM (inlineExp opts env) es
  | otherwise = Assign ilhs <$> inlineExp opts env rhs
  where go (Just ent) f ies = blockToProg <$> (inlineProc opts ilhs ent $ map ValueParameter ies)
        go Nothing    f ies = return $ Assign ilhs (FunctionCall f ies)

-- | Inline a procedure call
inlineProcCall :: Options -> Maybe (Entity ()) -> String -> [ActualParameter ()] -> IP (Program ())
inlineProcCall opts (Just e) proc ps = emitExpansion $ blockToProg <$> inlineProc opts lhs e ps
  where lhs = error $ "Inline.inlineProc: return value in procedure " ++ proc
inlineProcCall opts _ proc args = return $ ProcedureCall proc args

-- | Inline a funtion call, replacing it with a fresh variable
inlineFunCall :: Options -> Maybe (Entity ()) -> Function -> [Expression ()] -> IE (Expression ())
inlineFunCall opts (Just e) f es = do v <- newVar $ returnType f
                                      b <- inlineProc opts (VarExpr v) e (map ValueParameter es)
                                      emit $ prependDecls [Declaration v Nothing] b
                                      return $ VarExpr v
inlineFunCall opts _        f es = return $ FunctionCall f es

-- | Add a list of declartions to the front of the declarations in a Block.
prependDecls :: [Declaration ()] -> Block () -> Block ()
prependDecls ds' (Block ds p) = Block (ds' ++ ds) p

-- | Generate initializing declarations for the instantiated formal parameters of an inlined procedure
connectArg :: Variable () -> ActualParameter () -> Declaration ()
connectArg v (ValueParameter e) = Declaration v (Just e)
connectArg v vp                 = error $ "Inline.connectArg: illegal actual parameter " ++ show vp ++ " for " ++ show v

-- | Instantiate a procedure body
inlineProc :: Options -> Expression () -> Entity () -> [ActualParameter ()] -> IE (Block ())
inlineProc opts lhs (Proc _ _ iPs _ (Just b)) ps
  = do suff <- newSuff
       let (ve,vs) = reVars suff M.empty iPs
       return $ prependDecls (zipWith connectArg vs ps)
                             (instBlock opts lhs suff ve b)
inlineProc opts lhs ent ps = error $ "Inline.inlineEnt: illegal entity " ++ show ent

-- | Extract the code part of the IE monad and prepend it to a program; change to the IP monad
emitExpansion :: IE (Program ()) -> IP (Program ())
emitExpansion pm = state transform
  where transform i = let (p,s) = runState pm $ IState i []
                       in (foldl mkB p $ code s, next s)
        mkB p (Block ds b) = BlockProgram $ Block ds $ Sequence [b, p]

-- | Inline function calls found in an expression
inlineExp :: Options -> ProcEnv -> Expression () -> IE (Expression ())
inlineExp opts env = go
  where go (ArrayElem arr ixs) = ArrayElem <$> go arr <*> mapM go ixs
        go (StructField e f)   = StructField <$> go e <*> pure f
        go (FunctionCall f es) = inlineFunCall opts (M.lookup (funName f) env) f =<< mapM go es
        go (Cast t e)          = Cast t <$> go e
        go (AddrOf e)          = AddrOf <$> go e
        go (Deref e)           = Deref <$> go e
        go e                   = pure e

-- | Inline calls in a sequential loop
--   The complexity comes from the need to get the inlinings from the condition into the cond calc block
--   and float the variable declarations out of the loop.
seqLoop :: IE (Expression ()) -> Block () -> Block () -> IP (Program ())
seqLoop em cc b = do i <- get
                     let (e, IState j bs) = runState em $ IState i []
                         ds = [d{initVal = Nothing} | b <- bs, d <- locals b]
                         Block ccDs ccP = cc
                     put j
                     return $ mkBlockProg ds $ SeqLoop e (Block ccDs $ Sequence $ ccP : map blockToProgAssigns bs) b

-- | Convert the initializations in a block to assignments and preped them to the body.
blockToProgAssigns :: Block () -> Program ()
blockToProgAssigns (Block ds p) = Sequence $ [Assign (VarExpr v) e | Declaration v (Just e) <- ds] ++ [p]

-- | Make a block out of a list of declaration and a program unless the list is empty
mkBlockProg :: [Declaration ()] -> Program () -> Program ()
mkBlockProg [] p = p
mkBlockProg ds p = BlockProgram $ Block ds p

-- | Intelligent constructor
blockToProg :: Block () -> Program ()
blockToProg (Block [] p) = p
blockToProg b            = BlockProgram b

-- | Rename a list of variables and add the renamings to a variable environment
reVars :: String -> VarEnv -> [Variable ()] -> (VarEnv, [Variable ()])
reVars suff ve vs = mapAccumL (reVar suff) ve vs

-- | Rename one variable and add the renaming to a variable environment
reVar :: String -> VarEnv -> Variable () -> (VarEnv, Variable ())
reVar suff ve v@Variable{varName = s} = (M.insert s (VarExpr v') ve, v')
  where v' = v{varName = s ++ suff}

-- | Instantiate a block, renaming the local variables
instBlock :: Options -> Expression () -> String -> VarEnv -> Block () -> Block ()
instBlock opts lhs suff ve (Block ds p) = Block ds' (instProg opts lhs suff ve' p)
  where (ve',ds') = mapAccumL goD ve ds
        goD veS (Declaration v me) = let (veS',v') = reVar suff veS v
                                         me' = fmap (instExp opts veS) me
                                      in (veS', Declaration v' me')

-- | Instantiating an expression
instExp :: Options -> VarEnv -> Expression () -> Expression ()
instExp opts ve = go
  where go (VarExpr v)         = maybe (VarExpr v) id $ M.lookup (varName v) ve
        go (ArrayElem arr ixs) = ArrayElem (go arr) (map go ixs)
        go (StructField e f)   = StructField (go e) f
        go (FunctionCall f es) = FunctionCall f $ map go es
        go (Cast t e)          = Cast t $ go e
        go (AddrOf e)          = AddrOf $ go e
        go (Deref e)           = Deref $ go e
        go e                   = e

-- | Instantiating a program
instProg :: Options -> Expression () -> String -> VarEnv -> Program ()
         -> Program ()
instProg opts lhs suff ve = go
  where go (Assign lhs rhs)      = Assign (goE lhs) (goE rhs)
        go (ProcedureCall "return" [ValueParameter e])
                                 = Assign lhs $ goE e
        go (ProcedureCall p ps)  = ProcedureCall p $ map goP ps
        go (Sequence ps)         = Sequence $ map go ps
        go (Switch e alts)       = Switch (goE e) [(p, goB b) | (p,b) <- alts]
        go (SeqLoop c cc b)      = SeqLoop (goE c) (goB cc) (goB b)
        go (ParLoop p n s e i b) = ParLoop p n' (goE s) (goE e) (goE i) b'
          where (ve',n') = reVar suff ve n
                b' = instBlock opts lhs suff ve' b
        go (BlockProgram b)      = BlockProgram $ goB b
        go p                     = p

        goE e = instExp opts ve e

        goP (ValueParameter e) = ValueParameter $ goE e
        goP p                  = p

        goB b = instBlock opts lhs suff ve b

-- | Get a new unique suffix for renaming of parameters and local variables
newSuff :: IE String
newSuff = state $ \ s -> ("_i" ++ show (next s), s{next = next s + 1})

-- | Get a new unique variable for representing the result of an inlined FunctionCall
newVar :: Type -> IE (Variable ())
newVar t = state $ \ s -> (Variable t $ "inline" ++ show (next s), s{next = next s + 1})

-- | Record the instantiated body on an inlined entity for later
emit :: Block () -> IE ()
emit p = modify $ \ s -> s{code = p : code s}

-- | Convert a computation that cannot generate code into one that passes code through
toIM :: IP a -> IE a
toIM m = state $ \ s -> let (x,i) = runState m $ next s in (x, s{next = i})
