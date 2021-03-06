--
-- Copyright (c) 2009-2011, ERICSSON AB
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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Future where

import Language.Syntactic

import Control.Monad.RWS (ask)

import Feldspar.Core.Types (Type,defaultSize)
import Feldspar.Core.Constructs.Future
import Feldspar.Core.Interpretation

import qualified Feldspar.Compiler.Imperative.Representation as Rep (Type(..))
import Feldspar.Compiler.Imperative.Representation (Entity(..), Block(..),Expression(..), fv)
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation

import Data.Map (assocs)

instance Compile dom dom => Compile (FUTURE :|| Type) dom
  where
    compileExprSym = compileProgFresh

    compileProgSym (C' MkFuture) info (Just loc) (p :* Nil) = do
        env <- ask
        let args = [case lookup v (alias env) of
                         Nothing -> mkVariable (compileTypeRep t (defaultSize t)) v
                         Just (VarExpr e) -> e
                         Just (Deref (VarExpr e)) -> e -- These are variables that got a Pointer wrapped around their type in FromCore
                   | (v,SomeType t) <- assocs $ infoVars info
                   ] ++ fv loc
        -- Task core:
        ((_, ws), Block ds bl)  <- confiscateBigBlock $ do
            p' <- compileExprVar p
            tellProg [iVarPut loc p']
        funId  <- freshId
        let coreName = "task_core" ++ show funId
        tellDef [Proc coreName args [] $ Just (Block (decl ws ++ ds) bl)]
        -- Task:
        let taskName = "task" ++ show funId
        let runTask = Just $ toBlock $ run coreName args
        tellDef [Proc taskName [] [mkNamedRef "params" Rep.VoidType (-1)] runTask]
        -- Spawn:
        tellProg [iVarInit (AddrOf loc)]
        tellProg [spawn taskName args]

    compileProgSym (C' Await) _ loc (a :* Nil) = do
        fut <- compileExprVar a -- compileExpr a
        tellProg [iVarGet l fut | Just l <- [loc]]

