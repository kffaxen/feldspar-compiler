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

module Feldspar.Compiler.Imperative.FromCore.Mutable where



import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Mutable
import Feldspar.Core.Constructs.MutableArray
import Feldspar.Core.Constructs.MutableReference

import Feldspar.Compiler.Imperative.Frontend hiding (Type)
import Feldspar.Compiler.Imperative.FromCore.Interpretation



instance (Compile dom dom, Project (CLambda Type) dom) => Compile (MONAD Mut) dom
  where
    compileProgSym Bind _ loc (ma :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        = do
            e <- compileExpr ma
            withAlias v e $ compileProg loc body

{- TODO reenable this implementation! The case above inlines too much if v is used more than once in the body
    compileProgSym Bind _ loc (ma :* (Symbol (Decor info lam) :$ body) :* Nil)
        | Just (Lambda v) <- prjCtx typeCtx lam
        = do
            let var = mkVar (compileTypeRep $ argType $ infoType info) v
            withDecl var $ do
              compileProg var ma
              compileProg loc body
-}

    compileProgSym Then _ loc (ma :* mb :* Nil) = do
        compileExpr ma
        compileProg loc mb

    compileProgSym Return info loc (a :* Nil)
        | MutType UnitType <- infoType info = return ()
        | otherwise                         = compileProg loc a

    compileProgSym When _ loc (c :* action :* Nil) = do
        c' <- compileExpr c
        (_, Bl ds body) <- confiscateBlock $ compileProg loc action
        tellProg [If c' (Block ds body) Skip]

instance (Compile dom dom, Project (CLambda Type) dom) => Compile Mutable dom
  where
    compileProgSym Run _ loc (ma :* Nil) = compileProg loc ma

    compileExprSym Run _ (ma :* Nil) = compileExpr ma

instance (Compile dom dom, Project (CLambda Type) dom) => Compile MutableReference dom
  where
    compileProgSym NewRef _ loc (a :* Nil) = compileProg loc a
    compileProgSym GetRef _ loc (r :* Nil) = compileProg loc r
    compileProgSym SetRef _ _   (r :* a :* Nil) = do
        var  <- compileExpr r
        compileProg var a

    compileExprSym GetRef _ (r :* Nil) = compileExpr r
    compileExprSym feat info args      = compileProgFresh feat info args

instance (Compile dom dom, Project (CLambda Type) dom) => Compile MutableArray dom
  where
    compileProgSym NewArr_ _ loc (len :* Nil) = do
      l <- compileExpr len
      tellProg [initArray loc l]

    compileProgSym NewArr _ loc (len :* a :* Nil) = do
        let ix = Var U32 "i"
        a' <- compileExpr a
        l  <- compileExpr len
        tellProg [initArray loc l]
        tellProg [For "i" l 1 (Seq [assignProg (loc :!: ix) a'])]

    compileProgSym GetArr _ loc (arr :* i :* Nil) = do
        arr' <- compileExpr arr
        i'   <- compileExpr i
        assign loc (arr' :!: i')

    compileProgSym SetArr _ _ (arr :* i :* a :* Nil) = do
        arr' <- compileExpr arr
        i'   <- compileExpr i
        a'   <- compileExpr a
        assign (arr' :!: i') a'

    compileProgSym a info loc args = compileExprLoc a info loc args

    compileExprSym ArrLength info (arr :* Nil) = do
        a' <- compileExpr arr
        return $ Fun (compileTypeRep (infoType info) (infoSize info)) "getLength" [a']

    compileExprSym a info args = compileProgFresh a info args
