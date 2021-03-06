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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Feldspar.Compiler.Compiler where

import Data.List (partition)
import Data.Maybe (fromMaybe)
import Control.Applicative

import Feldspar.Transformation
import Feldspar.Core.Constructs (SyntacticFeld)
import Feldspar.Core.Interpretation (defaultFeldOpts)
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Platforms
import Feldspar.Compiler.Backend.C.Plugin.Rule
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Imperative.FromCore
import Feldspar.Compiler.Imperative.Plugin.IVars

data SplitModuleDescriptor = SplitModuleDescriptor
    { smdSource :: Module ()
    , smdHeader :: Module ()
    }

data SplitCompToCCoreResult = SplitCompToCCoreResult
    { sctccrSource :: CompToCCoreResult ()
    , sctccrHeader :: CompToCCoreResult ()
    }

data CompToCCoreResult t = CompToCCoreResult {
    sourceCode      :: String,
    debugModule     :: Module t
}

moduleSplitter :: Module () -> SplitModuleDescriptor
moduleSplitter m = SplitModuleDescriptor {
    smdHeader = Module (hdr ++ createProcDecls (entities m)),
    smdSource = Module body
} where
    (hdr, body) = partition belongsToHeader (entities m)
    belongsToHeader :: Entity () -> Bool
    belongsToHeader StructDef{}                     = True
    belongsToHeader Proc{..} | Nothing <- procBody  = True
    belongsToHeader _                               = False
    -- TODO These only belongs in the header iff the types are used in a
    -- function interface
    createProcDecls :: [Entity ()] -> [Entity ()]
    createProcDecls = concatMap defToDecl
    defToDecl :: Entity () -> [Entity ()]
    defToDecl (Proc n inp outp _) = [Proc n inp outp Nothing]
    defToDecl _ = []

moduleToCCore :: Options -> Module () -> CompToCCoreResult ()
moduleToCCore opts mdl = CompToCCoreResult { sourceCode  = incls ++ res
                                           , debugModule = mdl
                                           }
  where
    res = compToCWithInfos opts mdl
    incls = genIncludeLines opts Nothing


-- | Compiler core
-- This functionality should not be duplicated. Instead, everything should call this and only do a trivial interface adaptation.
compileToCCore
  :: SyntacticFeld c => String -> Options -> c -> SplitCompToCCoreResult
compileToCCore name opts prg = compileToCCore' opts mod
      where
        mod = fromCore opts (encodeFunctionName name) prg

compileToCCore' :: Options -> Module () -> SplitCompToCCoreResult
compileToCCore' opts m = createSplit $ moduleToCCore opts <$> separatedModules
      where
        separatedModules = moduleSeparator $ executePluginChain opts m

        moduleSeparator modules = [header, source]
          where (SplitModuleDescriptor header source) = moduleSplitter modules

        createSplit [header, source] = SplitCompToCCoreResult header source

genIncludeLines :: Options -> Maybe String -> String
genIncludeLines opts mainHeader = concatMap include incs ++ "\n\n"
  where
    include []            = ""
    include fname@('<':_) = "#include " ++ fname ++ "\n"
    include fname         = "#include \"" ++ fname ++ "\"\n"
    incs = includes (platform opts) ++ [fromMaybe "" mainHeader]

-- | Predefined options

defaultOptions :: Options
defaultOptions
    = Options
    { platform          = c99
    , unroll            = NoUnroll
    , debug             = NoDebug
    , memoryInfoVisible = True
    , printHeader       = False
    , rules             = []
    , frontendOpts      = defaultFeldOpts
    , nestSize          = 2
    }

c99PlatformOptions :: Options
c99PlatformOptions              = defaultOptions

c99OpenMpPlatformOptions :: Options
c99OpenMpPlatformOptions        = defaultOptions { platform = c99OpenMp }

tic64xPlatformOptions :: Options
tic64xPlatformOptions           = defaultOptions { platform = tic64x }

unrollOptions :: Options
unrollOptions                   = defaultOptions { unroll = Unroll 8 }

noPrimitiveInstructionHandling :: Options
noPrimitiveInstructionHandling  = defaultOptions { debug = NoPrimitiveInstructionHandling }

noMemoryInformation :: Options
noMemoryInformation             = defaultOptions { memoryInfoVisible = False }

-- | Plugin system

pluginChain :: ExternalInfoCollection -> Module () -> Module ()
pluginChain externalInfo
    = executePlugin RulePlugin (ruleExternalInfo externalInfo)
    . executePlugin RulePlugin (primitivesExternalInfo externalInfo)
    . executePlugin IVarPlugin ()

data ExternalInfoCollection = ExternalInfoCollection
    { primitivesExternalInfo :: ExternalInfo RulePlugin
    , ruleExternalInfo       :: ExternalInfo RulePlugin
    }

executePluginChain :: Options -> Module () -> Module ()
executePluginChain opt prg =
  pluginChain ExternalInfoCollection
    { primitivesExternalInfo = opt{ rules = platformRules $ platform opt }
    , ruleExternalInfo       = opt
    } $ prg

