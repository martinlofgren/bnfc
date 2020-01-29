--{-# LANGUAGE NoImplicitPrelude #-}

{-
    BNF Converter: Rust Main file
    Copyright (C) 2019  Author:  Martin Löfgren

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

module BNFC.Backend.Rust.DTypes
    ( REnum (..)
    , RVariant (..)
    , RAlloc (..)
    , RIdent
    , enum2doc
    ) where

--import Prelude'

import BNFC.PrettyPrint

import Debug.Trace

-- Rust Enum

data REnum    = REnum RIdent [RVariant]
type RIdent   = String
data RVariant = RVariant RIdent RData
type RData    = (Maybe [(RIdent, RAlloc)])
data RAlloc   = Stack | Box | Vector
                deriving Show
instance Show REnum where
    show = render . enum2doc

enum2doc :: REnum -> Doc
enum2doc (REnum ident vars) = (hang pre 4 body) $+$ rbrace
    where
      pre = (text "pub enum") <+> (text ident) <+> lbrace
      body = vcat $ punctuate comma $ map var2doc vars

instance Show RVariant where
    show = render . var2doc

var2doc :: RVariant -> Doc
var2doc (RVariant ident ds) = trace (show ident ++ " -> " ++ show ds) $
    text ident <+> case ds of
                    Nothing  -> empty
                    Just ds' -> lparen <+> hsep (punctuate comma (map alloc2doc ds')) <+> rparen

testREnum :: REnum
testREnum = REnum "Expr" [ RVariant "Number" (Just [("i32", Stack)])
                    , RVariant "Op" (Just [("Expr", Box), ("Opcode", Stack), ("Expr", Box)])
                    , RVariant "Error" Nothing
                    ]


-- LALRPOP Rule
data RRule = RRule RIdent Res [Rhs]
data Res   = Res RIdent RAlloc
data Rhs   = Rhs [RCat]
data RCat  = Nonterminal String
           | Terminal String

instance Show RRule where
    show = render . rule2doc

rule2doc :: RRule -> Doc
rule2doc (RRule ident (Res i a) rhss) = (hang pre 4 body) $+$ rbrace
    where
      pre = (text "pub") <+> (text ident) <+> colon <+> alloc2doc (i, a) <+> equals <+> lbrace
      body = empty

testRRule :: RRule
testRRule = RRule "Expr" (Res "Expr" Box) []


alloc2doc :: (RIdent, RAlloc) -> Doc
alloc2doc (i, Stack) = text i
alloc2doc (i, Box)  = hcat $ map text ["Box<", i, ">"]
                     
