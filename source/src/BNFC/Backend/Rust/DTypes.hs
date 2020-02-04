--{-# LANGUAGE NoImplicitPrelude #-}

{-
    BNF Converter: Rust data types
    Copyright (C) 2020  Author:  Martin Löfgren

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
    , allocStrat
    ) where

--import Prelude'

import BNFC.PrettyPrint
import BNFC.CF

import Debug.Trace

-- | The Enum is the Rust take on algebraic data types. We use it as our
--   abstract data type.
data REnum = REnum RIdent [RVariant]
instance Show REnum where
    show = render . enum2doc

-- | An identifier is a string.
type RIdent = String

-- | The variant: an identifier and possibly associated data.
data RVariant = RVariant RIdent RData
instance Show RVariant where
    show = render . var2doc

-- | If we have assoicated data, we need to keep track of where to allocate it.
type RData = Maybe [(RIdent, RAlloc)]

-- | In Rust, we need to keep track of what can be allocated on the stack and
--   what needs to be put on the heap (Boxed in Rust parlance). We also handle
--   lists, which are implemented using the Vector data type.
data RAlloc   = Stack | Box | Vector
                deriving Show


-- | There's a conflict between (Prelude.<>) and (BNFC.PrettyPrint.<>) and it's
--   tedious to write the qualified name. Further, this operator looks like a
--   Tie Bomber.
(<-:->) :: Doc -> Doc -> Doc
(<-:->) = (BNFC.PrettyPrint.<>)
infixl 6 <-:->


-- | Terminals can be stack allocated, non-terminals need to be put on the heap.
allocStrat :: Cat -> (RIdent, RAlloc)
allocStrat (Cat c)            = (c, Box)
allocStrat (TokenCat c)       = (c, Stack)
allocStrat (ListCat _)        = ("List", Vector)
allocStrat cat@(CoercCat _ _) = allocStrat $ normCat cat


-- | Render a Rust enum.
enum2doc :: REnum -> Doc
enum2doc (REnum ident vars) = hang pre 4 body $+$ rbrace
  where
    pre  = text "pub enum" <+> text ident <+> lbrace
    body = vcat $ punctuate comma $ map var2doc vars


-- | Render a Rust enum variant.
var2doc :: RVariant -> Doc
var2doc (RVariant ident ds) =
  text ident <-:-> case ds of
    Nothing  -> empty
    Just ds' -> lparen <-:-> hsep (punctuate comma (map variantData2doc ds'))
                       <-:-> rparen


-- | LALRPOP rule.
data RRule = RRule RIdent Res [Rhs]
instance Show RRule where
    show = render . rule2doc

data Res = Res RIdent RAlloc
newtype Rhs = Rhs [RCat]
data RCat = Nonterminal String
          | Terminal String


-- | Render a lalrpop rule.
rule2doc :: RRule -> Doc
rule2doc (RRule ident (Res i a) rhss) = hang pre 4 body $+$ rbrace
    where
      pre = text "pub" <+> text ident <+> colon <+> variantData2doc (i, a) <+> equals <+> lbrace
      body = empty


-- | Render variant data.
variantData2doc :: (RIdent, RAlloc) -> Doc
variantData2doc (i, Stack) = text $ rustDataType i
variantData2doc (i, Box)  = hcat $ map text ["Box<", rustDataType i, ">"]


-- | Map BNFC internal data types to Rust data types.
rustDataType :: String -> String
rustDataType n | n == "String"  = undefined
               | n == "Integer" = "i32"
               | n == "Double"  = undefined
               | n == "Char"    = undefined
               | n == "Ident"   = undefined
               | otherwise      = n

