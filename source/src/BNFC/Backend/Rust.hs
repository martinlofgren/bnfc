{-# LANGUAGE NoImplicitPrelude #-}

{-
    BNF Converter: Rust Main file
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
module BNFC.Backend.Rust (makeRust) where

import Prelude'

import BNFC.CF
import BNFC.Options
import BNFC.Backend.Base
import BNFC.PrettyPrint
import BNFC.Utils

import BNFC.Backend.Rust.DTypes

import Debug.Trace

-- | Entry point for the Rust backend.
makeRust :: SharedOptions -> CF -> MkFiles ()
makeRust opts cf = do
  let astFileName    = "src/abs_" ++ name ++ ".rs"
      -- dispFileName   = "src/display_" ++ name ++ ".rs"
      parserFileName = "src/" ++ name ++ ".lalrpop"
      mainFileName   = "src/main.rs"
      buildFileName  = "build.rs"
      cargoFileName  = "Cargo.toml"

  -- The abstract syntax data type and pretty printer (fmt::Display implementation)
  mkfile astFileName (ast name cf)
  -- mkfile displayFileName (display name cf)

  -- Generate lalrpop lexer/parser and associated build script
  mkfile parserFileName (parser name cf)
  mkfile buildFileName build

  -- Genereate Cargo project files
  mkfile cargoFileName (cargo name)
  mkfile mainFileName (main name cf)

    where
      name = mkName [] SnakeCase $ lang opts


-- | Generate Cargo project files.
cargo :: String -> Doc
cargo name = vcat [ "[package]"
                  , "name = \"" <> text name <> "\""
                  , "version = \"0.1.0\""
                  , "authors = []"
                  , "edition = \"2018\""
                  , ""
                  , "[build-dependencies]"
                  , "lalrpop = \"0.17.2\""
                  , ""
                  , "[dependencies]"
                  , "lalrpop-util = \"0.17.2\""
                  , "regex = \"0.2.1\""
                  ]


-- | The build script, always the same.
build :: Doc
build = vcat [ "extern crate lalrpop;"
             , ""
             , "fn main() {"
             , "    lalrpop::process_root().unwrap();"
             , "}"
             ]


-- | Minimal main function, sole purpose to ensure the project compiles.
--   TODO: Should restructure project as lib instead, and provide examples.
main :: String -> CF -> Doc
main name _ = vcat [ "#[macro_use]"
                   , "extern crate lalrpop_util;"
                   , ""
                   , "lalrpop_mod!(pub " <> text name <> ");"
                   , ""
                   , "pub mod abs_" <> text name <> ";"
                   , ""
                   , "fn main() {"
                   , "}"
                   ]


-- | The abstract syntax data types.
ast :: String -> CF -> Doc
ast _ cf = vsep . concat $ [ [ "use std::fmt::{Debug, Formatter, Error};" ]
                           , map prEnum $ getAbstractSyntax cf
                           ]
    where
      prEnum (Cat c, rs) = enum2doc (REnum c (map vars rs))
      vars (fun, []) = RVariant fun Nothing
      vars (fun, cs) = RVariant fun $ Just $ map allocStrat cs


-- | Generate the lalrpop lexer/parser.
parser :: String -> CF -> Doc
parser name cf =
    vsep [ vcat [ "use crate::" <> "abs_" <> text name <> "::" <> lbrace <>
                  sep (punctuate (comma <> space) $ map (text . show) (cats cf)) <>
                  rbrace <> semi
                , "use std::str::FromStr;"
                ]
         , "grammar;"
         , vsep $ map mkOne $ ruleGroups cf
         , vcat [ "Integer: i32 = {"
                , "    r\"[0-9]+\" => i32::from_str(<>).unwrap()"
                , "}"
                ]
         ]
    where
      cats cf = [ fst c | c <- getAbstractSyntax cf ]
      mkOne (cat, rules) = hang (text "pub" <+> text (show cat) <> colon <+> showCat cat <+> equals <+>
                           lbrace) 4 (vcat $ map mkRule rules) $$ rbrace
      showCat cat = case allocStrat cat of
                      (s, Box)   -> text "Box<" <> text s <> text ">"
                      (s, Stack) -> text s


-- | Generate a rule, taking allocation strategy (heap/stack) into account.
mkRule :: Rul Fun -> Doc
mkRule r@(Rule fun cat _ _ ) =
    rhs r <+> if isCoercion fun then empty else
                  "=>" <+> wrap (text (show $ normCat cat) <> "::" <> text fun)
                           (case allocStrat cat of
                              (_, Stack) -> (empty, empty)
                              (_, Box)   -> (text "Box::new(", text "(<>))")
                           ) <> comma
    where
      rhs r = case rhsRule r of
                []  -> text "empty"
                its -> hsep $ map mkIt its
      mkIt i = case i of
                 Left c  -> text "<" <> text (identCat c) <> text ">"
                 Right s -> text "\"" <> text s <> text "\""


-- | Helper to wrap a document.
wrap :: Doc -> (Doc, Doc) -> Doc
wrap body (pre, post) = pre <> body <> post
