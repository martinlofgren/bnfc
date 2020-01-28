{-# LANGUAGE NoImplicitPrelude #-}

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
module BNFC.Backend.Rust (makeRust) where

import Prelude'

import BNFC.CF
import BNFC.Options
import BNFC.Backend.Base
import BNFC.PrettyPrint

import BNFC.Backend.Rust.DTypes

import Debug.Trace

-- import Debug.Trace

makeRust :: SharedOptions -> CF -> MkFiles ()
makeRust opts cf = trace (show opts) $ do
  let astFileName    = "src/Abs" ++ name ++ ".rs"
      parserFileName = "src/" ++ name ++ ".lalrpop"
      mainFileName   = "src/main.rs"
      buildFileName  = "build.rs"
      cargoFileName  = "Cargo.toml"

  mkfile astFileName (ast name cf)
  mkfile parserFileName (parser name cf)
  mkfile buildFileName build
  mkfile cargoFileName (cargo name)
  mkfile mainFileName (main name cf)

    where
      name = lang opts

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

build :: Doc
build = vcat [ "extern crate lalrpop;"
             , ""
             , "fn main() {"
             , "    lalrpop::process_root().unwrap();"
             , "}"
             ]

main :: String -> CF -> Doc
main name cf = vcat [ "#[macro_use]"
                    , "extern crate lalrpop_util;"
                    , ""
                    , "lalrpop_mod!(pub " <> text name <> ");"
                    , ""
                    , "pub mod Abs" <> text name <> ";"
                    , ""
                    , "fn main() {"
                    , "}"
                    ]


ast :: String -> CF -> Doc
ast _ cf = vsep . concat $ [ [ "use std::fmt::{Debug, Formatter, Error};" ]
                           , map prEnum $ getAbstractSyntax cf
                           ]
    where
      prEnum (c, rs) = enum2doc (REnum (show c) (map (\(fun, _) -> (RVariant fun Nothing)) rs))

parser :: String -> CF -> Doc
parser name cf = 
    vsep $ [ vcat [ "use" <+> "Abs" <> text name <> "::" <> lbrace <>
                    (sep $ punctuate (comma <> space) $ map (text . show) (cats cf)) <>
                    rbrace <> semi
                  , "use std::str::FromStr;"
                  ]
           , "grammar;"
           , vcat $ map mkOne $ ruleGroups cf
           , vcat [ "Integer: i32 = {"
                  , "    r\"[0-9]+\" => i32::from_str(<>).unwrap()"
                  , "}"
             ]
           ]
    where
      cats cf = [ fst c | c <- getAbstractSyntax cf ]
      mkOne (cat, rules) = text "pub" <+> text (show cat) <> colon <+> text (show cat) <+> equals <+>
                           lbrace <+> (vcat $ map mkRule rules) <+> rbrace

mkRule :: Rul Fun -> Doc
mkRule r@(Rule fun cat _ _ ) = (rhs r) <+> "=>" <+> (text $ show cat) <> "::" <> (text fun)
    where
      rhs r = case rhsRule r of
                []  -> text "empty"
                its -> hsep $ map mkIt its
      mkIt i = case i of
                 Left c  -> (text "<") <> (text $ identCat c) <> (text ">")
                 Right s -> (text "\"") <> text s <> (text "\"")
