module Main where

import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest (doctest)

main :: IO ()
main =
  doctest args
  where
    args = flags ++ pkgs ++ module_sources

