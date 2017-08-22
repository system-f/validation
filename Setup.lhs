#!/usr/bin/env runhaskell
\begin{code}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List ( nub )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.BuildPaths ( autogenPackageModulesDir )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Types.MungedPackageId (MungedPackageId(..))
import Distribution.Types.MungedPackageName (unMungedPackageName)
import Distribution.Types.UnitId (UnitId)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Verbosity ( Verbosity )
import Distribution.Version ( showVersion )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenPackageModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      let testNameStr = unUnqualComponentName $ testName suite
      rewriteFile (dir </> "Build_" ++ testNameStr ++ ".hs") $ unlines
        [ "module Build_" ++ testNameStr ++ " where"
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (testDeps libcfg suitecfg))
        ]
  where
    formatdeps = map (formatone . snd)
    formatone p =
      unMungedPackageName (mungedName p) ++ "-" ++ showVersion (mungedVersion p)

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(UnitId, MungedPackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

\end{code}
