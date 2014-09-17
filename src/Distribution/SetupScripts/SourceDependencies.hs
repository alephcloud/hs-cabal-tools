{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- @Setup.hs@ script for for managing source dependencies. Source dependencies
-- are build dependencies that are not released on <https://hackage.haskell.org
-- Hackage> but instead are obtained from a <https://github.com GiHub>
-- repository.
--
-- It is recommended to use this @Setup.hs@ script with a sandbox.
--
-- This @Setup.hs@ script causes @cabal configure@ to look for a file named
-- @source-dependencies@ in the root directory of the cabal package. If this
-- file exists it expects that each line is either
--
-- * a blank line,
--
-- * a comment line with the first non-space character being @#@, or
--
-- * a line of the form
--
--   @git-repository-url branch-name-or-tag@
--
-- Cabal will then create a directory @source-dependencies-respositories@ if it
-- does not yet exist. It will checkout the respective branch of the respective
-- repository in that directory, by either cloneing or pulling from that URL.
--
-- If there is a cabal sandbox cabal will add the checkedout sources to the
-- sandbox, otherwise cabal will install the sources into the user database.
--
-- USAGE
--
-- There are three ways how this module can be used:
--
-- 1. Copy the code of this module into a file called @Setup.hs@ in the root
--    directory of your package.
--
-- 2. If the /cabal-tools/ package is already installed in the system
--    where the build is done, following code can be used as @Setup.hs@ script:
--
--    > module Main (main) where
--    >
--    > import Distribution.SetupScripts.SourceDependencies
--
-- 3. For usage within a more complex @Setup.hs@ script you shall import this
--    module qualified and use the 'installSourceDependencies' function. For example:
--
--    > module Main (main) where
--    >
--    > import qualified Distribution.SetupScripts.SourceDependencies as CabalTools
--    > import Distribution.Simple
--    >
--    > main :: IO ()
--    > main = defaultMainWithHooks (CabalTools.installSourceDependencies simpleUserHooks)
--    >
--
-- With all methods the field @Build-Type@ in the package description (cabal) file
-- must be set to @Custom@:
--
-- > Build-Type: Custom
--
-- NOTES
--
-- Whenever possible dependencies between packages should be expressed through
-- stable API versions of released cabal packages. A branch in a repository is
-- not stable but by definition a mutable target that changes with every commit
-- to that branch. If the branch is controled by a third party or tracks an
-- upstream branch of a third party those changes are random. A revision in a
-- repository is an immutable reference but it generally doesn't provide any
-- guarantees about the intention to serve as an API contract. It may reflect
-- just some transitional or experiemental state. Finally, there is no central
-- authority that guarantees persistence of such a reference. Git repositories
-- can be deleted at any time by there owners. Packages on Hackage can't be
-- deleted.
--
-- The automation of the build process is only one purpose of the
-- @source-dependencies@ file. The other, more important, purpose of that file
-- is the documentation of these dependencies that are not captured by the
-- cabal version of these packages. Ideally this file is empyt or contains only
-- a small number of entries at the time.
--
-- It is discouraged to use this file to permanently manage dependencies.
-- Example for intended use are
--
-- * For a /small/ number of packages a common feature is developed in a
--   in a tighly integrated style. As soon as the feature is complete a
--   new cabal versions of all packages should be released and the dependencies
--   should be captured through lower bound constraints on the respective
--   versions in the cabal files.
--
-- * A package dependes on a feature or bug fix of a third party dependency
--   that is not yet released on Hackage. If needed a pull request should
--   be submitted to the package of the dependency. A new cabal version of
--   the dependend package must be delayed until a new cabal version with
--   the required changes of the dependency is released on Hackage. If this
--   takes too long, a (probably temporary) fork of the package
--   (with a new name) must be released to public or private Hackage.
--
-- KNOWN ISSUES
--
-- * If there is no sandbox the dependencies are build during configure
--   (and not during @cabal install --dependencies-only@ as it should be the case).
--
-- * Consider to disable installation of source dependencies when there
--   is no sandbox.
--
-- * Removal of source dependencies from the sandbox is not yet implemented and
--   must be done by manually removing the sources from the sandbox.
--
-- * The specification if branches/tags/revisions is not very robust yet, this
--   is mostly due to git missing a unified UI for specifying revisions
--   accross all commands.
--
-- * error handling when calling @git@ and @cabal@ is inferior.
--
module Distribution.SetupScripts.SourceDependencies
( main
, installSourceDependencies
) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad

import Data.Char (isSpace)
import qualified Data.List as L
import Data.Monoid

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils

import System.Directory
import System.FilePath

sourceDependenciesFile :: FilePath
sourceDependenciesFile = "source-dependencies"

sourceDependenciesDir :: FilePath
sourceDependenciesDir = "source-dependencies-repositories"

main :: IO ()
main = defaultMainWithHooks (installSourceDependencies simpleUserHooks)

-- | Modfies a given record of 'UserHooks' to install source dependencies from
-- <https://github.com/ GitHub> repositories that are specified in a file named
-- @source-dependencies@.
--
installSourceDependencies
    :: UserHooks
    -> UserHooks
installSourceDependencies hooks = hooks
    { preConf = installSourceDependenciesPreConf (preConf hooks)
    , hookedPrograms = gitProgram : cabalProgram : hookedPrograms hooks
    }

-- TODO: is there a way with cabal to actually pass an argument from the
-- command line to Args?
installSourceDependenciesPreConf
    :: (Args -> ConfigFlags -> IO HookedBuildInfo)
    -> Args
    -> ConfigFlags
    -> IO HookedBuildInfo
installSourceDependenciesPreConf hook args flags = do
    programDb <- configureProgram verbosity cabalProgram
        =<< configureProgram verbosity gitProgram (configPrograms flags)
    _ <- requireProgram verbosity cabalProgram programDb
    _ <- requireProgram verbosity gitProgram programDb
    whenM (doesFileExist sourceDependenciesFile) $ run programDb
    hook args_ flags

  where
    (args_, force) = if forceArg `L.elem` args
        then (L.delete forceArg args, True)
        else (args, False)

    forceArg = "--force-source-reinstalls" -- FIXME not sure how to make this work

    verbosity = fromFlag $ configVerbosity flags

    commentOrEmpty l = case trim l of
        [] -> True
        ('#':_) -> True
        _ -> False

    getRepoName = takeBaseName

    run programDb = do
        info verbosity $ "process " <> sourceDependenciesFile
        depList <- filter (not . commentOrEmpty) . L.lines <$> readFile sourceDependenciesFile
        -- void $ unless (null depList) $ requireProgram verbosity gitProgram
        void $ forM depList $ \l -> do
            let (repoUrl, revSpec) = (trim *** trim) . break isSpace $ l
            info verbosity $ "Processing source dependency " ++ l

            -- Create directory for source dependency repositories
            createDirectoryIfMissingVerbose verbosity True sourceDependenciesDir

            -- Check if git repository exits
            doesDirectoryExist (sourceDependenciesDir </> getRepoName repoUrl) >>= \x -> if x
                then gitPull repoUrl revSpec
                else gitClone repoUrl revSpec

            -- FIXME maybe we could be a bit more clever here
            doesDirectoryExist "cabal.sandbox.config" >>= \x -> if x
                then cabalAddSandbox repoUrl
                -- FIXME we shouldn't do this at configuration time
                -- but during install.
                else do
                    info verbosity "install source dependencies into user data base"
                    when force $ info verbosity "source dependencies are installed with --force-reinstall"
                    cabalInstall repoUrl
      where
        gitPull repoUrl revSpec = runDbProgram verbosity gitProgram programDb
            [ "-C", "." </> sourceDependenciesDir </> getRepoName repoUrl
            , "pull"
            , "origin"
            , revSpec
            ]

        gitClone repoUrl revSpec = runDbProgram verbosity gitProgram programDb
            [ "-C", "." </> sourceDependenciesDir
            , "clone"
            , "-b", revSpec
            , repoUrl
            ]

        cabalAddSandbox repoUrl = runDbProgram verbosity cabalProgram programDb
            [ "sandbox"
            , "add-source"
            , sourceDependenciesDir </> getRepoName repoUrl
            ]

        -- FIXME FIXME FIMXE
        -- cabalInstallArgs = "install" : [ "--force-reinstall" | force ]
        --
        -- TODO
        --
        -- * we should use "--force-reinstalls" only if requested by the user
        --   on the command line and if the repository actually changed.
        --
        -- * we should use "--reinstall" only if the repository changed.
        --
        -- * we should catch a cabal exception that recommends usage of
        --   reinstall in situtation when the repository didn't change
        --   (or we shouldn't attempt to build at all).
        --
        cabalInstallArgs = "install" : [ "--force-reinstall" | True ]

        cabalInstall repoUrl = case lookupProgram cabalProgram programDb of
            Just configuredCabal -> runProgramInvocation verbosity $
                (programInvocation configuredCabal cabalInstallArgs)
                    { progInvokeCwd = Just $ "." </> sourceDependenciesDir </> getRepoName repoUrl
                    }
            Nothing -> error "Could not execute cabal install: cabal is not configured for execution"

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

gitProgram :: Program
gitProgram = (simpleProgram "git") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "git --version" gives a string like
      -- "git version 1.8.5.2 (Apple Git-48)"
      case words str of
        (_:_:ver:_) -> takeWhile (`elem` ('.':['0'..'9'])) ver
        _           -> ""
  }

cabalProgram :: Program
cabalProgram = simpleProgram "cabal"

whenM :: IO Bool -> IO () -> IO ()
whenM c a = c >>= \x -> when x a

