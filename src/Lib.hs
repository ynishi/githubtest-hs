{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Prelude.Compat                   hiding (head, map)

import           Control.Monad                    (mapM, sequence)
import           Data.Either                      (fromRight)
import           Data.Monoid                      ((<>))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, intercalate, pack)
import           Data.Text.IO                     as T (putStrLn)
import           Data.Vector                      as V (Vector, empty, fromList,
                                                        head, map, toList)

import qualified GitHub.Data.Definitions          as DD
import qualified GitHub.Data.Name                 as DN
import qualified GitHub.Endpoints.Organizations   as EO
import qualified GitHub.Endpoints.PullRequests    as EP
import qualified GitHub.Endpoints.Repos           as ER
import qualified GitHub.Endpoints.Users.Followers as GitHub

newtype Context = Context
  { users :: [Text]
  }

c = Context {users = ["mike-burns"]}

getPRList :: Context -> IO (V.Vector (V.Vector (V.Vector ())))
getPRList =
  mapM
    ((\x -> do
        possibleOrgs <- EO.publicOrganizationsFor x
        let orgs = fromRight empty possibleOrgs
        T.putStrLn $
          foldMap
            ((<> "\n") . (GitHub.untagName . EO.simpleOrganizationLogin))
            orgs
        mapM
          (\org -> do
             let orgName = EO.simpleOrganizationLogin org
             possibleRepos <- ER.organizationRepos orgName
             let repos = fromRight empty possibleRepos
             let repoNames = map ER.repoName repos
             mapM
               (\repoName -> do
                  let orgNameText = GitHub.untagName orgName
                  let ownerName =
                        DN.mkName (Proxy :: Proxy DD.Owner) orgNameText
                  possiblePRs <- EP.pullRequestsFor ownerName repoName
                  case possiblePRs of
                    (Left error) -> T.putStrLn . pack $ "Error: " ++ show error
                    (Right pullRequests) ->
                      T.putStrLn . pack . show $ pullRequests)
               repoNames)
          orgs) .
     DN.mkName (Proxy :: Proxy GitHub.User)) .
  V.fromList . users

formatPullRequest = EP.pullRequestTitle
