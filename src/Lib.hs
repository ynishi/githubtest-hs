{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Prelude.Compat                 hiding (filter, head, map)

import           Control.Monad                  (filterM, mapM, sequence)
import           Data.Either                    (fromRight)
import           Data.Monoid                    ((<>))
import           Data.Proxy                     (Proxy (..))
import           Data.Text                      (Text, intercalate, pack)
import           Data.Text.IO                   as T (putStrLn)
import           Data.Vector                    as V (Vector, empty, filter,
                                                      fromList, head, map,
                                                      toList)

import qualified GitHub                         as GH
import qualified GitHub.Endpoints.Issues.Labels as GHIL
import qualified GitHub.Endpoints.Organizations as GHEO
import qualified GitHub.Endpoints.Repos         as GHER

data Context = Context
  { users         :: [Text]
  , excludeLabels :: [Text]
  , endpoint      :: Text
  } deriving (Eq, Ord, Show)

c =
  Context
    { users = ["mike-burns"]
    , excludeLabels = ["WIP"]
    , endpoint = "https://api.github.com"
    }

getPRList :: Context -> IO (V.Vector (V.Vector (V.Vector ())))
getPRList =
  mapM
    ((\x -> do
        possibleOrgs <- GHEO.publicOrganizationsFor x
        let orgs = fromRight empty possibleOrgs
        mapM
          (\org -> do
             let orgName = GH.simpleOrganizationLogin org
             T.putStrLn . pack . show $ orgName
             possibleRepos <- GHER.organizationRepos orgName
             let repoNames = map GH.repoName . fromRight empty $ possibleRepos
             mapM
               (\repoName -> do
                  let ownerName =
                        GH.mkName (Proxy :: Proxy GH.Owner) . GH.untagName $
                        orgName
                  possiblePRs <-
                    executeReq $
                    GH.pullRequestsForR
                      ownerName
                      repoName
                      GH.stateOpen
                      GH.FetchAll
                  let prs = fromRight empty possiblePRs
                  filterd <-
                    filterM (condLabelPullRequest (excludeLabels c) repoName) .
                    toList . filter (condOwnerPullRequest ownerName) $
                    prs
                  T.putStrLn .
                    pack . show . map formatSimplePullRequest . V.fromList $
                    filterd)
               repoNames)
          orgs) .
     GH.mkName (Proxy :: Proxy GH.User)) .
  V.fromList . users
  where
    auth = GH.EnterpriseOAuth (endpoint c) ""
    executeReq =
      case GH.endpoint auth of
        Nothing -> GH.executeRequest'
        _       -> GH.executeRequest auth

condOwnerPullRequest owner pr =
  owner ==
  (GH.mkName (Proxy :: Proxy GH.Owner) .
   GH.untagName . GH.simpleUserLogin . GH.simplePullRequestUser $
   pr)

condLabelPullRequest excludeLabels repoName pr = do
  possible <- GHIL.labelsOnIssue ownerName repoName issueId
  return .
    not .
    any ((`elem` excludeLabels) . GH.untagName . GH.labelName) . fromRight empty $
    possible
  where
    ownerName =
      GH.mkName (Proxy :: Proxy GH.Owner) .
      GH.untagName . GH.simpleUserLogin . GH.simplePullRequestUser $
      pr
    issueId =
      GH.mkId (Proxy :: Proxy GH.Issue) .
      GH.unIssueNumber . GH.simplePullRequestNumber $
      pr

formatSimplePullRequest = GH.simplePullRequestTitle
