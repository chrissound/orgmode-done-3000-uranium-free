{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}

module Main where

import Data.OrgMode.Types

--import Data.OrgMode.Print
import qualified Data.Attoparsec.Text as APS
import Data.OrgMode.Parse.Attoparsec.Document
import qualified Data.Text.IO as TIO
import Text.Pretty.Simple (pPrint)

--import Control.Monad
import Data.HashMap.Strict (member, foldlWithKey', lookup)
import Data.Maybe (catMaybes, fromMaybe)

import Control.Monad
import Data.String.Conversions
import Data.Text (Text, stripPrefix, intercalate)
import Data.Tree
import Data.Monoid

import Data.OrgMode.Print
import System.Environment (getArgs)
import Data.List (sortBy)
import Data.Function (on)

import Data.Thyme hiding (yearMonthDay)
import qualified Data.Thyme.Format as ABC (formatTime)
import System.Locale
import Data.Thyme.Time.Core (fromThyme, toThyme, secondsToDiffTime)
import Control.Lens
import Control.Lens.Iso
import Data.Thyme.Clock
import qualified Data.Thyme.Clock as XYZ
import Data.Thyme.Calendar hiding (yearMonthDay)

mainArgs :: IO String
mainArgs = do
    args <- getArgs
    case args of
      (x:[]) -> pure x
      _ -> error "Invalid parameters passed"

main :: IO ()
main = do
  fp <- mainArgs
  f <- TIO.readFile fp
  case APS.parseOnly (parseDocument [""]) (f) of
    Right doc
     -> do
      let trees = catMaybes $ (fmap isHeadlineCompleted) $ documentHeadlines doc
      let trees' = sortBy (compare `on` snd) $ concat $ concat $ (fmap . fmap) (flattenHeadlines []) trees
      forM_ trees' (\x -> putStrLn $ cs $ printHeadline' (x))
    Left e -> error e

flattenHeadlines :: [Text] -> Tree Headline -> [([Text], UTCView)]
flattenHeadlines par x =
  case subForest x of
  [] -> do
    case (getCompleted (sectionPlannings $ section $ rootLabel x )) of
      Just x' -> [(par <> [fromMaybe "" $ stripPrefix "DONE " $title $ rootLabel x], x')]
      Nothing -> []
  xs -> concat $ flattenHeadlines (par <> [title $ rootLabel x]) <$> xs

type DoneHeadline = Forest Headline

printHeadline' :: ([Text], UTCView) -> Text
printHeadline' (h, p) = mconcat [cs $ ABC.formatTime defaultTimeLocale "%c" ((view (from utcTime) $ p)), " || ", (intercalate " || " h)]

renderDoneTimestamp :: Plannings -> Text
renderDoneTimestamp (Plns hm) = mconcat $ foldlWithKey' (\a k v -> [" [", textShow k, " ", renderTimestamp v, "]"] ++  a) [] hm

getCompleted :: Plannings -> Maybe UTCView
getCompleted (Plns hm) = f' . tsTime <$> Data.HashMap.Strict.lookup CLOSED hm where
  f' dt = UTCTime ((yearMonthDay dt)^.from gregorian) $
    case (hourMinute dt) of
      Just (h,m) -> (secondsToDiffTime $ fromIntegral ((60*60*h)+60*m))
      Nothing -> (secondsToDiffTime (0))

isHeadlineCompleted :: Headline -> Maybe DoneHeadline
isHeadlineCompleted h = do
  let (Plns x) = sectionPlannings . section $ h
  case member CLOSED x of
    True -> Just $ [Node h []] -- Stop at first headline that is closed
    False ->
      case subHeadlines h of
        [] -> Nothing
        sh ->
          case catMaybes (isHeadlineCompleted <$> sh) of
            [] -> Nothing
            sh' -> Just $ (Node h) <$> sh'
