{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Todou.Domain.Stat where

import Data.Time (Year, Day, pattern YearMonthDay, gregorianMonthLength)
import Data.Time.Calendar.Month (Month, pattern YearMonth, pattern MonthDay)
import Amazonka.Data (ToJSON (..), (.=))
import Data.Map qualified as Map
import Data.Map (Map)
import Todou.Domain.Todo (Todo (..), Entry (..))
import Data.Aeson qualified as Aeson
import Data.Maybe (catMaybes)
import Data.List (sort, foldl')
import Data.Containers.ListUtils (nubOrd)
import Data.Text (Text)
import Data.ByteString (ByteString)

----------------------------------------
-- Domain.Stat
----------------------------------------

-- | Cumulative Flow Resolution. This type indicates the scale of the
-- CF diagram.
data CFR
  = CFRYear Year
  | CFRMonth Month
  | CFRAll
  deriving (Show, Eq)


-- | One step on the Cumulative Flow Diagram
data CF = CF
  { date       :: Day
  , completed  :: Int
  , ongoing    :: Int
  }
  deriving (Show, Eq)


instance ToJSON CF where
  toJSON (CF date completed ongoing) =
    Aeson.object
      [ "date"      .= date
      , "completed" .= completed
      , "ongoing"   .= ongoing
      ]


-- Cumulative Flow Data for a month, resolution is day
newtype CFDMonth = CFDMonth [CF]
  deriving (Show, Eq)
  deriving newtype ToJSON


-- | Convert a CFR into a day range.
cfrToDayRange ::  CFR -> Map Day (Maybe Todo) -> (Day, Day)
cfrToDayRange r todos =
  case r of
    CFRYear year ->
      (YearMonthDay year 1 1, YearMonthDay year 12 31)
    CFRMonth month ->
      let YearMonth year monthOfYear = month
       in ( MonthDay month 1, MonthDay month (gregorianMonthLength year monthOfYear))
    CFRAll ->
      let (k1, _) = Map.findMin todos
          (k2, _) = Map.findMax todos
       in (k1, k2)


-- | Create a list of CF that can be plotted as a Cumulative Flow Diagram.
-- This function uses `rangeQuery` which is O(log(n) + log(n')).
createCumulativeFlow :: CFR -> Map Day (Maybe Todo) -> [CF]
createCumulativeFlow r todos =
  let
      (start, end)  = cfrToDayRange r todos

      -- todosInRange  = catMaybes $ rangeQuery start end todos
      todosInRange  = catMaybes $ rangeQuery start end todos

      completedCnt  = let es = concat $ fmap (.entries) todosInRange
                       in foldl'
                            (\acc (Entry { completedDate }) ->
                              case completedDate of
                                Just d  -> Map.insertWith (+) d 1 acc
                                Nothing -> acc)
                            Map.empty es

      completedDays  = Map.keys completedCnt

      -- all days that something happened
      days           = nubOrd $ sort $ completedDays ++ fmap (\t -> t.date) todosInRange

      go (cfs, cc) d = if d > end
                         then (cfs, cc)
                         else case Map.lookup d todos of
                                Just (Just (Todo {entries})) ->
                                  let total  = length entries
                                      comp   = Map.findWithDefault 0 d cc
                                      uncomp = total - comp
                                      cf'    = case cfs of
                                                 []   -> CF { date      = d
                                                            , ongoing   = uncomp
                                                            , completed = comp
                                                            }
                                                 cf:_ -> cf { date      = d
                                                            , ongoing   = cf.ongoing + uncomp
                                                            , completed = cf.completed + comp
                                                            }
                                      cc'     = Map.delete d cc
                                   in (cf':cfs, cc')
                                Just Nothing -> (cfs, cc)
                                Nothing      -> (cfs, cc)

   in reverse $ fst $ foldl' go ([], completedCnt) days


-- | Prepare [CF] so there is no gap between days for a month. The result `CFDMonth` is ready
-- for the frontend to render.
createCFDMonth :: Month -> Map Day (Maybe Todo) -> CFDMonth
createCFDMonth month todos =
  let cfd          = createCumulativeFlow (CFRMonth month) todos -- cfd is already sorted
      (start, end) = cfrToDayRange (CFRMonth month) todos

      go (x:xs) (y@(CF { date }):ys) a
        | x < date = let a' = a { date = x } :: CF
                      in a' : go xs (y:ys) a'
        | x == date = y : go xs ys y
        | otherwise = go (x:xs) ys a -- ignore
      go (x:xs) [] a = let a' = a { date = x } :: CF
                        in a' : go xs [] a'
      go [] _ _ = []
   in CFDMonth $ go [start..end] cfd (CF { date = start, completed = 0, ongoing = 0})


data Model = Model
  { date        :: Text
  , cfd         :: CFDMonth
  , presenceMap :: ByteString
  , firstDay    :: Text
  }


instance ToJSON Model where
  toJSON model = Aeson.object
    [ "date" .= model.date
    , "cfd"  .= model.cfd
    ]


----------------------------------------
-- Helper
----------------------------------------


rangeQuery :: (Ord k, Enum k) => k -> k -> Map k a -> [a]
rangeQuery s1 s2 m =
  let (_, afterS1) = Map.split (pred s1) m
      (inRange, _) = Map.split (succ s2) afterS1
   in Map.elems inRange
