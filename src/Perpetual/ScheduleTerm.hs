module Perpetual.ScheduleTerm where

import Perpetual.DateTerm
import Data.Time.LocalTime
import Data.Time.Calendar (fromGregorian,toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Applicative ((<$>),(<*>))

type Duration = TimeOfDay

type DaySchedule = [IntervalTime] 

type Description = T.Text

data IntervalTime = IntervalTime { beginTime :: TimeOfDay, endTime :: TimeOfDay , intervalDescription :: Description }
                  deriving (Show,Eq,Ord)

data InfoType = Remark -- ^ only for information 
              | Holiday -- ^ The day is a Holiday
              | Description -- ^ Description of a rule  
              deriving (Show,Eq,Enum,Ord)

data TaskType = FixedTask IntervalTime
              | FlexibleTask Duration
              deriving (Show,Eq,Ord)

data WorkableType = NonBusiness -- ^ The day is a non business Day
                  | Special { daySchedule :: DaySchedule } -- ^ The day is a business Day with special schedule
                  | Business { daySchedule :: DaySchedule } -- ^ The day is a Normal labour day
                  deriving (Show,Eq,Ord)

data RuleType = InfoRule InfoType
              | TaskRule TaskType 
              | WorkableRule WorkableType Bool -- ^ Apply to holiday if True
              deriving (Show,Eq,Ord)

data CalendarRule = DayRule  { ruleType :: RuleType,
                               description:: Description,
                               datePattern :: [DatePattern] } -- ^ rules applied  
                  | RangeRule { ruleType :: RuleType,
                                description :: Description,
                                sinceDate :: DatePattern,
                                untilDate ::  DatePattern,
                                weekDays :: [WeekDay] } -- ^ range rule
                  deriving (Show,Eq)  

data CalendarValue = CalendarValue {
                         dt :: DateValue,
                         wt  :: Maybe WorkableType,
                         isHoliday :: Bool,
                         tasks :: [(TaskType,Description)],
                         info  :: [(InfoType,Description)]
                         }
                   deriving (Show,Eq)  

instance Ord CalendarRule where
  compare ct ct' = compare (ruleType ct) (ruleType ct')

data RulesEnv = RulesEnv { variables :: [(T.Text,DatePattern)] , rules :: [CalendarRule]  }
              deriving (Show)  

wellDefinedInterval :: IntervalTime -> Bool
wellDefinedInterval (IntervalTime b e _) = b < e

notOverlapping :: IntervalTime -> IntervalTime -> Bool
notOverlapping (IntervalTime b e _) (IntervalTime b' e' _) = e < b' || e' < b

wellDefinedSchedule :: [IntervalTime] -> Bool
wellDefinedSchedule [] = True
wellDefinedSchedule (x:xs) = wellDefinedInterval x && all (notOverlapping x) xs
                             && wellDefinedSchedule xs 

matchCalendarSchedule :: CalendarRule -> Date -> DateContext -> Bool
matchCalendarSchedule (DayRule _ _ dps) date ctx = or $ map (\dp -> matchWithDate dp date ctx) dps
matchCalendarSchedule (RangeRule _ _ s u wds) date ctx = matchRange s u date ctx && wd `elem` wds
     where wd = toEnum . (\x -> x - 1) . day . toWeekDate . fromGregorian' $ date
           matchRange s u date ctx = either (const False) id ev
           ev = do sd <- evalDatePattern s ctx
                   ud <- evalDatePattern u ctx
                   return $ sd <= date && date <= ud

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y
                      then (x:merge xs (y:ys))
                      else (y:merge (x:xs) ys)

mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort ys) (mergeSort ys')
             where (ys,ys') = splitAt (length xs `div` 2) xs
                   
-- Precondition: The rules must be sorted according the day type
matchRules :: RulesEnv -> Date -> CalendarValue
matchRules (RulesEnv env rules) dt0 = matchRules' rules (CalendarValue dt' Nothing False [] [])
  where dt = (\(y,m,d) -> toGregorian . fromGregorian y m $ d) dt0
        matchRules' [] cv = cv
        matchRules' (r:rs) cv | matchCalendarSchedule r dt ctx =
          case ruleType r of
            (InfoRule infoT) -> matchRules' rs (cv { isHoliday = isHoliday cv || infoT == Holiday,
                                                     info = (infoT,description r):info cv})
            (TaskRule taskT) -> matchRules' rs (cv {tasks = (taskT,description r):tasks cv}) 
            (WorkableRule wtype h) -> if h == isHoliday cv
                                      then cv { wt = (Just wtype), info = inf } 
                                      else matchRules' rs cv
                                where inf = if d == T.empty then info cv else (Description,d):info cv
                                      d = description r
        matchRules' (_:rs) cv = matchRules' rs cv
        ctx0 = defaultDateContext dt
        Right dt' = getDateVar defaultDateIdentifier ctx0
        ctx = foldr (\(v,dp) m -> either (const m) (flip (M.insert v) m . getDateValueFromDate) $ evalDatePattern dp m) ctx0 env 

