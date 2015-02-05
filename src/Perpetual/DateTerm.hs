
module Perpetual.DateTerm where

import Data.Time.Calendar (fromGregorian,
                           toGregorian,
                           gregorianMonthLength,
                           addGregorianMonthsClip,
                           addDays,diffDays, Day)
import Data.Time.Calendar.Easter (gregorianEaster)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Control.Applicative ((<$>),(<*>),pure)
import qualified Data.Map as M 
import qualified Data.Text as T

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
           deriving (Show,Eq,Ord,Enum)

data MonthTerm = MonthCst Month
               | MonthVar Identifier
               | MonthOf DatePattern
               deriving (Show,Eq,Ord)

data WeekDay = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Show,Eq,Ord,Enum)  

data WeekDayTerm = WeekDayCst WeekDay               
                 | WeekDayOf DatePattern
                 deriving (Show,Eq,Ord)


type MonthDay = Int

data MonthDayTerm = MonthDayCst MonthDay
                  | MonthDayVar Identifier
                  | MonthDayOf DatePattern
                  deriving (Show,Eq,Ord)

type Year = Integer

type Date = (Integer,Int,Int)

data YearTerm = YearCst Year
              | YearVar Identifier
              | YearOf DatePattern
              deriving (Show,Eq,Ord)

type Identifier = T.Text

data DatePattern = DatePattern YearTerm MonthTerm MonthDayTerm  
                 | Easter YearTerm 
                 | DateVar Identifier  
                 | DateDaysAfter DatePattern Integer
                 | DateDaysBefore DatePattern Integer  
                 | DateMonthFirstWeekDay YearTerm MonthTerm WeekDayTerm
                 | DateMonthLastWeekDay YearTerm MonthTerm WeekDayTerm
                 | DateFirstDay YearTerm MonthTerm
                 | DateLastDay YearTerm MonthTerm
                 | DateWeekDayAfter DatePattern WeekDayTerm -- 
                 | DateWeekDayBefore DatePattern WeekDayTerm -- 
                 | DateNthWeekDay DatePattern WeekDayTerm Integer  
                 | DateNYearsSince DatePattern Integer
                 | DateNMonthsSince DatePattern Integer
                 | DateNDaysSince DatePattern Integer  
                 deriving (Show,Eq,Ord)

data DateValue = DateValue {
                     weekDayV :: WeekDay,
                     monthDayV :: MonthDay,
                     monthV :: Month,
                     yearV :: Year,
                     weekNumberV :: Int,
                     dateV :: Date
                 }
                 deriving (Show,Eq,Ord)

type DateContext = M.Map T.Text DateValue

year  (y,_,_) = y
month (_,m,_) = m
day   (_,_,d) = d

weekDay :: Int -> WeekDay
weekDay n = toEnum (n - 1)

fromGregorian' :: (Integer,Int,Int) -> Day
fromGregorian' (y,m,d) = fromGregorian y m d

getDateVar :: T.Text -> DateContext -> Either String DateValue
getDateVar k ctx = maybe (Left $ "undefined date variable '"++ T.unpack k ++"'") Right $ k `M.lookup` ctx

defaultDateIdentifier :: T.Text
defaultDateIdentifier = T.pack "_default"

getDefaultDateValue :: DateContext -> Either String DateValue
getDefaultDateValue = getDateVar defaultDateIdentifier 

evalWeekDayTerm :: WeekDayTerm -> DateContext -> Either String Int
evalWeekDayTerm (WeekDayCst wd) _ = pure $ fromEnum wd + 1
evalWeekDayTerm (WeekDayOf t) ctx = day . toWeekDate . fromGregorian' <$> evalDatePattern t ctx 

evalMonthDayTerm :: MonthDayTerm -> DateContext -> Either String MonthDay
evalMonthDayTerm (MonthDayCst d) _ = pure d
evalMonthDayTerm (MonthDayOf t) ctx = day <$> evalDatePattern t ctx
evalMonthDayTerm (MonthDayVar ident) ctx = monthDayV <$> getDateVar ident ctx

evalMonthTerm :: MonthTerm -> DateContext -> Either String Int
evalMonthTerm (MonthCst m) _ = pure $ fromEnum m + 1 
evalMonthTerm (MonthOf t) ctx = month <$> evalDatePattern t ctx
evalMonthTerm (MonthVar ident) ctx = (+1) . fromEnum . monthV <$> getDateVar ident ctx

evalYearTerm :: YearTerm -> DateContext -> Either String Year
evalYearTerm (YearCst n) _ = return n
evalYearTerm (YearOf t) cxt = year <$> evalDatePattern t cxt
evalYearTerm (YearVar ident) ctx = yearV <$> getDateVar ident ctx

normWeekDay :: Integral n => n -> n
normWeekDay n = (7 + n) `mod` 7

evalDatePattern :: DatePattern -> DateContext -> Either String Date
evalDatePattern (DatePattern yt mt dt) ctx = 
  (,,) <$> evalYearTerm yt ctx <*> evalMonthTerm mt ctx <*> evalMonthDayTerm dt ctx
evalDatePattern (Easter yt) ctx = 
  toGregorian . gregorianEaster <$> evalYearTerm yt ctx
evalDatePattern (DateDaysAfter dt n) ctx = 
  toGregorian . addDays n . fromGregorian' <$>  evalDatePattern dt ctx
evalDatePattern (DateDaysBefore dt n) ctx = 
  toGregorian . addDays (-n) . fromGregorian' <$> evalDatePattern dt ctx
evalDatePattern (DateMonthFirstWeekDay yt mt wdt) ctx =
  evalDatePattern (DateNthWeekDay (DatePattern yt mt (MonthDayCst 1)) wdt 1) ctx
evalDatePattern (DateMonthLastWeekDay yt mt wdt) ctx =
   lastWeekDay <$> evalYearTerm yt ctx <*> evalMonthTerm mt ctx <*> evalWeekDayTerm wdt ctx
  where lastWeekDay y m wd' = (y,m,ld - normWeekDay (wd - wd'))
             where ld = gregorianMonthLength y m
                   d  = fromGregorian y m ld
                   wd = day . toWeekDate $ d 
evalDatePattern (DateFirstDay yt mt) ctx =
  (,,) <$> evalYearTerm yt ctx <*> evalMonthTerm mt ctx <*> return 1
evalDatePattern (DateLastDay yt mt) ctx = do
  (\y m -> (y,m,gregorianMonthLength y m)) <$> evalYearTerm yt ctx <*> evalMonthTerm mt ctx 
evalDatePattern (DateWeekDayAfter dt wdt) ctx = evalDatePattern (DateNthWeekDay dt wdt 1) ctx 
evalDatePattern (DateWeekDayBefore dt wdt) ctx = do
     weekDayUpto <$> evalDatePattern dt ctx <*> evalWeekDayTerm wdt ctx
  where weekDayUpto (y,m,dd) wd = (y,m,dd - normWeekDay (d - wd))
            where d = day . toWeekDate $ fromGregorian y m dd
evalDatePattern (DateNthWeekDay dt wdt n) ctx = do
    nthWeekDay <$> evalDatePattern dt ctx <*> evalWeekDayTerm wdt ctx
  where nthWeekDay (y,m,dd) wd = toGregorian gd'
            where gd = fromGregorian y m dd
                  d = day . toWeekDate $ gd
                  gd' = addDays ((normWeekDay . toInteger $ wd - d) + 7*(n - 1)) gd
evalDatePattern (DateVar ident) ctx = dateV <$> getDateVar ident ctx
evalDatePattern (DateNYearsSince dt n) ctx = do 
        (y,m,d) <- evalDatePattern dt ctx
        (y',_,_) <- dateV <$> getDefaultDateValue ctx
        let y'' = if y <= y' then y' - ((y' - y) `mod` n) else y 
        return (y'',m,d)
evalDatePattern (DateNMonthsSince dt n) ctx = do
        date@(y,m,d) <- evalDatePattern dt ctx
        date'@(y',m',d') <- dateV <$> getDefaultDateValue ctx
        let deltaM = toInteger $ diffMonths date' date
            sinceD = fromGregorian' date
            date'' = toGregorian $ addGregorianMonthsClip (deltaM - (deltaM `mod` n)) sinceD
        return $ if date > date' then date else date''
evalDatePattern (DateNDaysSince dt n) ctx = do
  date@(y,m,d) <- evalDatePattern dt ctx
  date'@(y',m',d') <- dateV <$> getDefaultDateValue ctx
  let days  = fromGregorian' date
      days' = fromGregorian' date'
      deltaD = toInteger $ diffDays days' days
      date'' = toGregorian $ addDays (deltaD - (deltaD `mod` n)) days
  return $ if date > date' then date else date''

diffMonths :: Date -> Date -> Integer
diffMonths d1@(y,m,_) d2@(y',m',_) | d1 <= d2 && y == y' = toInteger (m' - m)
diffMonths d1@(y,m,_) d2@(y',m',_) | d1 < d2 = 13 - toInteger m + (y' - y - 1)*12 + toInteger (m' - 1)
diffMonths d1@(y,m,_) d2@(y',m',_) = -(diffMonths d2 d1) 

matchWithDate :: DatePattern -> Date -> DateContext -> Bool
matchWithDate dt date ctx = either (const False) id $ (== date) <$> evalDatePattern dt ctx     


getDateValueFromDay :: Day -> DateValue
getDateValueFromDay day = DateValue { monthDayV = monthDay,
                                      weekDayV = weekDay,
                                      monthV = month,
                                      yearV = year,
                                      weekNumberV = weekN,
                                      dateV = date } 
                        where date@(year,monthN,monthDay) = toGregorian day
                              month = toEnum (monthN - 1)
                              (_,weekN,weekDayN) = toWeekDate day
                              weekDay = toEnum (weekDayN - 1)

getDateValueFromDate :: Date -> DateValue
getDateValueFromDate date@(year,monthN,monthDay) =
                          DateValue { monthDayV = monthDay,
                                      weekDayV = weekDay,
                                      monthV = month,
                                      yearV = year,
                                      weekNumberV = weekN,
                                      dateV = date }
                   where month = toEnum (monthN - 1)
                         (_,weekN,weekDayN) = toWeekDate . fromGregorian' $ date
                         weekDay = toEnum (weekDayN - 1)

defaultDateContext dt = M.singleton defaultDateIdentifier (getDateValueFromDate dt)
