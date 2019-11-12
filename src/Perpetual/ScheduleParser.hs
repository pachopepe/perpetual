{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Perpetual.ScheduleParser where

{-
import Data.Attoparsec.Text as P
import Data.Attoparsec.Combinator 
import Control.Applicative
-}
import Control.Applicative ((<|>),(<$),(<*>),(<$>),(<*),(*>))
-- Parsec
import Text.Parsec.Prim (many,try,lookAhead,Parsec(..),updateState,getState,setState,runParser)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (spaces,digit,lower,char,alphaNum,satisfy,string)
import Text.Parsec.Text hiding (Parser)
import Text.Parsec.Error
-- *Parsec
import Data.List (nub)

import Data.Time.LocalTime

import Data.Char

import Perpetual.DateTerm
import Perpetual.ScheduleTerm

import qualified Data.Text as T
import qualified Data.Text.IO as T


type Parser = Parsec T.Text RulesEnv

-- Parsec
-- type Parser = Parsec T.Text ()
-- *Parsec

parseEnum :: (Enum a,Show a) => Parser a
parseEnum = cases
          <|> do let e = toEnum 0
                 fail ("No enum case ["++ show e ++"..]")
                 return e
          where cases = foldl1 (<|>) $ map
                               (\d -> d <$ (try . accept . T.pack . show $ d))
                               [toEnum 0 .. ]

parseWeekDay :: Parser WeekDay
parseWeekDay = parseEnum

parseMonth :: Parser Month
parseMonth = parseEnum

parseDescription :: Parser T.Text
parseDescription = myToken (char '\"' *> (T.pack <$> many (satisfy (/= '\"'))) <*char '\"')
                 <|> return T.empty
{-
parseDescription = char '\"' *> (P.takeWhile (/= '\"')) <*char '\"'
-}

parseYear :: Parser Year
parseYear = satM (> 1858) integer
          <|> fail "The year must be at least 1859"

parseOrdinal :: Parser Integer
parseOrdinal = try (
             1  <$ (accept "first" <|> accept "1st")
             <|> 2  <$ (accept "second" <|> accept "2nd")
             <|> 3  <$ (accept "third" <|> accept "3rd")
             <|> 4  <$ accept "fourth"
             <|> 5  <$ accept "fifth"
             <|> 6  <$ accept "sixth"
             <|> 7  <$ accept "seventh"
             <|> 8  <$ accept "eighth"
             <|> 9  <$ accept "ninth"
             <|> 10 <$ accept "tenth"
             <|> satM (>3) integer <* accept "th")

satM :: (Monad m,Show b) => (b -> Bool) -> m b -> m b
satM cnd parser = do v <- parser
                     if cnd v
                     then return v
                     else fail ("Unsatisfied condition with value '" ++ show v ++ "'")

parseVar :: Parser T.Text
parseVar =   defaultDateIdentifier <$ accept "*"
         <|> do v <- parseIdentifier
                s <- getState
                case lookup v (variables s) of
                  Nothing -> fail $ "undefined variable '"++ T.unpack v ++"'"
                  (Just _) -> return v

parseIdentifier :: Parser T.Text
parseIdentifier = T.pack <$ spaces <*> (char '$' *> ((:) <$> lower <*> many alphaNum)) <* spaces

parseDatePatternDefList :: Parser [DatePattern]
parseDatePatternDefList = (:) <$> parseDateDef <*> many (accept ";" *> parseDateDef)

parseDateDef :: Parser DatePattern
parseDateDef = do var@(v,t) <- try ((,) <$> parseIdentifier <* accept "=" <*> parseDatePattern)
                  s <- getState
                  case lookup v (variables s) of
                    Nothing -> do setState (s { variables = var : variables s })
                                  return t
                    (Just _) -> fail $ "variable '"++ T.unpack v ++"' already defined"
             <|> parseDatePattern

ignore :: Parser a -> Parser ()
ignore = (() <$)

parseWeekDayTerm :: Parser WeekDayTerm
parseWeekDayTerm = WeekDayOf <$ accept "week" <* accept "day" <* accept "of" <*> parseDatePattern
               <|> WeekDayCst <$> parseWeekDay

parseMonthDayTerm :: Parser MonthDayTerm
parseMonthDayTerm = MonthDayCst <$> satM (\n -> 1 <= n && n <= 31) integer
                <|> MonthDayVar <$> parseVar  
                <|> MonthDayOf  <$ accept "day" <* accept "of " <*> parseDatePattern

parseMonthTerm :: Parser MonthTerm
parseMonthTerm = MonthOf <$ accept "month" <* accept "of" <*> parseDatePattern
             <|> MonthVar <$> parseVar
             <|> MonthCst . toEnum . (+ (-1)) <$> satM (\n -> 1 <= n && n <= 12) integer
             <|> MonthCst <$> parseMonth
             
parseYearTerm :: Parser YearTerm
parseYearTerm =   YearCst <$> parseYear
              <|> YearVar <$> parseVar
              <|> YearOf  <$ accept "year" <* accept "of" <*> parseDatePattern

parseDatePattern :: Parser DatePattern
parseDatePattern = parseAddDatePattern

parseAddDatePattern :: Parser DatePattern
parseAddDatePattern = do dt <- parseSimpleDatePattern  
                         ((DateDaysAfter dt <$ sChar '+' <*> integer
                            <|> DateDaysBefore dt <$ sChar '-' <*> integer) <* accept "days")
                            <|> return dt 

-- Tener en cuenta si hay que implementar modo ingles britanico o otros idioms
parseSimpleDatePattern :: Parser DatePattern
parseSimpleDatePattern =
            try ((\m d y -> DatePattern y m d) <$> parseMonthTerm
                                            <*> parseMonthDayTerm
                                            <*  sChar ','
                                            <*> parseYearTerm)
            <|> try ((\m d y -> DatePattern y m d)
                         <$> parseMonthTerm <* sChar '/'
                         <*> parseMonthDayTerm <* sChar '/'
                         <*> parseYearTerm)
            <|> try (Easter <$ accept "Easter" <*> parseYearTerm)
            <|> DateVar <$> parseVar
            <|> try (do n <- integer <* accept "days"
                        (flip DateDaysAfter n  <$ accept "after" <*> parseDatePattern
                           <|> flip DateDaysBefore n <$ accept "before" <*> parseDatePattern))
            <|> try (do accept "first"
                        (flip DateFirstDay <$ accept "day" <* accept "of" <*> parseMonthTerm <*> parseYearTerm
                          <|> (\w m y -> DateMonthFirstWeekDay y m w) <$> parseWeekDayTerm <* accept "in"
                                                                      <*> parseMonthTerm
                                                                      <*> parseYearTerm))
            <|> do  accept "last"
                    (flip DateLastDay <$ accept "day" <* accept "of" <*> parseMonthTerm <*> parseYearTerm
                      <|> (\w m y -> DateMonthLastWeekDay y m w) <$> parseWeekDayTerm <* accept "in"
                                                                 <*> parseMonthTerm
                                                                 <*> parseYearTerm)   
            <|> do wd <- parseWeekDayTerm
                   (flip DateWeekDayAfter wd <$ accept "after" <*> parseDatePattern
                     <|> flip DateWeekDayBefore wd <$ accept "before" <*> parseDatePattern)
            <|> (\nth wd dt -> DateNthWeekDay dt wd nth) <$> parseOrdinal
                                                         <*> parseWeekDayTerm <* accept "after"
                                                         <*> parseDatePattern
            <|> do accept "every"                                         
                   (flip DateNYearsSince 1 <$ accept "year" <* accept "since" <*> parseDatePattern 
                     <|> flip DateNMonthsSince 1 <$ accept "month" <* accept "since" <*> parseDatePattern 
                     <|> flip DateNDaysSince 1 <$ accept "day" <* accept "since" <*> parseDatePattern
                     <|> do n <- integer
                            (flip DateNYearsSince n <$ accept "years" <* accept "since" <*> parseDatePattern 
                              <|> flip DateNMonthsSince n <$ accept "months" <* accept "since" <*> parseDatePattern 
                              <|> flip DateNDaysSince n <$ accept "days" <* accept "since" <*> parseDatePattern)
                     )
            <|> sChar '(' *> parseAddDatePattern <* sChar ')' 

accept :: T.Text -> Parser T.Text
accept p = myToken (T.pack <$> (string . T.unpack) p) 

myToken :: Parser a -> Parser a
-- Parsec
myToken p = spaces *> p <* spaces
-- *Parsec
{-
-- Attoparsec
myToken p = skipSpace *> p <* skipSpace
-}

integer :: (Read a,Integral a) => Parser a
-- Parsec
integer = read <$> myToken (many1 digit)
-- *Parsec
{-
-- Attoparsec
integer = myToken decimal
-}

sChar :: Char -> Parser Char
sChar = myToken . char

parseHour :: Parser TimeOfDay
parseHour = do h <- parseH
               char ':'
               m <- parseM
               nm <- normOrMil
               verifyTime h m nm
          where  verifyTime h m Nothing = return $ TimeOfDay h m 0
                 verifyTime h m (Just h') = if h <= 12
                                            then return $ TimeOfDay ((h `mod` 12)+h') m 0
                                            else fail "Hour must be between 0 and 11"
                 parseH = satM (\h -> 0 <= h && h < 24) integer
                       <|> fail "The hour must be bettwen 0 and 23"
                 parseM = satM (\m -> 0 <= m && m < 60) integer
                        <|> fail "The minutes must be bettwen 0 and 59"
                 normOrMil =   return (Just 0) <* ( accept "am" <|>  accept "a.m." <|>  accept "AM" <|>  accept "A.M.")
                           <|> return (Just 12) <* ( accept "pm" <|>  accept "p.m." <|>  accept "PM" <|>  accept "P.M.")
                           <|> return Nothing

parseInterval :: Parser IntervalTime
parseInterval = IntervalTime <$> parseHour <* accept "-" <*> parseHour <*> parseDescription

parseList :: T.Text -> Parser a -> T.Text -> T.Text -> Parser [a]
parseList bg p sep end = accept bg *> parseListTerms p sep <* accept  end
                       where parseListTerms p sep = (:) <$> p <*> many (accept sep *> p)

parseListInt :: (Enum a) => T.Text -> Parser a -> T.Text -> T.Text -> Parser [a]
parseListInt bg p sep end = accept bg *> parseListTerms p sep <* accept  end
                       where parseListTerms p sep = do e <- p
                                                       do accept ".."
                                                          e' <- p
                                                          return [e .. e']
                                                         <|> (e:) <$> many (accept sep *> p)

optParseDaySchedule :: Parser DaySchedule
optParseDaySchedule = lookAhead (accept "[") *> parseDaySchedule
                    <|> return []

parseDaySchedule :: Parser DaySchedule
parseDaySchedule = do ds <- parseList "[" parseInterval ";" "]"
                      if wellDefinedSchedule ds
                      then return ds
                      else fail $ "Not well defined interval: "++show ds

parseWeekDayList :: Parser [WeekDay]
parseWeekDayList = nub <$> parseListInt "{" parseWeekDay "," "}"

parseInfoType :: Parser InfoType
parseInfoType =  parseEnum

parseWorkableType :: Parser (WorkableType,Bool,Description)
parseWorkableType = (NonBusiness,,) <$ accept "Non-business" <*> parseApplyHoliday <*> parseDescription 
                  <|> (\h des sc -> (Special sc,h,des)) <$ accept "Special" <* accept "schedule"
                                                    <*> parseApplyHoliday
                                                    <*> parseDescription <*> parseDaySchedule
                  <|> (\h des sc -> (Business sc,h,des))  <$ accept "Business" <* accept "schedule"
                                                      <*> parseApplyHoliday
                                                      <*> parseDescription <*> parseDaySchedule

parseDateVariable :: Parser DatePattern
parseDateVariable = accept "var" *> parseDateDef 

parseApplyHoliday :: Parser Bool
parseApplyHoliday = True <$ accept "for" <* accept "Holiday"
                  <|> return False

parseCalendarRule :: Parser CalendarRule
parseCalendarRule = do (ruleT,desc) <- ((,) <$> (InfoRule <$> parseInfoType) <*> parseDescription)
                                            <|> (\(x,h,d) -> (WorkableRule x h,d)) <$> parseWorkableType 
                            -- Could pass dayT argument
                       (DayRule ruleT desc <$> parseDatePatternDefList)
                              <|> ((\wds from to -> RangeRule ruleT desc from to wds)
                                   <$> parseWeekDayList
                                   <* accept "from" <*> parseDatePattern
                                   <* accept "to" <*> parseDatePattern)

parseCalendarTerm :: Parser ()
parseCalendarTerm = do r <- parseCalendarRule
                       updateState (\env -> env { rules = r : rules env })
                   <|> parseDateVariable *> return ()


parseEnv :: Parser RulesEnv
parseEnv = do many parseCalendarTerm
              getState
              
parseOnly:: Parser a -> T.Text -> Either ParseError a
parseOnly p s = runParser p (RulesEnv [] []) "unknown" s 

parseFile :: FilePath -> IO (Either ParseError RulesEnv)
parseFile path = parseOnly (parseEnv) <$> T.readFile path


{-
parseOnly (parseSimpleDatePattern) $ T.pack "January 12, 1994"
parseOnly (parseSimpleDatePattern) $ T.pack "12/15/1994"
parseOnly (parseSimpleDatePattern) $ T.pack "* 15, *"
parseOnly (parseSimpleDatePattern) $ T.pack "*/15/*"
parseOnly (parseSimpleDatePattern) $ T.pack "easter *"
parseOnly (parseSimpleDatePattern) $ T.pack "Monday after January 3, 1994"
parseOnly (parseSimpleDatePattern) $ T.pack "Monday after January *, *"
parseOnly (parseSimpleDatePattern) $ T.pack "10 days after easter 1994"
parseOnly (parseSimpleDatePattern) $ T.pack "first day of January *"
parseOnly (parseSimpleDatePattern) $ T.pack "first Monday in January *"
parseOnly (parseSimpleDatePattern) $ T.pack "last day of * *"
parseOnly (parseSimpleDatePattern) $ T.pack "last Monday in January *"
parseOnly (parseSimpleDatePattern) $ T.pack "third Monday after 01/1/1994"
parseOnly (parseSimpleDatePattern) $ T.pack "4th Monday after 01/1/1994"
parseOnly (parseSimpleDatePattern) $ T.pack "every year since 1/1/1994"
parseOnly (parseSimpleDatePattern) $ T.pack "every 4 years since 1/1/1994"
parseOnly (parseSimpleDatePattern) $ T.pack "every month since 1/1/1994"
parseOnly (parseSimpleDatePattern) $ T.pack "every 2 months since 1/1/1994"
parseOnly (parseSimpleDatePattern) $ T.pack "every day since 1/1/1994"
parseOnly (parseSimpleDatePattern) $ T.pack "every 30 days since 1/1/1994"
parseOnly (parseAddDatePattern) $ T.pack "January 12, 1994 - 3 days"

parseOnly (parseCalendarRule) $ T.pack "Holiday January 1, * 'Maria mother of God'"

parseOnly (parseRangeRule) $ T.pack "schedule since January 1, 2014 to January 20, 2014 {Monday .. Friday} [8:00 - 12:00pm]" 

parseOnly (parseRangeTerm) $ T.pack "rest period since January 1, * to January 20, *"
parseOnly (parseCalendarRule) $ T.pack "schedule since January 20, * to June 20, 2014 {Monday .. Friday} [8:00 - 12:00pm] 'First period'" 

parseOnly (parseCalendarRule) $ T.pack "Holiday every 4 years since January 20, 1937 'Inaguration Day'" 
parseOnly (parseCalendarRule) $ T.pack "Holiday every 4 years since 1/20/1937 'inaguration day'"
-}

{-
Examples MatchRule:

matchRules  <$> (either (const $ error "Error") id <$> (parseFile "Rules/churchRules.txt")) <*> return (2015,7,19)
smatchRules  <$> (either (const $ error "Error") id <$> (parseFile "Rules/churchRules.txt")) <*> return (2015,7,19)

-}        
