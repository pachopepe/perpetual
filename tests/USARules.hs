{-# LANGUAGE OverloadedStrings #-}

module Test.USARules where

import qualified Data.Text as T
import Perpetual.DateTerm
import Perpetual.ScheduleTerm
import Perpetual.ScheduleParser 

rulesUsaTxt :: [T.Text]
rulesUsaTxt = [
  "Holiday \"New Year's Day\" January 1, *",
  "Holiday \"Birthday of Martin Luther King\" 3rd Monday after January 1, *",
  "Holiday \"Inauguration Day\" every 4 years since January 20, 1937",
  "Holiday \"Washington's Birthday\" 3rd Monday after February 1, *",
  "Holiday \"Armed Forces Day\" 3rd Sunday after May 1, *",
  "Holiday \"Memorial Day\" last Monday in May *",
  "Holiday \"Flag Day\" June 14, *",
  "Holiday \"United States of America's Independence Day\" July 4, *",
  "Holiday \"Labor Day\" first Monday after September 1, *",
  "Holiday \"Columbus Day\" 2nd Monday after October 1, *",
  "Holiday \"Election Day\" Tuesday after November 2, *",
  "Holiday \"Veterans Day\" Monday after November 11, *",
  "Holiday \"Thanksgiving Day\" 4th Thursday after November 1, *",
  "Remark \"Black Friday\" 4th Friday after November 1, *",
  "Holiday \"Christmas Day\" December 25, *",
  "Holyday \"Easter\" Easter *",
  "Remark \"Monday remark\" Monday after every 7 days since January 1, *",
  "Remark \"Last month day\" last day of * *",
  "Non-business \"Vacations\" {Monday .. Sunday} from December 23, * to December 31, *",
  "Non-business \"Vacations\" {Monday .. Sunday} from January 1, * to January 3, *",
  "Non-business \"Weekend\" Saturday after *",
  "Non-business \"Weekend\" Sunday after *",
  "Non-business \"Holy Friday\" Friday before Easter *",
  "Special schedule \"Last Friday\" [8:00 - 12:00] last Friday in * *",
  "Special schedule \"Easter days\" [8:00 - 12:00] {Monday .. Friday} from Thursday before Easter * to Wednesday after Easter *",
  "Business schedule [8:00 - 12:30pm; 2:00pm - 6:00pm] {Monday .. Friday} from January 1, * to December 31, *"
  ]

rulesUsaP = map (parseOnly parseCalendarTerm) rulesUsaTxt
rulesUsa = mergeSort . map (either (error . show) id) $ rulesUsaP


