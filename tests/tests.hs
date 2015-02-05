
{-# LANGUAGE OverloadedStrings #-}

module Test.USARules where

import qualified Data.Text as T
import Perpetual.DateTerm
import Perpetual.ScheduleTerm
import Perpetual.ScheduleParser 

getRules file = do Right (RulesEnv v rs) <- parseFile file
                   return (RulesEnv v (mergeSort rs))

usaRules = getRules "Rules/usRules.txt"

churchRules = getRules "Rules.txt"

usEx dt = matchRules <$> usaRules <*> return dt
        
churchEx dt = matchRules <$> churchRules <*> return dt
