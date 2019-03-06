module Schedule(
  Schedule,
  Event,
  newEvent,
  addEvent,
  nextEvent
) where

import Data.List (sortOn)

type Schedule = [Event]
emptySchedule = []

type Time = (Int, Int) -- hours : minutes
type Loc = String -- location
type Desc = String -- description

data Event = Event Time Loc Desc
instance Show Event where
  show (Event (h, m) l d) = "\nEvent at "
    ++ show (h `mod` 12) ++ ":" ++ (show m) ++ (if (h < 12) then " AM" else " PM") ++ ":"
    ++ "\nAt: " ++ l ++ "\nDescription: " ++ d

newEvent :: Int -> Int -> String -> String -> Event
newEvent h m l d = Event (h, m) l d

addEvent :: Event -> Schedule -> Schedule
addEvent e s = sortOn eventTimeInMinutes $ e:s

nextEvent :: Time -> Schedule -> Maybe Event
nextEvent _ [] = Nothing
nextEvent t (event:events) = if (eventTimeInMinutes event < timeToMinutes t)
  then nextEvent t events
  else Just event


eventTimeInMinutes :: Event -> Int
eventTimeInMinutes (Event t _ _) = timeToMinutes t

timeToMinutes :: Time -> Int
timeToMinutes (h, m) = 60 * h + m

main = print . nextEvent (15, 0) $ (addEvent $ newEvent 0 0 "home" "sleep")
  . (addEvent $ newEvent 15 0 "home" "eat lunch")
  . (addEvent $ newEvent 8 0 "home" "eat breakfast")
  $ emptySchedule
