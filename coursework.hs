--
-- MATHFUN
-- UP857256
--

import System.IO
import Data.List
import Text.Printf
import Data.Map (fromListWith, toList)
import Data.Text (pack, splitOn, unpack)
import Data.List (intercalate, genericLength)
import Data.Map (Map, keys, fromList, lookup)
import Data.Maybe

--
-- Types (define Place type here)
--

type PlaceName = String
type DegreesN = Float
type DegreesE = Float
type DailyFigures = [Integer]

data Place = Place {
    placeName :: String,
    degreesN :: Float,
    degreesE :: Float,
    dailyFigures :: [Integer]
} deriving (Show, Eq)

testData :: [Place]
testData = [
    Place "London"       51.5  (-0.1)   [0, 0, 5, 8, 8, 0, 0],
    Place "Cardiff"      51.5  (-3.2)   [12, 8, 15, 0, 0, 0, 2],
    Place "Norwich"      52.6  (1.3)    [0, 6, 5, 0, 0, 0, 3],
    Place "Birmingham"   52.5  (-1.9)   [0, 2, 10, 7, 8, 2, 2],
    Place "Liverpool"    53.4  (-3.0)   [8, 16, 20, 3, 4, 9, 2],
    Place "Hull"         53.8  (-0.3)   [0, 6, 5, 0, 0, 0, 4],
    Place "Newcastle"    55.0  (-1.6)   [0, 0, 8, 3, 6, 7, 5],
    Place "Belfast"      54.6  (-5.9)   [10, 18, 14, 0, 6, 5, 2],
    Place "Glasgow"      55.9  (-4.3)   [7, 5, 3, 0, 6, 5, 0],
    Place "Plymouth"     50.4  (-4.1)   [4, 9, 0, 0, 0, 6, 5],
    Place "Aberdeen"     57.1  (-2.1)   [0, 0, 6, 5, 8, 2, 0],
    Place "Stornoway"    58.2  (-6.4)   [15, 6, 15, 0, 0, 4, 2],
    Place "Lerwick"      60.2  (-1.1)   [8, 10, 5, 5, 0, 0, 3],
    Place "St Helier"    49.2  (-2.1)   [0, 0, 0, 0, 6, 10, 0]
    ]

--
--  Your functional code goes here
--

-- Convert a list of Places to a readable multiline string
placesToString :: [Place] -> String
placesToString [] = []
placesToString ((Place placeName degreesN degreesE dailyFigures):xs) = placeName ++ (rS " " (20-(length placeName))) ++ show degreesN ++ " " ++ show degreesE ++ " " ++ show dailyFigures ++ "\n" ++ placesToString xs

-- Duplicate a given string X amount of times
rS :: String -> Int -> String
rS a b= (concat(replicate b a))

-- Convert a string of place data (from places.txt) into Place data type by splitting it on space and passing it down
convert :: String -> Place
convert placeText = convertPass (map unpack (splitOn (pack(" ")) (pack(placeText))))

-- Convert a list of strings from the convert function, into a data type Place
convertPass :: [String] -> Place
convertPass [a,b,c,d] = (Place a (read b :: Float ) (read c :: Float) (read d :: [Integer]))

-- Convert a list of integers into a list of strings
listIntToListString :: [Integer] -> [String]
listIntToListString listI = map show listI

-- Demo 1

-- join a list of all place names together by using intercalate to join the list on space
getAllPlaceNames :: [Place] -> String
getAllPlaceNames placeList = intercalate " " [placeName | (Place placeName _ _ _) <- placeList]

-- Demo 2

-- Give the average of a list of numeric data
average xs = realToFrac (sum xs) / genericLength xs

-- Find a Place by string input to filter and find the place name
getPlaceFromPlaces :: [Place] -> String -> Place
getPlaceFromPlaces places inputPlace = (filter (\(Place placeName _ _ _) -> placeName == inputPlace) places)!!0

-- Get the dailyFigures for the weather of a Place
getRainfallOfPlace :: Place -> [Integer]
getRainfallOfPlace (Place placeName degreesN degreesE dailyFigures) = dailyFigures

-- Get the average rainfall of a Place given a search term
getAverageRainfallForPlace :: [Place] -> String -> Float
getAverageRainfallForPlace places inputPlace = average (getRainfallOfPlace(getPlaceFromPlaces places inputPlace))

-- Demo 3

-- Fetch all place names and daily figures for a list of Places
getAllPlacesAndRainFall :: [Place] -> [(String,[Integer])]
getAllPlacesAndRainFall places = [(placeName, dailyFigures) | (Place placeName _ _ dailyFigures) <- places]

-- take in a grouped search term and weather data and return a joined list with dynamic spacing
stringListIntegerToString :: (String, [Integer]) -> String
stringListIntegerToString (placeName, dailyFigures) = placeName ++ (rS " " (20-(length placeName)))++ intercalate ",   " (listIntToListString dailyFigures)

-- Demo 4

-- Given a list of Place filter out all Place's that were dry on their second day
findPlacesDry2DaysAgo :: [Place] -> [Place]
findPlacesDry2DaysAgo places = (filter (\(Place placeName _ _ dailyFigures) -> dailyFigures !! 1 == 0) places)

-- Demo 5

-- Take in a grouped Place and new rain data
-- Fetch rainfall of Place
-- Append new data at the front of the list
-- Remove last data in list
-- set it to place parameter and return it
updatePlaceRain :: (Place, Integer) -> Place
updatePlaceRain (place, newRain) = place {dailyFigures = init (newRain : (getRainfallOfPlace place))}

-- Take in a list of Places and a list of new weather data
-- Run it through a zip to join the lists into more managable data
-- Then map it through the above function to update the data within
updateAllPlaceRain :: [Place] -> [Integer] -> [Place]
updateAllPlaceRain places newRainData = map updatePlaceRain (zip places newRainData)

-- Demo 6

-- Add a new Place given the correct params
addPlace :: [Place] -> PlaceName -> DegreesN -> DegreesE -> DailyFigures -> [Place]
addPlace places placeName degreesN degreesE dailyFigures = places ++ [Place placeName degreesN degreesE dailyFigures]

-- Remove a Place from a list of Places by filter on place name
removePlace :: [Place] -> String -> [Place]
removePlace places searchName = (filter (\(Place placeName _ _ _) -> placeName /= searchName) places)

-- Demo 7

-- Get the X/Y (North East) of a Place
getPlaceXY :: Place -> (Float, Float)
getPlaceXY (Place placeName degreesN degreesE dailyFigures) = (degreesN, degreesE)

-- Create Place
createPlace :: PlaceName -> DegreesN -> DegreesE -> DailyFigures -> Place
createPlace placeName degreesN degreesE dailyFigures = Place placeName degreesN degreesE dailyFigures

-- Pythag on two co-ord pairs to find Float distance
distanceBetween2Points :: (Float, Float) -> (Float, Float) -> Float
distanceBetween2Points (aX, aY) (bX, bY) = sqrt ((aX - bX) ^ 2 + (aY - bY) ^ 2)

-- Take in a grouped Place with a destination co-ord pair to check against
-- Return the distance with a Place
placeToPlaceCoordPair :: (Place, (Float, Float)) -> (Float, Place)
placeToPlaceCoordPair ((Place placeName degreesN degreesE dailyFigures), (bX, bY)) = ((distanceBetween2Points (degreesN, degreesE) (bX, bY)), (createPlace placeName degreesN degreesE dailyFigures))

-- Take in a list of places and destination co-ord pair to check against
-- Run map over function to generate list of grouped distance to place
-- Use Zip so map can take in 2 sets of data in 1 value
-- Use replicate to create a list of co-ord pairs equal to length of places so Zip can work
distanceAndPlaceGen :: [Place] -> (Float, Float) -> [(Float, Place)]
distanceAndPlaceGen places (coX, coY) = map placeToPlaceCoordPair (zip places (replicate (length places) (coX, coY)))

-- From a given list of grouped distance and Place
-- Return the closest place with distance to it
-- Use Data.Map.fromList to create a data map on the paired data
-- Then use keys to extract a List of floats
-- Run minimum over said List of floats to find the smallest aka closest distance
-- Run Data.Map.lookup using the minimum value with the original list to find the value of the minimum key
-- Return a grouped Place and distance
minimumDistanceFinder :: [(Float, Place)] -> (Place, Float)
minimumDistanceFinder distancePlacePair = (Data.Maybe.fromJust (Data.Map.lookup (minimum (keys (fromList distancePlacePair))) (fromList distancePlacePair)), minimum (keys (fromList distancePlacePair)))

-- Convert a grouped Place and distance pair to a readable string
placeFloatToString :: (Place, Float) -> String
placeFloatToString ((Place placeName degreesN degreesE dailyFigures), distance) = "The closest place is " ++ placeName ++ ", with a distane of " ++ show distance

--
--  Demo
--

demo :: Int -> IO ()
-- Demo 1, Print all place names from testData
demo 1 = putStrLn (getAllPlaceNames testData)
-- Demo 2, get the average rainfall for Cardfiff and format it to 2dp
demo 2 = printf "%.2f\n" (getAverageRainfallForPlace testData "Cardiff")
-- Demo 3, pretty print a table of place name to weather data via intercalate
demo 3 = putStr ((intercalate "\n" (map stringListIntegerToString (getAllPlacesAndRainFall testData))) ++ "\n")
-- Demo 4, get all places that were dry 2 days ago and print it
demo 4 = putStrLn (getAllPlaceNames (findPlacesDry2DaysAgo testData))
-- Demo 5, Update weather data with given set of ints
demo 5 = putStrLn (placesToString (updateAllPlaceRain testData [0,8,0,0,5,0,0,3,4,2,0,8,0,0]))
-- Demo 6, Add Portsmouth to place list and then remove Plymouth
demo 6 = putStrLn (placesToString (removePlace (addPlace testData "Portsmouth" 50.8 (-1.1) [0,0,3,2,5,2,1]) "Plymouth"))
-- demo 7 = -- display the name of the place closest to 50.9 (N), -1.3 (E) 
--          that was dry yesterday
demo 7 = putStrLn (placeFloatToString (minimumDistanceFinder (distanceAndPlaceGen testData (50.9, -1.3))))
-- demo 8 = -- display the rainfall map


--
-- Screen Utilities (use these to do the rainfall map - note that these do 
-- not work in WinGHCi on Windows, so use GHCi.)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your rainfall map code goes here
--



--
-- Your user interface (and loading/saving) code goes here
--

userInterface :: [Place] -> IO ()
userInterface placeData = do
  putStrLn (rS "*" 15)
  putStrLn "Place Data by UP857256"
  putStrLn (getAllPlaceNames testData)
  putStrLn (rS "*" 15)
  putStrLn ""
  putStrLn "1. - Return a list of the names of all the places"
  putStrLn "2. - Return the average rainfall (as a float) for a place given its name"
  putStrLn "3. - Return all place names and their 7-day rainfall figures as a single string which, when output using putStr, will display the data formatted neatly into eight columns"
  putStrLn "4. - Return a list of the names of places that were totally dry (i.e. had zero rainfall) a given number of days ago"
  putStrLn "5. - Update the data given a list of most recent rainfall figures (one value for each place), removing the oldest rainfall figure for each place"
  putStrLn "6. - Replace a given existing place with a new place"
  putStrLn "7. - Given a location return the closest place that was totally dry yesterday"
  putStrLn "8. - Open Map"
  putStrLn "9. - Exit Program"
  putStrLn ""
  putStrLn (rS "*" 20)

  putStr ("> ")
  input <- getLine
  putStr "\n"
  if input `elem` map (show) [1..9]
    then case input of
      "1" -> putStrLn (getAllPlaceNames placeData)
      "2" -> printf "%.2f" (getAverageRainfallForPlace placeData "Cardiff")
      "3" -> putStr (intercalate "\n" (map stringListIntegerToString (getAllPlacesAndRainFall placeData)))
      "4" -> putStrLn (getAllPlaceNames (findPlacesDry2DaysAgo placeData))
      "5" -> putStrLn (placesToString (updateAllPlaceRain placeData [0,8,0,0,5,0,0,3,4,2,0,8,0,0]))
      "6" -> putStrLn (placesToString (removePlace (addPlace placeData "Portsmouth" 50.8 (-1.1) [0,0,3,2,5,2,1]) "Plymouth"))
      "7" -> putStrLn (placeFloatToString (minimumDistanceFinder (distanceAndPlaceGen placeData (50.9, -1.3))))
      "9" -> return()
  else
    userInterface placeData


main :: IO ()
main = do
    ls <- fmap lines (readFile "places.txt")
    userInterface (map convert ls)
