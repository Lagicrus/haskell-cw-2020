--
-- MATHFUN
-- UP857256
--

import System.IO
import Data.List
import Text.Printf
import Data.Map (fromListWith, toList)
import Data.Text (pack, splitOn, unpack)
import Data.List (intercalate)

--
-- Types (define Place type here)
--

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
    Place "Norwich"      52.6  (1.3)   [0, 6, 5, 0, 0, 0, 3],
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

placesToString :: [Place] -> String
placesToString [] = []
-- placesToString ((Place placeName degreesN degreesE dailyFigures):xs) = printf "Place Name: %s\nDegrees N: %s\nDegrees E: %s\nDaily Figures: %s\n" placeName degreesN degreesE dailyFigures ++ placesToString xs
-- placesToString [] = map printf "Place Name: %s\nDegrees N: %s\nDegrees E: %s\nDaily Figures: %s\n" placeName degreesN degreesE dailyFigures

rS :: String -> Int -> String
rS a b= (concat(replicate b a))

convert :: String -> Place
convert placeText = convertPass (map unpack (splitOn (pack(" ")) (pack(placeText))))

convertPass :: [String] -> Place
convertPass [a,b,c,d] = (Place a (read b :: Float ) (read c :: Float) (read d :: [Integer]))

getAllPlaceNames :: [Place] -> String
getAllPlaceNames placeList = intercalate " " [placeName | (Place placeName _ _ _) <- placeList]


getPlaceFromPlaces:: [Place] -> [String] -> [Place]
getPlaceFromPlaces places inputPlace place = (filter (\(Album place _ _ _) -> place == inputPlace) places)!!0

--
--  Demo
--

demo :: Int -> IO ()
-- demo 1 = putStrLn (placesToString testData)
demo 1 = putStrLn (getAllPlaceNames testData)
-- demo 2 = -- display, to two decimal places, the average rainfall in Cardiff
-- demo 3 = putStrLn (placesToString testData)
-- demo 4 = -- display the names of all places that were dry two days ago
-- demo 5 = -- update the data with most recent rainfall 
--          [0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and remove oldest rainfall figures)
-- demo 6 = -- replace "Plymouth" with "Portsmouth" which has 
--          location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1
-- demo 7 = -- display the name of the place closest to 50.9 (N), -1.3 (E) 
--          that was dry yesterday
-- demo 8 = -- display the rainfall map


--
-- Screen Utilities (use these to do the rainfall map - note that these do 
-- not work in WinGHCi on Windows, so use GHCi.)
--

{-
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
 
-}

userInterface :: [Place] -> IO ()
userInterface placeData = do
  putStrLn (rS "*" 15)
  putStrLn "Place Data by UP857256"
  putStrLn (rS "*" 15)
  putStrLn ""
  putStrLn "1. - Return a list of the names of all the places"
  putStrLn "2. - Top 10 Albums in descending order"
  putStrLn "3. - Give all Albums released between 2 given years (inclusive)"
  putStrLn "4. - Give all Albums that start with a given prefix"
  putStrLn "5. - Give the total sales of a given artist"
  putStrLn "6. - Give a list of artist names to number of albums in top 50"
  putStrLn "7. - Remove the 50th album and replace it"
  putStrLn "8. - Increasing the sales figures of a given album"
  putStrLn "9. - Exit Program"
  putStrLn ""
  putStrLn (rS "*" 20)

  putStr ("> ")
  input <- getLine
  if input `elem` map (show) [1..9]
    then case input of
      "1" -> putStrLn (getAllPlaceNames testData)
    --   "2" -> putStrLn (placesToString (top10 placeData))
    --   "3" -> putStrLn (placesToString (filterReleaseYear 2000 2008 placeData))
    --   "4" -> putStrLn (placesToString (albumStartsWithFilter "Th" placeData))
    --   "5" -> putStrLn (show (sum(totalSales "Queen" placeData)))
    --   "6" -> putStrLn (occuranceToString(occurance_counter (albumReduction(placeData)) 50))
    --   "7" -> putStrLn (placesToString (updateLastEntry placeData (Album "Progress" "Take That" 2010 2700000)))
    --   "8" -> putStrLn (placesToString (updateCertainEntry placeData "21"))
      "9" -> return()
  else
    userInterface placeData


main :: IO ()
main = do
    -- putStrLn (splitOn "," (lines contents))
    ls <- fmap lines (readFile "places.txt")
    print ls
    userInterface (map convert ls)

-- main :: IO ()
-- main = do
--     putStrLn("========================================================")
--     putStrLn("Rainfall places")
--     loadedFile <- readFile "places.txt"
--     let places convertToPlace (lines loadedFile))
--     putStrLn("\nSuccesfully loaded "++ show(length places) ++" albums!")
--     putStrLn("========================================================\n")
--     userInterface()
    
    
-- convertToPlace :: [String] -> Place
-- convertToPlace [title,artist,year,sales] = (Place placeName degreesN degreesE (read dailyFigures :: Int))


-- ((Place placeName degreesN degreesE dailyFigures):xs)