import Text.CSV
import Control.Applicative
import Data.List
import Debug.Trace
import Control.Monad.Trans.State
import System.Random
import Control.Monad

data City = City {latitude :: Maybe Float,
                 longtitude :: Maybe Float,
                 name :: Maybe String,
                 description :: Maybe String
                 } deriving Show
type Lat = Float
type Long = Float

deg2rad x = 2 * pi * x / 360 

distanceMatrix :: [(Lat, Long)] -> [[Float]]
distanceMatrix cities = [[distance (x,y) | x <- cities] | y <- cities ]

distance :: ((Lat, Long), (Lat, Long)) -> Float 
distance ((lat1,long1), (lat2, long2)) = radius_earth*c
                                    where 
                                        lat     = deg2rad (lat1 - lat2)
                                        long    = deg2rad (long1 - long2)
                                        a       = ((sin lat/2)**2) + cos (deg2rad lat1) * cos (deg2rad lat2) * (sin long/2)**2
                                        c       = 2 * ( atan2  (sqrt a) (sqrt (1-a)))
                                        radius_earth = 6378.7




parseCities (Left err) = []
parseCities (Right csv) = 
    foldl(\a record -> if length record == 5 then (parseCity record):a else a) [] csv
    
parseCity record = City { latitude = getFloat $ record !! 0,
                        longtitude = getFloat $ record !! 1,
                        name = getString $ record !! 2,
                        description = getString $ record !! 3}

getFloat :: String -> Maybe Float
getFloat str = case reads str::[(Float,String)] of 
                    [(val, "")] -> Just val
                    _ -> Nothing
              
                
getString :: String -> Maybe String
getString str = if null str then Nothing else Just str


totalDistance :: [[Float]] -> [Int] -> Float -> Float
totalDistance dMatrix (x:y:ys) distance = totalDistance dMatrix (y:ys) distance'
                                            where
                                                distance' = distance + (dMatrix !! x !! y) --get distance from x to y
totalDistance dMatrix (x:[]) distance = distance 



type Route = [Int]
type DistanceMatrix = [[Float]]
type AcceptState = State StdGen Route


swapElem idx1 idx2 state = firstPart ++ elem2 ++ secondPart ++ elem1 ++ lastPart
                                    where
                                        splitted = splitAt idx1 state 
                                        firstPart = case fst splitted of
                                            [] -> []
                                            xs -> init xs
                                        elem1  = case fst splitted of
                                            [] -> []
                                            xs -> [last xs] --swap element

                                        splitted2 = splitAt (idx2 - (length $ fst splitted)) $ snd splitted --split the second part
                                        secondPart = case fst splitted2 of
                                            [] -> []
                                            xs -> init xs
                                        elem2  = case fst splitted2 of
                                            [] -> []
                                            xs -> [last xs]

                                        lastPart = snd $ splitted2

swap :: Int -> Int -> [a] -> [a]
swap idx1 idx2 state | idx1 > idx2 = swapElem idx2 idx1 state 
                     | idx1 < idx2 = swapElem idx1 idx2 state
                     | otherwise = state 


generateCandidate :: Route -> AcceptState -- stdgen route
generateCandidate route = do
                        gen <- get 
                        let (idx1, newGenerator) = randomR (1,(length route)) gen
                        let (idx2, newGenerator') = randomR (1,(length route)) newGenerator
                        put (newGenerator')
                        return $ swap idx1 idx2 route   

accept :: Int -> DistanceMatrix -> Route -> Route -> AcceptState
accept i dM route candidate = do
                        g <- get
                        let distanceRoute = totalDistance dM route 0.0
                        let distanceCandidate = totalDistance dM candidate 0.0
                        let (v, gg) =  random g :: (Float, StdGen)
                        let cn = ((1+ fromIntegral i)**(distanceRoute - distanceCandidate))
                        let route' = if distanceCandidate < distanceRoute then candidate else (if v < cn then   candidate else  route)
                        put (gg)
                        return route' 


step :: Int -> DistanceMatrix -> Route -> AcceptState 
step nr dm route = do 
    newCandidate <- generateCandidate route 
    accept nr dm route newCandidate 

sim :: DistanceMatrix -> Route -> Int -> Int -> AcceptState 
sim dm route count nCount   | count == 0 = return route
                            | otherwise = do 
                            route' <- step nCount dm route
                            sim dm route' (count - 1) (nCount+1)



main :: IO ()
main = do
    let filename = "cities.csv"
    input <- readFile filename
    let csv = parseCSV filename input
    let cities = parseCities csv

    let onlyLongLat = map(\city -> (latitude city, longtitude city)) cities --get lat long tuples from records
    let onlyLongLatClean = sequence $ map(\(xx,yy) -> (\x y -> (x,y)) <$> xx <*> yy) onlyLongLat 
    let dMatrix = fmap distanceMatrix onlyLongLatClean --calculate distance matrix
    let shortest = case dMatrix of
                Just dm -> evalState (sim dm (reverse [0..49]) 30000 0) (mkStdGen 10)

    let shorestDist = case dMatrix of 
            Just dM -> totalDistance dM shortest 0.0

    print shorestDist

    print shortest


