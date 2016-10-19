module Phone where

import Data.Char ( isUpper
                 , toLower)
import Data.List (find
                 , groupBy
                 , sortBy)

data PhoneKey =
   Zero
 | One
 | Two
 | Three
 | Four
 | Five
 | Six
 | Seven
 | Eight
 | Nine
 | Star
 | Pound
 deriving (Eq)

instance Show PhoneKey where
  show Zero  = "0"
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Star  = "*"
  show Pound = "#"

data DaPhone =
  DaPhone [(PhoneKey, String)]
  deriving (Eq, Show)

daPhone :: DaPhone
daPhone = DaPhone [ (Zero, " ")
                  , (One, "")
                  , (Two, "abc")
                  , (Three, "def")
                  , (Four, "ghi")
                  , (Five, "jkl")
                  , (Six, "mno")
                  , (Seven, "pqrs")
                  , (Eight, "tuv")
                  , (Nine, "wxyz")
                  , (Star, "^")
                  , (Pound, ".,")
                  ]

type Digit = Char

type Presses = Int

reverseLookUp :: DaPhone -> Char ->Maybe (PhoneKey, String)
reverseLookUp (DaPhone dp) c =
  find scanEntry dp
  where c' = toLower c
        scanEntry (_, s) = case find (==c') s of
                           Nothing -> False
                           _  -> True

presses :: DaPhone -> Char -> Maybe Presses
presses dp c = do
  (_, vs) <- reverseLookUp dp c
  (_, p) <-find (\(v, _) -> v == c) (zip vs [1..])
  return p

reverseTaps :: DaPhone -> Char -> [(PhoneKey, Presses)]
reverseTaps dp c
  | isUpper c = (Star, 1) : reverseTaps dp (toLower c)
  | otherwise = let pk = do
                    (k, _) <- reverseLookUp dp c
                    p <- presses dp c
                    return (k, p)
                 in case pk of
                      Nothing -> []
                      Just e -> [e]

cellPhonesDead :: DaPhone -> String -> [(PhoneKey, Presses)]
cellPhonesDead dp = concatMap (reverseTaps dp)

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"]

fingerTaps :: [(PhoneKey, Presses)] -> Int
fingerTaps = sum . map snd

mostPopular :: String -> String
mostPopular s = show $ fst $ head $ last sortedVals
  where grouped = groupBy (\(_, a) (_, b) -> a == b) $ concatMap (reverseTaps daPhone) s
        sortedVals = sortBy (\a b -> compare (length a) (length b)) grouped
