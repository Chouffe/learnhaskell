module Jammin where

data Fruit =
  Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars =
  Jam Fruit Int
  deriving (Eq, Show, Ord)

data Count =
  Count { count :: Integer
        , jam :: JamJars}

row1 :: Count
row1 = Count 1 (Jam Peach 42)

row2 :: Count
row2 = Count 2 (Jam Apple  0)

row3 :: Count
row3 = Count 3 (Jam Blackberry 100)

allJam :: [Count]
allJam = [row1, row2, row3]

nJars :: [Count] -> Integer
nJars = sum . map count

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = o, lang = l} | o <- allOperatingSystems, l <- allLanguages]
-- allProgrammers = do
--   o <- allOperatingSystems
--   l <- allLanguages
--   return Programmer {os = o, lang = l}
