import AOCInputs

import Data.Maybe

data RawNotes = RawNotes { signal :: [String]
                         , output :: [String] } deriving Show

readInput :: [String] -> [RawNotes]
getRefinedOutNotesTemplate :: RawNotes -> [(String, Maybe Integer)]

refineOutNotesByLengthPattern :: [(String, Maybe Integer)] -> [(String, Maybe Integer)]
countRefinedOutNotes :: [(String, Maybe Integer)] -> Integer

run1 :: IO ()

readInput strs = map parseRawNotes strs
  where parseRawNotes str = let note = splitLine (atString " | ") str
                            in RawNotes { signal=(splitLine atSpace $ head note)
                                        , output=(splitLine atSpace $ last note)}

getRefinedOutNotesTemplate rawNotes = map (\x -> (x, Nothing)) $ output rawNotes

refineOutNotesByLengthPattern notes = map refineOutNote notes
  where refineOutNote (note, _)
          | length note == 2 = (note, Just 1)
          | length note == 4 = (note, Just 4)
          | length note == 3 = (note, Just 7)
          | length note == 7 = (note, Just 8)
          | otherwise = (note, Nothing)

countRefinedOutNotes notes = sum $ map (\(_, x) -> if isJust x then 1 else 0) notes


run1 =
  do ls <- readLines "test1.txt"
     let rawNotess = readInput ls
     let refinedNotess = map refineOutNotesByLengthPattern $ map getRefinedOutNotesTemplate rawNotess
     print rawNotess
     print refinedNotess
     print $ sum $ map countRefinedOutNotes refinedNotess


