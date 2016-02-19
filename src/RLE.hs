{-
LOSSLESS DATA COMPRESSION
Run-length encoding and decoding
-}

import Data.List
import Text.Read

{-
=========================================================
Run-length encoding
=========================================================
-}

-- Takes in a String, like "aaabffffkk"
-- Returns a String, like "a3b1f4k2"
rleEncode :: String -> String
rleEncode = (concat . concat) . buildEncodedList


-- HELPER
-- Takes in a String, like "aaabffffkk" 
-- Returns a nested list of Strings, 
-- like [["a","3"],["b","1"],["f","4"],["k","2"]]
buildEncodedList :: String -> [[String]]
buildEncodedList = 
    map (\x -> [[head x] ++ show (length x)]) . group


{-
=========================================================
Run-length decoding
=========================================================
-}

-- Takes in a String, like "a3b1f4k2"
-- Returns a String, like "aaabffffkk"
rleDecode :: String -> String
rleDecode str = 
    concat $
        concatMap (\(a, b) -> replicate b a) 
            (convert $ unencode (group str) "" "" [])


-- Returns list like: [("a","3"),("b","1"),("f","14"),("k","2")]
unencode :: [String] -> String -> String -> 
                [(String, String)] -> [(String, String)]
unencode [] [] [] built = built
unencode [] strAcc numAcc built = built ++ [(strAcc, numAcc)]
unencode (x:xs) strAcc numAcc built =
    if isInteger x
        then
            if strAcc == ""
                then
                    unencode xs "" "" built 
                else 
                    unencode xs strAcc (numAcc++x) built
        else 
            if strAcc == ""
                then
                    unencode xs x "" built
                else 
                    unencode xs x "" (built ++ [(strAcc, numAcc)])


-- checks if a String is an integer
isInteger :: String -> Bool
isInteger s = case readMaybe s :: Maybe Int of
    Just int -> True
    Nothing -> False


-- Returns list like: [("a",3),("b",1),("f",4),("k",2)]
convert :: [(String, String)] -> [(String, Int)]
convert lst = map (\(a, b) -> (a, read b :: Int)) lst



{-
USAGE:
Uncomment this section to try the RLE Encoder/Decoder for yourself

main :: IO ()
main = do
    putStr "String to encode: "
    strToEncode <- getLine
    let encoded = rleEncode strToEncode
        decoded = rleDecode encoded
    putStrLn $ 
        "Encoded: " ++ show encoded ++ "\nDecoded: " ++ show decoded
-}

