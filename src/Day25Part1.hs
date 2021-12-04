cardPubKey = 11349501 :: Int
doorPubKey = 5107328 :: Int

denominator = 20201227 :: Int
subjectNum = 7 :: Int
startVal = 1 :: Int

transform :: Int -> Int -> Int
transform subNum val = (val * subNum) `rem` denominator

transformN :: Int -> Int -> Int -> Int
transformN 0     subNum val = val
transformN steps subNum val = transformN (steps - 1) subNum (transform subNum val)

getLoopSizes :: Int -> Int -> (Int, Int) -> (Maybe Int, Maybe Int)
getLoopSizes val loopSize (cardPubKey, doorPubKey)
  | val == cardPubKey = (Just loopSize, Nothing)
  | val == doorPubKey = (Nothing, Just loopSize)
  | otherwise = getLoopSizes (transform val subjectNum) (loopSize + 1) (cardPubKey, doorPubKey)

getEncryptionKey :: (Int, Int) -> Int
getEncryptionKey (cardPubKey, doorPubKey) =
  let (cardLoopSize, doorLoopSize) = getLoopSizes subjectNum startVal (cardPubKey, doorPubKey)
  in  case cardLoopSize of
        Just card -> transformN card doorPubKey startVal
        Nothing   -> case doorLoopSize of
          Just door -> transformN door cardPubKey startVal

main = print $ getEncryptionKey (cardPubKey, doorPubKey)

