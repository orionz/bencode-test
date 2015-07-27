
-- todo - Maybe or Either
-- beware of Head

import System.Environment
import Data.Char
import Text.Read
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as B

data Bencode =
    BInt Int
  | BData B.ByteString
  | BList [Bencode]
  | BMap (Map.Map String Bencode)
  deriving (Ord,Eq)

instance Show Bencode where
    show (BData s) =  "bdata:" <> show (B.length s)
    show (BInt a)  = show a
    show (BList a) = show a
    show (BMap a)  = show a

decodeInt :: B.ByteString -> Maybe (Bencode, B.ByteString)
decodeInt bytes =
  case readMaybe (B.unpack num_str) :: Maybe Int of
       Just num -> Just $ (BInt num, rest)
       Nothing  -> Nothing
  where num_str = B.takeWhile (/= 'e') bytes
        rest    = B.drop (B.length num_str + 1) bytes

decodeString :: B.ByteString -> Maybe (Bencode, B.ByteString)
decodeString bytes =
  case readMaybe len :: Maybe Int  of
       Nothing     -> Nothing
       Just strlen -> Just (BData $ B.take strlen rest, B.drop strlen rest)
  where len  = B.unpack $ B.takeWhile isDigit bytes
        rest = B.drop (length len + 1) bytes -- +1 because of the ":" -- TODO check this

decodeList :: [Bencode] -> Char -> B.ByteString -> Maybe (Bencode, B.ByteString)
decodeList list 'e' bytes = Just (BList $ reverse list, B.drop 1 bytes)
decodeList list  _  bytes =
  case decode' bytes of
    Nothing             -> Nothing
    Just (element,rest) -> decodeList (element:list) (B.head rest) rest

decodeDict :: Map.Map String Bencode -> B.ByteString -> Maybe (Bencode, B.ByteString)
decodeDict map bytes =
  case B.uncons bytes of
    Nothing       -> Nothing
    Just ('e', _) -> Just (BMap map, B.drop 1 bytes)
    otherwise     -> case decodeString bytes of
      Nothing -> Nothing
      Just (BData key, rest) -> case decode' rest of
        Nothing -> Nothing
        Just (val, rest2) -> decodeDict (Map.insert (B.unpack key) val map) rest2

decode' :: B.ByteString -> Maybe (Bencode, B.ByteString)
decode' bytes = case B.uncons bytes of
    Nothing -> Nothing
    Just ('i', rest) -> decodeInt rest
    Just ('l', rest) -> decodeList [] (B.head rest) rest
    Just ('d', rest) -> decodeDict Map.empty rest
    otherwise        -> decodeString bytes

decode :: B.ByteString -> Maybe Bencode
decode bytes = case decode' bytes of
    Just (result,_) -> Just result
    Nothing         -> Nothing

decodeFiles :: [String] -> IO ()
decodeFiles [] = return ()
decodeFiles (file:files) = do
  body <- B.readFile file
  let Just result = decode body
  print result
  decodeFiles files

test :: String -> Bencode -> IO ()
test str result = do
  let Just (d,_) = decode' $ B.pack str
  print
    (if d == result then
      "Test passed: " <> show d
    else
      "Test failed: " <> show str <> " -> " <> show result <> "vs" <> show d)

tests :: IO ()
tests = do
  test "i-100e" (BInt (-100))
  test "3:xxx" (BData $ B.pack "xxx")
  test "10:0123456789" (BData $ B.pack "0123456789")
  test "l3:aaa4:bbbbe" (BList [ BData $ B.pack "aaa", BData $ B.pack "bbbb" ])
  test "d3:aaa4:bbbb2:xxi99ee" (BMap $ Map.fromList [("aaa",BData $ B.pack "bbbb"),("xx",BInt 99)])

main = do
  tests
  args <- getArgs
  decodeFiles args
