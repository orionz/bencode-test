
-- todo - Maybe or Either
-- beware of Head

import System.Environment
import Data.Char
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

decodeInt :: B.ByteString -> (Bencode, B.ByteString)
decodeInt bytes = 
  let num_str = B.takeWhile (/= 'e') bytes
      rest    = B.drop (B.length num_str + 1) bytes
  in (BInt (read (B.unpack num_str) :: Int), rest)

decodeString :: B.ByteString -> (Bencode, B.ByteString)
decodeString bytes = 
  let len    = B.unpack $ B.takeWhile isDigit bytes 
      rest   = B.drop (length len + 1) bytes -- +1 because of the ":" -- TODO check this
      strlen = read len :: Int 
      str    = B.take strlen rest
      rest2  = B.drop strlen rest
  in (BData str, rest2)

decodeList :: [Bencode] -> Char -> B.ByteString -> (Bencode, B.ByteString)
decodeList list 'e' bytes  = (BList $ reverse list, B.drop 1 bytes) 
decodeList list  _  bytes = 
  let (element,rest) = decode' bytes
  in decodeList (element:list) (B.head rest) rest

decodeDict :: Map.Map String Bencode -> B.ByteString -> (Bencode, B.ByteString)
decodeDict map bytes =
  let char             = B.head bytes
      (BData key,rest) = decodeString bytes
      (val,rest2)      = decode' rest
  in if char == 'e' then
    (BMap map, B.drop 1 bytes)
  else
    decodeDict (Map.insert (B.unpack key) val map) rest2

decode' :: B.ByteString -> (Bencode, B.ByteString)
decode' bytes =
  let char = B.head bytes
      rest = B.drop 1 bytes
  in case char of
    'i' -> decodeInt rest
    'l' -> decodeList [] (B.head rest) rest
    'd' -> decodeDict Map.empty rest
    otherwise -> decodeString bytes

decode :: B.ByteString -> Bencode
decode bytes = 
  let (result,_) = decode' bytes
  in result

decodeFiles :: [String] -> IO ()
decodeFiles [] = return ()
decodeFiles (file:files) = do
  body <- B.readFile file
  let result = decode body
  print result
  decodeFiles files

test :: String -> Bencode -> IO ()
test str result = do
  let (d,_) = decode' $ B.pack str
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
