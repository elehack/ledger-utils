{-# LANGUAGE DeriveDataTypeable #-}
import Data.Time
import Data.List
import Data.Ord
import Data.Monoid
import Control.Applicative
import Control.Monad
import System.Environment
import System.Locale (defaultTimeLocale)

import System.Console.CmdArgs
import System.Console.CmdArgs.Verbosity
import qualified System.Console.Rainbow as Term

import Text.CSV
import Hledger
import Text.Printf

data Invocation = Invocation { invQuery :: String
                             , invFuzz :: Integer
                             , invMakeLedger :: Bool
                             , invFile :: FilePath }
                deriving (Show, Data, Typeable)

invocation = Invocation{ invMakeLedger = def &= explicit
                                         &= name "make-ledger"
                                         &= name "L"
                                         &= help "Output ledger templates"
                       , invFuzz = def &= explicit
                                   &= name "fuzz"
                                   &= name "z"
                                   &= help "search window of Z days"
                       , invQuery = def &= argPos 0 &= typ "QUERY"
                       , invFile = def &= argPos 1 &= typ "FILE" }
             &= program "ledger-reconcile"
             &= summary "Reconcile ledger with a CSV file"
             &= verbosity

data BankRecord = BankRecord { rDay :: Day,
                               rAmount :: Amount,
                               rDescr :: String }

data ResultEntry = Matched Posting BankRecord
                 | LedgerOnly Posting
                 | BankOnly BankRecord

resultDate (Matched p b) = min (postingDate p) (rDay b)
resultDate (LedgerOnly p) = postingDate p
resultDate (BankOnly b) = rDay b

readBankRecords :: FilePath -> IO [BankRecord]
readBankRecords fn = do
  lines <- parseCSVFromFile fn >>= failOnError
  return $ sortBy (comparing rDay)
    [ BankRecord (date dt) (usd $ read amt) desc
    | (dt:amt:_:_:desc:_) <- lines ]
  where
    failOnError (Left err) = fail $ show err
    failOnError (Right csv) = return csv

    date :: String -> Day
    date = readTime defaultTimeLocale "%m/%d/%Y"

checkingLedger :: String -> Day -> IO [Posting]
checkingLedger acct start = fmap filter defaultJournal
  where
    filter = journalPostings . filterJournalPostings query
    query = And [Acct acct,
                 Date (DateSpan (Just start) Nothing)]

reconcile :: Integer
          -> [Posting]
          -> [BankRecord]
          -> [ResultEntry]
reconcile fuzz ledger bank = sortBy (comparing resultDate) $ scan [] ledger bank
  where
    scan :: [ResultEntry] -> [Posting] -> [BankRecord] -> [ResultEntry]
    scan results [] bs = map BankOnly bs ++ results
    scan results ls [] = map LedgerOnly ls ++ results
    scan results (l:ls) bs =
      case rmMatching [] l bs of
        Just (b,nbs) -> scan (Matched l b : results) ls nbs
        Nothing -> scan (LedgerOnly l : results) ls bs
    rmMatching acc l [] = Nothing
    rmMatching acc l (b:bs)
      | l `matches` b = Just (b, reverse acc ++ bs)
      | otherwise     = rmMatching (b:acc) l bs

    matches l b = pdollars l == rAmount b && datesMatch fuzz l b

    pdollars = dollars . amounts . pamount
    dollars = maybe (usd 0) id
              . find ((==) "$" . acommodity)

    datesMatch 0 _ _ = True
    datesMatch z l b = abs (diffDays (rDay b) (postingDate l)) <= z

pDescr :: Posting -> String
pDescr p | pcomment p == "\n" = maybe "" tdescription $ ptransaction p
         | otherwise = pcomment p

printResult :: ResultEntry -> IO ()
printResult x = maybePrint x $ do
  Term.putChunkLn $ (Term.fromString $ display x) <> color x
  where
    display :: ResultEntry -> String
    display x = printf "%s %s %12s  %s"
                (code x)
                (show $ date x)
                (amount x)
                (descr x)


    maybePrint (Matched p b) | postingCleared p = whenLoud
                             | otherwise = id
    maybePrint _ = id

    color :: ResultEntry -> Term.Chunk
    color (Matched p b) | postingCleared p = Term.f_green
                        | otherwise = Term.bold
    color (LedgerOnly p) | postingCleared p = Term.f_red <> Term.bold
                         | otherwise = Term.f_white
    color (BankOnly b) = Term.f_yellow
    
    date (Matched p b) = postingDate p
    date (LedgerOnly p) = postingDate p
    date (BankOnly b) = rDay b

    code (Matched p b) | postingCleared p = "M"
                       | otherwise = "C"
    code (LedgerOnly p) | postingCleared p = "X"
                        | otherwise = "L"
    code (BankOnly b) = "B"

    amount (Matched p b) = showMixedAmount $ pamount p
    amount (LedgerOnly p) = showMixedAmount $ pamount p
    amount (BankOnly b) = showAmount $ rAmount b

    descr (Matched p b) = pDescr p
    descr (LedgerOnly p) = pDescr p
    descr (BankOnly b) = rDescr b

printLedgerEntry :: String -> ResultEntry -> IO ()
printLedgerEntry query (BankOnly b) = do
  putStr $ formatTime defaultTimeLocale "%Y/%m/%d" $ rDay b
  putStr " "
  putStrLn $ rDescr b
  putStr "    "
  putStr query
  putStr "               "
  putStrLn $ showAmount $ rAmount b
  putStrLn "    UNBAL"
  putStrLn ""
printLedgerEntry query _ = return ()

-- filter out false positive due to fuzz
filterEarly cutoff results = reverse $ dropWhile bogus $ reverse results
  where
    bogus (LedgerOnly p) = postingDate p < cutoff
    bogus _ = False

main :: IO ()
main = do
  options <- cmdArgs invocation
  let fuzz = invFuzz options
  let q = invQuery options
  let fn = invFile options
  records <- readBankRecords fn
  let minDate = minimum $ map rDay records
  let fuzzDate = addDays (0 - fuzz) minDate
  whenLoud $ do
    putStrLn ("Loading transactions since " ++ show fuzzDate)
  ledger <- checkingLedger q fuzzDate
  let results = filterEarly minDate $ reconcile fuzz ledger records
  let printer = if invMakeLedger options then printLedgerEntry q else printResult
  forM_ results printer

