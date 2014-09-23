{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad

import Hledger

main :: IO ()
main = do
  ledger <- defaultJournal
  let accts = journalAccountNames ledger
  forM_ accts putStrLn
