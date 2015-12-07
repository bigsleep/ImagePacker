{-# LANGUAGE DataKinds, OverloadedStrings, TemplateHaskell #-}
module Main where

import ImagePackerCommand (runImagePackerCommand)

main :: IO ()
main = runImagePackerCommand
