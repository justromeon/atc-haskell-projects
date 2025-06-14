module Main where

import Options.Applicative (execParser)

import CLI

main :: IO ()
main = execParser commandParserInfo >>= print 