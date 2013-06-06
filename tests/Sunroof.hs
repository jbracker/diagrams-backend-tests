module Main where

import Text.Html as H
import Diagrams.Prelude hiding (D)
import Diagrams.Backend.Sunroof
import Diagrams.Tests
import Diagrams.Test.Sunroof
import System.Directory
import Data.Default


main = do
   -- all output is put into the canvas directory
   createDirectoryIfMissing False "sunroof"
   -- and run the tests to generate the html5 canvas examples
   runTests (examples) "sunroof-index.html" $ [sunroofTester]
