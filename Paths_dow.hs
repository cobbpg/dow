-- This file is needed to let me load the app in ghci during development.

module Paths_dow where

getDataFileName n = return ("../" ++ n)
