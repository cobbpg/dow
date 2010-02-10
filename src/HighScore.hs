module HighScore where

import Control.Monad
import System.Directory
import System.IO

loadScore :: IO Int
loadScore = do
  dir <- getAppDir

  let fileName = dir ++ "/highscore"
  fileChk <- doesFileExist fileName
  unless fileChk $ writeFile fileName "0"

  score <- readFile fileName
  return (read score)

saveScore :: Int -> IO ()
saveScore score = do
  dir <- getAppDir
  writeFile (dir ++ "/highscore") (show score)

getAppDir :: IO FilePath
getAppDir = do
  dir <- getAppUserDataDirectory "dow"
  dirChk <- doesDirectoryExist dir
  unless dirChk $ createDirectory dir
  return dir