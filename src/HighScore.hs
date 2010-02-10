module HighScore where

import Control.Monad
import System.Directory
import System.IO

loadScore :: IO Int
loadScore = do
  dir <- getAppDir

  let fileName = dir ++ "/highscore"
  fileChk <- doesFileExist fileName
  unless fileChk $ do
    hdl <- openFile fileName WriteMode
    hPutStr hdl "0"
    hClose hdl

  hdl <- openFile fileName ReadMode
  score <- hGetLine hdl
  hClose hdl

  return (read score)

saveScore :: Int -> IO ()
saveScore score = do
  dir <- getAppDir
  hdl <- openFile (dir ++ "/highscore") WriteMode
  hPutStr hdl (show score)
  hClose hdl

getAppDir :: IO FilePath
getAppDir = do
  dir <- getAppUserDataDirectory "dow"
  dirChk <- doesDirectoryExist dir
  unless dirChk $ createDirectory dir
  return dir