module Utils.FileUtils where

import Control.Lens
import Control.Applicative
import Happstack.Server ( ContentType )
import System.Directory
import System.FilePath.Lens
import System.Random

randomStringIO :: Int -> IO String
randomStringIO len = randomString len <$> newStdGen

randomString :: RandomGen g => Int -> g -> String
randomString len gen =
  let alphanum = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
      indxs    = take len $ randomRs (0, length alphanum - 1) gen
   in map (alphanum !!) indxs

moveToRandomFile
  :: FilePath         -- ^ file directory
  -> Int              -- ^ length of filename
  -> FilePath         -- ^ file
  -> IO String        -- ^ new filename
moveToRandomFile dir len file = do
  gen <- newStdGen
  let new_file = file & directory .~ dir
                      & filename .~ randomString len gen
  renameFile file new_file
  return new_file

getFilePath :: (FilePath, FilePath, ContentType) -> FilePath
getFilePath (fp, _, _) = fp

getFileName :: (FilePath, FilePath, ContentType) -> FilePath
getFileName (_, name, _) = name

