-- Copyright (C) 2016 Jan Synáček
--
-- Author: Jan Synáček <jan.synacek@gmail.com>
-- URL: https://github.com/jsynacek/lambda-launcher
-- Created: Feb 2016
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth
-- Floor, Boston, MA 02110-1301, USA.

import Prelude hiding (lookup)

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Data.Maybe (catMaybes, fromMaybe)
import Text.Parsec

import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, sortBy)
import qualified Data.Map as M
import Graphics.UI.Gtk
import System.Directory (getDirectoryContents, getHomeDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import System.Process (spawnProcess)

-------------------------------------------------------------------------------
-- Really naive desktop file parser. It was easier to roll my own than to
-- install cabal packages...
-------------------------------------------------------------------------------
comment :: Parsec String String ()
comment =  char '#' *> skipMany (noneOf "\n")

eol :: Parsec String String ()
eol = newline >> return ()

section :: Parsec String String String
section = do
  section <- char '[' *> manyTill (noneOf "[]") (char ']')
  putState section
  return section

item :: Parsec String String (String, String)
item = (,) <$> manyTill (noneOf "=") (char '=') <*> manyTill anyChar (try comment <|> try eol <|> eof)


line :: Parsec String String (Maybe (String, String))
line = do
  state <- getState
  try ((comment <|> eol <|> (section >> return ())) >> return Nothing) <|> (item >>= decide state)
    -- Only care about [Desktop Entry] sections.
    where decide state | state == "Desktop Entry" = return . Just
                       | otherwise                = (\_ -> return Nothing)

file :: Parsec String String [(String, String)]
file = many line <* eof >>= return . catMaybes
-------------------------------------------------------------------------------

parseFile :: FilePath -> IO (Maybe [(String, String)])
parseFile path = do
  input <- readFile path
  --case parse file path input of
  case runParser file "" path input of
    Right res -> return $ Just $ filter pick res
    Left err  -> putStrLn (show err) >> return Nothing
  where pick (key, _) = key `elem` ["Exec", "Icon", "Name"]

parseFiles :: FilePath -> IO [[(String, String)]]
parseFiles dir = do
  paths <- (fmap (dir </>)) <$> (listDesktopFiles dir)
  catMaybes <$> mapM parseFile paths

-- In System.Directory since 1.2.5, which I don't currently have.
listDirectory :: FilePath -> IO [FilePath]
listDirectory dir =
  (filter f) <$> (getDirectoryContents dir)
  where f filename = filename /= "." && filename /= ".."

listDesktopFiles dir = (filter f) <$> (listDirectory dir)
  where f filename = takeExtension filename == ".desktop"


lookup key map = fromMaybe "unknown" $ M.lookup key map

sortMap :: [M.Map String String] -> [M.Map String String]
sortMap = sortBy $ \left right ->
  lookup "Name" left `compare` lookup "Name" right


runProgram :: String -> IO ()
runProgram cmd = spawnProcess bin args >> return ()
  where bin = head $ words cmd
        args = filter (not . ("%" `isPrefixOf`)) $ tail $ words cmd

searchEqualFunc :: String -> (M.Map String String) -> IO Bool
searchEqualFunc str row = return $
  map toLower str `isInfixOf` map toLower (lookup "Name" row) ||
  map toLower str `isInfixOf` map toLower (takeBaseName $ lookup "Exec" row)

main :: IO ()
main = do
  initGUI
  win <- windowNew
  set win [windowDefaultWidth := 250
          ,windowDefaultHeight := 300
          ,windowTitle := "λ Launcher"
          ,windowIconName := "system-run"
          ]
  on win objectDestroy mainQuit
  on win keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    key <- eventKeyName
    liftIO $ widgetDestroy win

  box <- vBoxNew False 1
  containerAdd win box

  sw <- scrolledWindowNew Nothing Nothing
  boxPackStart box sw PackGrow 0

  home <- getHomeDirectory
  desktopFiles <- (++) <$> parseFiles "/usr/share/applications" <*> parseFiles (home </> ".local/share/applications")
  model <- listStoreNew $ sortMap $ fmap M.fromList desktopFiles
  view <- treeViewNewWithModel model
  treeViewSetHeadersVisible view False

  col <- treeViewColumnNew
  renderer <- cellRendererPixbufNew
  cellLayoutPackStart col renderer False
  cellLayoutSetAttributes col renderer model $ \row -> [cellPixbufIconName := lookup "Icon" row]
  treeViewAppendColumn view col

  col <- treeViewColumnNew
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer False
  cellLayoutSetAttributes col renderer model $ \row -> [cellText := lookup "Name" row]
  treeViewAppendColumn view col

  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
    (i:_) <- treeModelGetPath model iter
    row <- listStoreGetValue model i
    searchEqualFunc str row

  on view rowActivated $ \tpath tcol -> do
    row <- listStoreGetValue model (head tpath)
    runProgram (fromMaybe "/bin/true" $ M.lookup "Exec" row)
    widgetDestroy win

  containerAdd sw view

  button <- buttonNew
  set button [buttonLabel := "Quit"]
  on button buttonActivated $ widgetDestroy win
  boxPackStart box button PackNatural 0

  widgetShowAll win
  mainGUI
