{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Data.List
import qualified Data.Text as T

generatePaths :: [(Int, Int)] -> [([String], String)]
generatePaths ((0,x):xs) =
  go [0] (map (\(depth, item) -> (depth, (show item))) xs) [([show 0], show x)]
  where
    go ds@(currentDepth:_) (curr@(depth, item):xs) res
      | depth < 1 = error "The depth must be > 1. 0 is reserved for the root."
      | depth == currentDepth =
          go ds xs (res ++ [(map show ds, item)])
      | depth == currentDepth + 1 =
          go (depth:ds) xs (res ++ [(map show (depth:ds), item)])
      | depth > currentDepth + 1 =
          let diffRange = takeWhile ((>) depth) (iterate succ (currentDepth + 1))
              holes = zip diffRange (repeat "<>")
          in go ds (holes ++ [curr] ++ xs) res
      | depth < currentDepth =
          let droppedDepth = dropWhile ((/=) depth) ds
          in go droppedDepth xs (res ++ [(map show droppedDepth, item)])
    go _ [] res = res
generatePaths _ = error "All tree must start at the root, eg. [(0,10) ... ]"

toTreeItem :: [([String], String)] -> [String]
toTreeItem = map (\(path, item) -> concat ((intersperse "/" ((reverse path) ++ [item]))))

ui :: [(Int, Int)] -> IO ()
ui roseTree = do
  _ <- FL.setScheme "gtk+"
  window <- windowNew (toSize (250,400)) Nothing (Just "Simple Tree")
  begin window
  windowWidth' <- getW window
  windowHeight' <- getH window
  tree <- treeNew (toRectangle (10,10,windowWidth' - 20,windowHeight' - 20)) Nothing
  setShowroot tree False
  let items = toTreeItem (generatePaths roseTree)
  mapM_ (add tree . T.pack) items
  end window
  showWidget window
  _ <- FL.run
  return ()

sample = [(0,1), (1,2), (2,3), (4,5), (1,4), (1,5), (2,6), (3,7), (3,8), (1,9)]

main :: IO ()
main = (ui sample) >> FL.run >> FL.flush

replMain :: IO ()
replMain = (ui sample) >> FL.replRun
