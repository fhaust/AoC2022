{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as L

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

import           Data.Void (Void)
import           Data.Maybe (catMaybes)

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.IntMap as M

-- parsing

type Parser = Parsec Void Text

crateP = Just <$> between "[" "]" upperChar
noCrateP = Nothing <$ "   "
cratesP = ((crateP <|> noCrateP) `sepBy` " ") `endBy` newline

numbersP = between " " " " decimal `sepBy` " "

moveP :: Parser (Int,Int,Int)
moveP = (,,) <$> ("move " *> decimal) <*> (" from " *> decimal) <*> (" to " *> decimal)
movesP = moveP `endBy1` newline

inputP = do
  crates <-  cratesP
  numbers <- numbersP
  space1
  moves <- movesP

  let crates' = M.fromList . zip numbers . map catMaybes . L.transpose $ crates

  return (crates', moves)

-- algorithm

step rev crates (n,f,t) = let (a,b) = splitAt n (crates M.! f) in M.insert f b . M.adjust (rev a ++) t $ crates

part1 crates moves = M.elems $ head <$> foldl (step reverse) crates moves
part2 crates moves = M.elems $ head <$> foldl (step id)      crates moves

-- main

main = do

  t <- T.readFile "inputs/day05.txt"
  let Right (crates, moves) = parse inputP "" t
  putStrLn $ "Part 1: " ++ part1 crates moves
  putStrLn $ "Part 2: " ++ part2 crates moves
