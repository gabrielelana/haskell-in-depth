{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stems where

import Protolude

import qualified Prelude

newtype Stem a = Stem {unStem :: [a]} deriving (Foldable)

comp :: Eq a => Stem a -> Stem a -> Maybe (Stem a)
comp (Stem ys) (Stem xs)
  | ys `isPrefixOf` xs = Just $ Stem $ drop (length ys) xs
  | otherwise = Nothing

superStem :: Eq a => Stem a -> Stem a -> Bool
superStem y = isJust . comp y

dist :: Eq a => Stem a -> Stem a -> Maybe Int
dist y = fmap length . comp y

newtype Population a = Population [Stem a] deriving (Foldable)

-- | read input from a file
population :: FilePath -> IO (Population Char)
population = notImplemented

subs :: Eq a => Stem a -> Population a -> Population a
subs x (Population xs) = Population $ filter (isJust . comp x) xs

size :: Population a -> Int
size = length


value :: Eq a => Population a -> Stem a -> Stem a -> Maybe Int
value xs y x = (size (subs x xs) *) <$> dist y x


-- | given a population and one stem in it computes the (at-most) 'n' most valuable
-- super stems
query ::
  -- | maximum number of super stems
  Int ->
  -- | stem population
  Population a ->
  -- | base stem
  Stem a ->
  -- | super stems
  [Stem a]
query = notImplemented

mainNaive :: IO b
mainNaive = do
  [fp, n'] <- getArgs
  p <- population fp
  let q = query (Prelude.read n') p
  forever $ do
    x <- Prelude.getLine
    traverse (Prelude.putStrLn . unStem) $ q $ Stem x


-- TODO: refine the value function so that query returns something better :-)
-- (in case you do not like it)


-- TODO: define population as a trie usind Data.Map
-- so that values are computed at trie construction time

type ToBeImplemented = Void

newtype PopulationTrie a = PopulationTrie ToBeImplemented

-- | primitive operation to update a population trie with a new stem
insert :: Stem a -> PopulationTrie a -> PopulationTrie a
insert = notImplemented

-- | select the trie of the given stem and traverse all children to
-- rebuild the 'n' best super-stems
queryTrie ::
  -- | maximum number of super stems
  Int ->
  -- | stem population
  PopulationTrie a ->
  -- | base ste
  Stem a ->
  -- | super stems
  [Stem a]
queryTrie = notImplemented

-- TODO: prove that is possible/impossible to prune the search based on values only
-- and that is unnecessary/necessary to store some other auxiliary information at
-- each node

-- TODO: optimize the population trie by hardcoding 'n' and tracking some
-- auxiliary information in the nodes to then traverse only the necessary subtries
-- to collect the best super-stems of a node

newtype PopulationTrieO a = PopulationTrieO ToBeImplemented

-- | primitive operation to update a population trie with a new stem
insertO :: Stem a -> PopulationTrieO a -> PopulationTrieO a
insertO = notImplemented

-- | select the trie of the given stem and traverse only necessary children to
-- rebuild the 'n' best super-stems
queryTrieO ::
  -- | stem population
  PopulationTrieO a ->
  -- | base ste
  Stem a ->
  -- | super stems
  [Stem a]
queryTrieO = notImplemented

-- TODO: write main so that we can add files to the population interactevely as
-- we perform searches


main :: IO ()
main = notImplemented

-- TODO: improve the trie so that it returns also matches for infixes
