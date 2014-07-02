import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Data.Array
import Data.Tuple
import System.Random

type Mapping = Array Char Int

dom = ('a', 'z')
cod = (-2, 8)

mutate :: Mapping -> IO Mapping
mutate = (<$> fmap pure point) . (//)
    where
        point = (,) <$> randomRIO dom <*> randomRIO cod

crossoverUniform :: (Mapping, Mapping) -> IO (Mapping, Mapping)
crossoverUniform = elems *** elems >>> uncurry zip
    >>> map pure >>> zipWith (<*>) (repeat ch)
    >>> sequence >>> fmap (unzip >>> listArray dom *** listArray dom)
    where
        ch = ([id, swap]!!) <$> randomRIO (0, 1)

choice :: Ord f => (a -> f) -> Float -> [a] -> IO a
choice f e xs = (xs'!!) . floor . (* fromIntegral (length xs))
    . exp . (*e) . log
    <$> randomRIO (0.0, 1.0)
    where
        xs' = sortBy (compare `on` f) xs

nextGeneration :: Float -> Float -- crossover rate, mutational rate
   -> ([Mapping] -> IO Mapping) -- selector
   -> ((Mapping, Mapping) -> IO (Mapping, Mapping)) -- crossover
   -> (Mapping -> IO Mapping) -- mutation
   -> [Mapping] -- current generation
   -> IO [Mapping]

nextGeneration rC rM fS fC fM p = collect (length p)
    $ repeat (randomRIO (0.0, 1.0) >>= operate)
    where
        operate x
            | x < rM      = fmap pure $ fS p >>= fM
            | x < rM + rC = fmap g    $ (,) <$> fS p <*> fS p >>= fC
            | otherwise   = fmap pure $ fS p
            where
                g (a, b) = [a, b]

        collect 0 _ = return []
        collect _ [] = return []
        collect 1 (x:_) = take 1 <$> x
        collect n (x:xs) = x >>= \v -> (v ++) <$> collect (n - length v) xs

firstGeneration :: IO [Mapping]
firstGeneration = repeat <$> listArray dom
    <$> (rangeSize dom `replicateM` randomRIO cod)
