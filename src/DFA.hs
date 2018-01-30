module DFA where

import qualified Data.Map as Map
import Data.Map.Merge.Strict
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (fromMaybe)


data DFA node symbol =
    DFA{transitions :: Map.Map node (Map.Map symbol node)
    , accepting :: Set.Set node
    , start :: node
    }

recognizedByOne :: (Ord n, Ord s) => DFA n s -> DFA n s -> Maybe ([s], Bool)
recognizedByOne a b = result where
    
    result =
        case differing (Just $ start a, Just $ start b) [] Set.empty of
            Right _ -> Nothing
            Left path -> Just path

    differing pos path visited =
        if Set.member pos visited then
            Right visited
        else if difference pos then
            Left (reverse path, accepts (fst pos) a)
        else
            foldr
                step
                (Right visited)
                (neighbors pos)
        where
            step (c, n) (Right visited) =
                differing n (c:path) $ Set.insert pos visited
            step _ (Left path) =
                Left path

    neighbors (x, y) = Map.toList $ merge' (oneSide x a) (oneSide y b) where
        oneSide mState dfa =
            fromMaybe Map.empty
            (mState >>= (\s -> Map.lookup s $ transitions dfa))

        merge' =
            merge
                (mapMissing $ \k x -> (Just x, Nothing))
                (mapMissing $ \k x -> (Nothing, Just x))
                (zipWithMatched $ \k a b -> (Just a, Just b))

    difference (x, y) =
        accepts x a /= accepts y b

    accepts (Just x) a =
        Set.member x (accepting a)
    accepts Nothing _ =
        False

dfs :: Ord a => (a -> [a]) -> a -> Set a
dfs neighbors start = inner start $ Set.singleton start where
    inner pos visited =
        foldr (\n v ->
            if Set.member n v then
                v
            else
                inner n (Set.insert n v)
            )
            visited
            (neighbors pos)
