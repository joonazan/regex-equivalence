module NFA where

import DFA
import Regex
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import qualified Data.MultiMap as MultiMap


data NFA node symbol =
    NFA (Map node (Map symbol (Set node))) (Set node) node

toDFA :: (Ord node, Ord sym) => NFA node sym -> DFA (Set node) sym
toDFA (NFA arrows accepting start) = DFA arrows' accepting' start' where
    accepting' =
        Set.filter (not . Set.null . Set.intersection accepting) states'

    arrows' = Map.fromList $
        fmap (\x -> (x, conns x)) $ Set.toList states'

    states' =
        dfs neighbors $ start'

    neighbors =
        Map.elems . conns

    conns =
        Map.unionsWith Set.union . fmap (\x -> Map.findWithDefault Map.empty x arrows) .
        Set.toList

    start' = Set.singleton start

fromRegex :: Regex -> NFA Int Char
fromRegex regex = epsilonToNFA res where
    res = evalState (buildNFA regex) 0

data NFAPart = NFAPart
    { arrows :: Map Int (Map Char Int)
    , epsilons :: [(Int, Int)]
    , start :: Int
    , end :: Int
    }

epsilonToNFA :: NFAPart -> NFA Int Char
epsilonToNFA (NFAPart arrows epsilons start end) = NFA arrows' accepting' start where
    arrows' =
        Map.fromList $
        fmap (\x -> (x, indirectArrows x)) states

    states =
        Set.toList $
        Set.fromList (Map.keys arrows) `Set.union` Set.fromList (MultiMap.keys epsilons')

    indirectArrows :: Int -> Map Char (Set Int)
    indirectArrows =
        Map.unionsWith Set.union . Set.toList .
        Set.map (\x -> Map.map Set.singleton $ Map.findWithDefault Map.empty x arrows) .
        connectedViaEpsilon
    
    accepting' =
        Set.union (Set.singleton end) $ Set.fromList $ filter connectedToEnd states

    connectedToEnd =
        Set.member end . connectedViaEpsilon

    connectedViaEpsilon =
        dfs (\x -> MultiMap.lookup x epsilons')

    epsilons' =
        MultiMap.fromList epsilons

buildNFA :: Regex -> State Int NFAPart
buildNFA (OneOf regexes) = buildHelper f where
    f entry exit = do
        nfas <- sequence $ map buildNFA regexes

        let fromEntry = map ( (,) entry . NFA.start ) nfas
        let toExit = map (\x -> (end x, exit) ) nfas

        let arrows' = foldr1 combine $ map arrows nfas
        let epsilons' = foldr (++) [] (map epsilons nfas) ++ fromEntry ++ toExit

        return (arrows', epsilons')


buildNFA (Consecutive regexes) =
    fmap (foldr1 putInFront) $
    sequence $ map buildNFA regexes

buildNFA (Character c) = buildHelper
    (\entry exit -> return
        ( (Map.singleton entry (Map.singleton c exit))
        , []
        )
    )

buildNFA (Maybe regex) = extraConnections
    (\entry exit -> [(entry, exit)])
    regex

buildNFA (OneOrMore regex) = extraConnections
    (\entry exit -> [(exit, entry)])
    regex

buildNFA (NTimes regex) = extraConnections
    (\entry exit -> [(entry, exit), (exit, entry)])
    regex

extraConnections :: (Int -> Int -> [(Int, Int)]) -> Regex -> State Int NFAPart
extraConnections g regex = buildHelper f where
    f entry exit = do
        NFAPart arrows epsilons childEntry childExit <- buildNFA regex
        return
            ( arrows
            , g childEntry childExit
                ++ [(entry, childEntry), (childExit, exit)]
                ++ epsilons
            )

buildHelper f = do
    entry <- nextId
    exit <- nextId
    (arrows, epsilons) <- f entry exit
    return $
        NFAPart arrows epsilons entry exit

combine =
    Map.unionWith Map.union

putInFront (NFAPart arr1 eps1 s m1) (NFAPart arr2 eps2 m2 e) =
    NFAPart (combine arr1 arr2) ((m1, m2) : (eps1++eps2)) s e

nextId = do
    current <- get
    put (current+1)
    return current