import Data.Array
import Data.List


--GET THE BALANCED PARTITIONS FROM THE LIST--
balancedPartition :: (Integral a) => [a] -> [[a]]
balancedPartition [] = error "Empty list!"
balancedPartition [a] = [[a],[]]
balancedPartition [a,b] = [[a],[b]]
balancedPartition (a:as) =
  let h = halfSum (a:as)
   in let pA = getPartA h (getSubsets (a:as))
    in [pA, getPartB pA (a:as)]


--GET HALF THE SUM OF THE LIST--
halfSum :: (Integral a) => [a] -> a
halfSum a = div (sum a) 2


--BUILD ALL SUBSETS FROM THE LIST--
buildSubsets :: (Integral a) => [a] -> [[a]]
buildSubsets [] = [[]]
buildSubsets (a:as) = buildSubsets as ++ map (a:) (buildSubsets as)


--FILTER OUT UNWANTED SUBSETS THAT HAVE A SUM
--GREATER THAN (SUM LIST)/2--
filterSubsets :: (Integral a) => a -> [[a]] -> [[a]]
filterSubsets _ [] = []
filterSubsets hs (a:as) =
  if (sum a > hs || sum a == 0)
    then filterSubsets hs as
    else [a] ++ filterSubsets hs as


--GET ALL SUBSETS OF THE LIST--
--BUILD THE SUBSETS AND FILTER OUT sum (subset) > sum(list)/2--
getSubsets :: (Integral a) => [a] -> [[a]]
getSubsets as = filterSubsets (halfSum as) (buildSubsets as)


--GET FIRST PARTITION--
--SUBSET WITH MINIMISED DIFFERENCE FROM SUM/2--
getPartA :: (Integral a) => a -> [[a]] -> [a]
getPartA _ [] = []
getPartA _ [a] = a
getPartA h [a,b] =
  if ((h - (sum a)) < (h - (sum b)))
    then a
    else b
getPartA h (a:b:as) =
  if ( (h - (sum a)) < (h - (sum b)))
    then getPartA h (a:as)
    else getPartA h (b:as)


--GET SECOND PARTITION--
--ORIGINAL LIST MINUS PARTITION A--
getPartB :: (Integral a) => [a] -> [a] -> [a]
getPartB [] as = as
getPartB (a:as) (b:bs) = getPartB as (delete a (b:bs))
