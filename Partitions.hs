import Data.List

-- 'PARTITION' DATA TYPE USED FOR CONTAINING A PARTITION (MAINLY FOR THE 'SPLITAT' FUNCTION) --
type Partition a = ([a],[a])


-- MAIN METHOD --
balancedPartition :: (Integral a) => [a] -> Partition a
balancedPartition [] = ([],[])
balancedPartition [a] = ([a],[])
balancedPartition [a,b] = ([a],[b])
balancedPartition as = findBalanced (getPartitions as)


-- GET PARTITIONS (BY ACCUMULATING THEM...) --
getPartitions :: (Integral a) => [a] -> [Partition a]
getPartitions as =
  let len = length as
    in accumulatePartitions len (len `div` 2) 0 (sort as)


-- ACCUMULATE THE PARTITIONS --
accumulatePartitions :: (Integral a) => Int -> Int -> Int -> [a] -> [Partition a]
accumulatePartitions len halfLen n (a:as) =
  let partitions = split len 0 (a:as)
    in if (n < len)
      then let newN = n + 1
        in partitions ++ accumulatePartitions len halfLen newN (as++[a])
      else partitions


-- SPLIT A LIST INTO ALL PARTITIONS --
split :: (Integral a) => Int -> Int -> [a] -> [Partition a]
split len n as =
  if (n < len)
    then let newN = n + 1
      in [splitAt n as] ++ split len newN as
    else []


-- FIND MOST BALANCED PARTITION IN A LIST OF PARTITIONS --
findBalanced :: (Integral a) => [Partition a] -> Partition a
findBalanced [a] = a
findBalanced [a,b] = getMoreBalanced a b
findBalanced (a:b:bs) =
  let balanced = getMoreBalanced a b
    in if ( (difference balanced) == 0)
      then balanced
      else findBalanced (balanced:bs)


-- GET MORE BALANCED PARTITION OUT OF TWO --
getMoreBalanced :: (Integral a) => Partition a -> Partition a -> Partition a
getMoreBalanced a b =
  let diffA = difference a
    in let diffB = difference b
      in
        if(diffA < diffB)
          then a
          else b


-- GET DIFFERENCE OF A PARTITION'S LISTS --
difference :: (Integral a) => Partition a -> a
difference (a,b) = abs ((sum a) - (sum b))


-- TEST FUNCTION --
-- GET SUMS OF A PARTITION'S LISTS --
sums :: (Integral a) => Partition a -> [a]
sums (a,b) = [(sum a), (sum b)]
