getFloor :: [Char] -> Int
getFloor ps = length [p | p <- ps, p == '('] - length [p | p <- ps, p == ')']

getIndex :: [(Int, Char)] -> Int -> Maybe Int
getIndex [] _ = Nothing
getIndex ((i, '(') : ips) n = case n of
  -2 -> Just i
  otherwise -> getIndex ips (n + 1)
getIndex ((i, ')') : ips) n = case n of
  0 -> Just i
  otherwise -> getIndex ips (n - 1)