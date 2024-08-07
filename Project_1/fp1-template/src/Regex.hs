module Regex (Regex (..), reverseRegex, size, nullable, deriv, simplify) where

data Regex
  = EmptySet -- ∅
  | Epsilon -- Ɛ
  | Any -- ⊤
  | Char Char -- a
  | Concatenation Regex Regex -- ab
  | Intersection [Regex] -- a & b & ..
  | Alternation [Regex] -- a | b | ..
  | Negation Regex -- !a
  | Star Regex -- a*
  | Range Regex Int (Maybe Int) -- a{n,m?}
  deriving (Show, Eq)

-- | Returns the reverse of the regex
reverseRegex :: Regex -> Regex
reverseRegex _ = EmptySet

-- | Returns the size of the regex r
size :: Regex -> Int
size _ = 0

-- | Simpifies the given regex
simplify :: Regex -> Regex

simplify (Char c) = Char c
simplify Epsilon = Epsilon
simplify EmptySet = EmptySet
simplify Any = Any  --Use in simplication

simplify (Range _ 0 (Just 0)) = Epsilon
simplify (Range r 0 Nothing) = Star r
simplify (Range r n Nothing) = Range r n Nothing
simplify (Range r n (Just m)) = Range r n (Just m)

simplify (Star Epsilon) = Epsilon
simplify (Star EmptySet) = Epsilon
simplify (Star (Star r)) = Star (simplify r)
simplify (Star r) = Star (simplify r)

simplify (Negation (Negation r)) = simplify r
simplify (Negation r) = Negation (simplify r)

simplify (Concatenation EmptySet _) = EmptySet
simplify (Concatenation _ EmptySet) = EmptySet
simplify (Concatenation Epsilon r) = simplify r
simplify (Concatenation r Epsilon) = simplify r
simplify (Concatenation r1 r2) = Concatenation (simplify r1) (simplify r2)

simplify (Alternation []) = EmptySet
simplify (Alternation [r]) = r
simplify (Alternation r)
  | elem (Negation EmptySet) r = Negation EmptySet
  | elem Any r = Any
  | otherwise = Alternation [simplify r' | r' <- uniq r, r' /= EmptySet]

simplify (Intersection [r]) = r
simplify (Intersection r)
  | elem EmptySet r = EmptySet
  | otherwise = Alternation [simplify r' | r' <- uniq r, r' /= Negation EmptySet, r' /= Any]


uniq :: [Regex] -> [Regex]
uniq [] = []
uniq (r:rs) = r : uniq (remove r rs)
  where
    remove :: Regex -> [Regex] -> [Regex]
    remove r rs = [rs' | rs' <- rs, r /= rs']

-- | Checks whether the regex can match the empty string
nullable :: Regex -> Bool
nullable EmptySet = False
nullable (Char _) = False
nullable Epsilon = True
nullable (Star _) = True
nullable Any = False
nullable (Intersection r) = and [nullable r' | r' <- r]
nullable (Alternation r) = or [nullable r' | r' <- r]
nullable (Concatenation r1 r2) = (nullable r1) && (nullable r2)
nullable (Negation r) = not (nullable r)

nullable (Range _ 0 _) = True
nullable (Range _ _ Nothing) = False
nullable (Range _ _ (Just _)) = False

helper :: Bool -> Regex
helper b
  | b == True = Epsilon
  | otherwise = EmptySet

-- | Returns the derivative of the regex with respect to a character
deriv :: Regex -> Char -> Regex
deriv EmptySet _ = EmptySet
deriv Epsilon _ = EmptySet
deriv Any _  = Epsilon
deriv (Char c') c
  | c == c'   = Epsilon
  | otherwise = EmptySet
deriv (Star r) c = Concatenation (deriv r c) (Star r)
deriv (Intersection r) c = Intersection [deriv r' c | r' <- r]
deriv (Alternation r) c = Alternation [deriv r' c | r' <- r] --deriv (Alternation [r]) c = deriv r c --Alternation ([deriv r c] ++ [deriv (Alternation rs) c])
deriv (Concatenation r1 r2) c = Alternation ([Concatenation (deriv r1 c) r2] ++ [Concatenation (helper (nullable r1)) (deriv r2 c)])  -- Double Check
deriv (Negation r) c = Negation (deriv r c)

deriv (Range r 0 Nothing) _ = Star r --EmptySet
deriv (Range r 1 Nothing) c = Concatenation (deriv r c) (Star r)
deriv (Range r n Nothing) c = Concatenation (deriv r c) (Range r (n - 1) Nothing)

deriv (Range _ 0 (Just 0)) _ = EmptySet
deriv (Range r 0 (Just m)) c = Alternation [(deriv r c), (Range r 0 (Just (m - 1)))]
deriv (Range r n (Just m)) c = Concatenation (deriv r c) (Range r (n - 1) (Just (m - 1)))
