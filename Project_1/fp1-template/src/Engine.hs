module Engine
  ( matches,
    matchesSimp,
    maxMatchEnd,
    minMatchStart,
    llmatch,
  )
where

import Location
import Regex

-- Full matching

-- | Tests whether the regex matches the entire string.
matches :: Regex -> String -> Bool

matches r [] = nullable r
matches r [c] = nullable (deriv r c)
matches r (c:cs) = matches (deriv r c) cs

-- Simplification

-- | Tests whether the regex matches the entire string.
-- Makes use of regex simplification rules.
matchesSimp :: Regex -> String -> Bool

matchesSimp r [] = nullable r
matchesSimp r [c] = nullable (fullSimplify (deriv r c))
matchesSimp r (c:cs) = matchesSimp (fullSimplify (deriv r c)) cs

fullSimplify :: Regex -> Regex
fullSimplify r
  | r == simplify r = r
  | otherwise = fullSimplify (simplify r)

-- Partial matching

-- | Finds the maximal end location of a match.
-- Returns nothing if no match exists.
maxMatchEnd :: Regex -> Loc -> Maybe Loc
maxMatchEnd _ _ = Nothing

-- | Finds the minimal start location of a match.
-- Returns nothing if no match exists.
minMatchStart :: Regex -> String -> Maybe Loc
minMatchStart _ _ = Nothing

-- | Converts start and end locations to a span.
toSpan :: Loc -> Loc -> Span
toSpan _ _ = ([], [], [])

-- | Finds the leftmost longest match.
-- Returns nothing if no match exists.
llmatch :: Regex -> String -> Maybe Span
llmatch _ _ = Nothing
