module Location (Loc, Span, asLoc, asSpan, asEndLoc, reverseLoc, reverseSpan, locHead) where

type Loc = (String, String)

type Span = (String, String, String)

asLoc :: Span -> Loc
asLoc (s, u, v) = (s, u ++ v)

asSpan :: Loc -> Span
asSpan (x, y) = (x, [], y)

asEndLoc :: String -> Loc
asEndLoc x = (reverse x, [])

reverseLoc :: Loc -> Loc
reverseLoc (u, v) = (v, u)

reverseSpan :: Span -> Span
reverseSpan (s, u, v) = (v, reverse u, s)

locHead :: Loc -> Char
locHead (_, x : _) = x
