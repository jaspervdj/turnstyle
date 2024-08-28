{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Turnstyle.Parse
  ( Ann
  , Dir (..)
  , Pos (..)
  , ParseError (..)
  , parseImage

  , RelPos (..)
  , relPos
  , contiguous

  , initialPosition
  ) where

import qualified Data.Set           as S
import           GHC.Exception      (Exception)
import           Turnstyle.Expr
import           Turnstyle.Image
import           Turnstyle.Prim
import           Turnstyle.Quattern (Quattern (..), quattern)
import           Turnstyle.TwoD

inside :: Image img => Pos -> img -> Bool
inside (Pos x y) img = x >= 0 && x < width img && y >= 0 && y < height img

initialPosition :: Image img => img -> Maybe Pos
initialPosition img
    | width img <= 0 || height img <= 0 = Nothing
    | otherwise                         = Just $ Pos 0 (height img `div` 2)

-- | Selecting single pixel from the turnstyle:
--
--     L
--     CF
--     R
--
data RelPos = LeftPos | CenterPos | FrontPos | RightPos deriving (Eq, Show)

relPos :: Pos -> Dir -> RelPos -> Pos
relPos (Pos x y) _ CenterPos = Pos (x    ) (y    )
relPos (Pos x y) R LeftPos   = Pos (x    ) (y - 1)
relPos (Pos x y) R FrontPos  = Pos (x + 1) (y    )
relPos (Pos x y) R RightPos  = Pos (x    ) (y + 1)
relPos (Pos x y) D LeftPos   = Pos (x + 1) (y    )
relPos (Pos x y) D FrontPos  = Pos (x    ) (y + 1)
relPos (Pos x y) D RightPos  = Pos (x - 1) (y    )
relPos (Pos x y) L LeftPos   = Pos (x    ) (y + 1)
relPos (Pos x y) L FrontPos  = Pos (x - 1) (y    )
relPos (Pos x y) L RightPos  = Pos (x    ) (y - 1)
relPos (Pos x y) U LeftPos   = Pos (x - 1) (y    )
relPos (Pos x y) U FrontPos  = Pos (x    ) (y - 1)
relPos (Pos x y) U RightPos  = Pos (x + 1) (y    )

type Ann = (Pos, Dir, Quattern)

data ParseError
    = OutOfBounds
    | EmptyImage
    | UnknownSym Int
    | UnknownPrim Int Int
    deriving (Eq, Show)

instance Exception ParseError

parse
  :: forall img. (Image img, Eq (Pixel img), Show (Pixel img))
  => Pos -> Dir -> img -> Expr Ann ParseError (Pixel img)
parse pos dir img = case pattern of
    _ | not (inside (relPos pos dir LeftPos) img)   -> Err ann OutOfBounds
    _ | not (inside (relPos pos dir CenterPos) img) -> Err ann OutOfBounds
    _ | not (inside (relPos pos dir FrontPos) img)  -> Err ann OutOfBounds
    _ | not (inside (relPos pos dir RightPos) img)  -> Err ann OutOfBounds

    -- App
    ABCA -> App ann parseLeft  parseRight
    ABAC -> App ann parseLeft  parseFront
    ABCC -> App ann parseFront parseRight

    -- Lam
    AABC -> Lam ann (relPixel RightPos) parseLeft
    ABCB -> Lam ann (relPixel LeftPos) parseRight
    ABBC -> Lam ann (relPixel CenterPos) parseFront

    -- Var
    ABAA -> Var ann $ relPixel CenterPos
    AABA -> Var ann $ relPixel FrontPos
    AAAB -> Var ann $ relPixel RightPos
    ABBB -> Var ann $ relPixel LeftPos

    -- Int/Prim
    ABCD -> case areaLeft of
        1 -> Lit ann $ (fromIntegral areaFront :: Integer) ^ areaRight
        2 -> case decodePrim areaFront areaRight of
            Nothing   -> Err ann $ UnknownPrim areaFront areaRight
            Just prim -> Prim ann prim
        _ -> Err ann $ UnknownSym areaLeft

    -- Id
    AAAA -> Id ann $ parseFront
    ABBA -> Id ann $ parseFront
    AABB -> Id ann $ parseLeft
    ABAB -> Id ann $ parseRight
 where
    ann        = (pos, dir, pattern)
    relPos'    = relPos pos dir
    relPixel r = let (Pos px py) = relPos' r in pixel px py img

    pattern = quattern
        (relPixel LeftPos)
        (relPixel CenterPos)
        (relPixel FrontPos)
        (relPixel RightPos)

    parseLeft  = parse (relPos' LeftPos) (rotateLeft dir) img
    parseFront = parse (relPos' FrontPos) dir img
    parseRight = parse (relPos' RightPos) (rotateRight dir) img

    areaLeft  = S.size $ contiguous (relPos' LeftPos) img
    areaFront = S.size $ contiguous (relPos' FrontPos) img
    areaRight = S.size $ contiguous (relPos' RightPos) img

parseImage
  :: forall img. (Image img, Eq (Pixel img), Show (Pixel img))
  => Maybe Pos -> img -> Expr Ann ParseError (Pixel img)
parseImage (Just (Pos x y)) img = parse (Pos x y) R img
parseImage Nothing img = case initialPosition img of
    Nothing  -> Err (Pos 0 0, R, AAAA) EmptyImage
    Just pos -> parse pos R img

contiguous :: (Image img, Eq (Pixel img)) => Pos -> img -> S.Set Pos
contiguous pos0@(Pos x0 y0) img = go (S.singleton pos0) (S.singleton pos0)
  where
    pixel0 = pixel x0 y0 img
    go acc frontier
        | S.null frontier = acc
        | otherwise       =
            let nbs =
                    S.filter (\(Pos x y) -> pixel x y img == pixel0) $
                    (`S.difference` acc) $
                    S.filter (`inside` img) $
                    S.fromList . concatMap neighbors $ S.toList frontier in
            go (S.union acc nbs) (S.difference nbs acc)
