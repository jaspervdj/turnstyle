{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Turnstyle.Parse
  ( Ann
  , Dir (..)
  , Pos (..)
  , ParseError (..)
  , parseImage

  , initialPosition
  ) where

import qualified Data.Set           as S
import           GHC.Exception      (Exception)
import           Turnstyle.Expr
import           Turnstyle.Image
import           Turnstyle.Prim
import Data.Maybe (fromMaybe)
import           Turnstyle.Quattern (Quattern (..), quattern)

data Pos = Pos Int Int deriving (Eq, Ord, Show)

inside :: Image img => Pos -> img -> Bool
inside (Pos x y) img = x >= 0 && x < width img && y >= 0 && y < height img

initialPosition :: (Image img, Eq (Pixel img)) => img -> Maybe Pos
initialPosition img
    | width img <= 0 || height img <= 0 = Nothing
    | otherwise                         = Just $ Pos
        (fromMaybe (width  img `div` 2) $ go (move 1 R) (Pos 0 0))
        (fromMaybe (height img `div` 2) $ go (move 1 D) (Pos 0 0))
  where
    color0 = pixel 0 0 img
    go next pos@(Pos x y)
        | not (inside pos img)    = Nothing
        | color0 /= pixel x y img = Just . S.size $ flood pos img
        | otherwise               = go next (next pos)

data Dir = R | D | L | U deriving (Eq, Show)

move :: Int -> Dir -> Pos -> Pos
move n R (Pos x y) = Pos (x + n) (y    )
move n D (Pos x y) = Pos (x    ) (y + n)
move n L (Pos x y) = Pos (x - n) (y    )
move n U (Pos x y) = Pos (x    ) (y - n)

rotateLeft, rotateRight :: Dir -> Dir
rotateLeft R = U
rotateLeft D = R
rotateLeft L = D
rotateLeft U = L
rotateRight  = rotateLeft . rotateLeft . rotateLeft

-- | Selecting single pixel from the turnstyle:
--
--     L
--     CF
--     R
--
data RelPos = LeftPos | CenterPos | FrontPos | RightPos deriving (Eq, Show)

rel :: Pos -> Dir -> RelPos -> Pos
rel (Pos x y) _ CenterPos = Pos (x    ) (y    )
rel (Pos x y) R LeftPos   = Pos (x    ) (y - 1)
rel (Pos x y) R FrontPos  = Pos (x + 1) (y    )
rel (Pos x y) R RightPos  = Pos (x    ) (y + 1)
rel (Pos x y) D LeftPos   = Pos (x + 1) (y    )
rel (Pos x y) D FrontPos  = Pos (x    ) (y + 1)
rel (Pos x y) D RightPos  = Pos (x - 1) (y    )
rel (Pos x y) L LeftPos   = Pos (x    ) (y + 1)
rel (Pos x y) L FrontPos  = Pos (x - 1) (y    )
rel (Pos x y) L RightPos  = Pos (x    ) (y - 1)
rel (Pos x y) U LeftPos   = Pos (x - 1) (y    )
rel (Pos x y) U FrontPos  = Pos (x    ) (y - 1)
rel (Pos x y) U RightPos  = Pos (x + 1) (y    )

type Ann = (Pos, Dir)

data ParseError
    = OutOfBounds
    | EmptyImage
    | UnknownPrim Int Int
    deriving (Show)

instance Exception ParseError

parse
  :: forall img. (Image img, Eq (Pixel img), Show (Pixel img))
  => Pos -> Dir -> img -> Expr Ann ParseError (Pixel img)
parse pos dir img = case pattern of
    _ | not (inside (rel pos dir LeftPos) img)   -> Err ann OutOfBounds
    _ | not (inside (rel pos dir CenterPos) img) -> Err ann OutOfBounds
    _ | not (inside (rel pos dir FrontPos) img)  -> Err ann OutOfBounds
    _ | not (inside (rel pos dir RightPos) img)  -> Err ann OutOfBounds

    -- App
    ABCA -> App ann parseLeft  parseRight
    ABAC -> App ann parseLeft  parseFront
    ABCC -> App ann parseFront parseRight

    -- Lam
    --
    --     |
    --     A     A
    --     BA    BC
    --     C     C
    --           |
    --
    AABC -> Lam ann (relPixel LeftPos) parseLeft
    ABCB -> Lam ann (relPixel RightPos) parseRight

    -- Var
    --
    --     A     A
    --    +BA    AB+
    --     A     A
    ABAA -> Var ann $ relPixel CenterPos
    AABA -> Var ann $ relPixel FrontPos

    -- Int/Prim
    ABCD
        | floodLeft >= floodRight -> Lit ann $ floodFront
        | otherwise               -> case decodePrim p mode of
            Nothing   -> Err ann $ UnknownPrim p mode
            Just prim -> Prim ann prim
      where
        p    = floodRight - floodLeft
        mode = abs (floodFront - floodRight)

    -- Steer
    AAAA -> parseFront
    AAAB -> parseFront
    AABB -> parseLeft
    ABAB -> parseRight
    ABBA -> parseFront
    ABBB -> parseFront
    ABBC -> parseFront
 where
    ann        = (pos, dir)
    relPos     = rel pos dir
    relPixel r = let (Pos px py) = relPos r in pixel px py img

    pattern = quattern
        (relPixel LeftPos)
        (relPixel CenterPos)
        (relPixel FrontPos)
        (relPixel RightPos)

    parseLeft  = parse (relPos LeftPos) (rotateLeft dir) img
    parseFront = parse (relPos FrontPos) dir img
    parseRight = parse (relPos RightPos) (rotateRight dir) img

    floodLeft  = S.size $ flood (relPos LeftPos) img
    floodFront = S.size $ flood (relPos FrontPos) img
    floodRight = S.size $ flood (relPos RightPos) img

parseImage
  :: forall img. (Image img, Eq (Pixel img), Show (Pixel img))
  => img -> Expr Ann ParseError (Pixel img)
parseImage img = case initialPosition img of
    Nothing  -> Err (Pos 0 0, R) EmptyImage
    Just pos -> parse pos R img

flood :: (Image img, Eq (Pixel img)) => Pos -> img -> S.Set Pos
flood pos0@(Pos x0 y0) img = go (S.singleton pos0) (S.singleton pos0)
  where
    pixel0 = pixel x0 y0 img
    go acc frontier
        | S.null frontier = acc
        | otherwise       =
            let nbs =
                    S.filter (\(Pos x y) -> pixel x y img == pixel0) $
                    (`S.difference` acc) $
                    S.filter (`inside` img) $
                    S.fromList . concatMap neighbours $ S.toList frontier in
            go (S.union acc nbs) (S.difference nbs acc)

    neighbours pos = [move 1 d pos | d <- [U, R, D, L]]

