module Turnstyle.Compile.Contaminate
    ( palette
    ) where


import qualified Codec.Picture          as JP
import           Control.Monad          (guard)
import qualified Data.Set               as S
import           Turnstyle.Compile.Expr
import           Turnstyle.Image
import           Turnstyle.JuicyPixels

palette :: Expr v -> S.Set (Pixel JuicyPixels)
palette (Import as img _) | ("contaminate", "true") `elem` as = S.fromList $ do
    y <- [0 .. height img - 1]
    x <- [0 .. width img - 1]
    let p@(JP.PixelRGBA8 _ _ _ alpha) = pixel x y img
    guard $ alpha /= 0
    pure p
palette (Import _ _ _) = mempty
palette (App _ f x) = palette f <> palette x
palette (Lam _ _ b) = palette b
palette (Var _) = mempty
palette (Prim _) = mempty
palette (Lit _ _) = mempty
