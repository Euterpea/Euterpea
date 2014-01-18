{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, ExistentialQuantification, TemplateHaskell, Arrows #-}
module Euterpea.IO.Audio.Basics
       (outA, integral, countDown, countUp, upsample, pchToHz, apToHz)
       where
import Prelude hiding (init)
import Euterpea.Music.Note.Music
import Euterpea.IO.Audio.Types
import Control.Arrow
import Control.CCA.ArrowP
import Control.CCA.Types
import Control.CCA.CCNF
import Language.Haskell.TH
import Language.Haskell.TH.Instances
import Language.Haskell.TH.Syntax
 
outA :: forall a b . (ArrowInit a) => a b b
outA = arr' [| id |] id
 
integral ::
         forall a p . (ArrowInitP a p, Clock p) => ArrowP a p Double Double
integral
  = let dt = 1 / rate (undefined :: p) in
      (loop
         ((arr' [| (\ (x, i) -> let i' = i + x * dt in i') |]
             (\ (x, i) -> let i' = i + x * dt in i')
             >>> init' [| 0 |] 0)
            >>> arr' [| (\ i -> (i, i)) |] (\ i -> (i, i)))
         >>> outA)
 
countDown :: forall a . (ArrowInit a) => Int -> a () Int
countDown x
  = (loop
       (arr' [| (\ (_, i) -> i - 1) |] (\ (_, i) -> i - 1) >>>
          (init' [| x |] x >>> arr' [| (\ i -> (i, i)) |] (\ i -> (i, i))))
       >>> outA)
 
countUp :: forall a . (ArrowInit a) => a () Int
countUp
  = (loop
       (arr' [| (\ (_, i) -> i + 1) |] (\ (_, i) -> i + 1) >>>
          (init' [| 0 |] 0 >>> arr' [| (\ i -> (i, i)) |] (\ i -> (i, i))))
       >>> outA)
 
upsample ::
         forall a p1 p2 b x .
           (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1,
            Clock p2, AudioSample b) =>
           ArrowP a p1 x b -> ArrowP a p2 x b
upsample f = g
  where g = (loop
               (arr' [| (\ (x, ~(cc, y)) -> (cc, (x, y))) |]
                  (\ (x, ~(cc, y)) -> (cc, (x, y)))
                  >>>
                  (first
                     (arr' [| (\ cc -> if cc >= r - 1 then 0 else cc + 1) |]
                        (\ cc -> if cc >= r - 1 then 0 else cc + 1)
                        >>> init' [| 0 |] 0)
                     >>>
                     arr' [| (\ (cc, (x, y)) -> ((cc, x, y), cc)) |]
                       (\ (cc, (x, y)) -> ((cc, x, y), cc)))
                  >>>
                  (first
                     (arr' [| (\ (cc, x, y) -> if cc == 0 then Left x else Right y) |]
                        (\ (cc, x, y) -> if cc == 0 then Left x else Right y)
                        >>> (ArrowP (strip f) ||| init' [| zero |] zero))
                     >>>
                     arr' [| (\ (y, cc) -> (y, (cc, y))) |]
                       (\ (y, cc) -> (y, (cc, y)))))
               >>> outA)
        r = if outRate < inRate then
              error "Cannot upsample a signal of higher rate to lower rate" else
              outRate / inRate
        inRate = rate (undefined :: p1)
        outRate = rate (undefined :: p2)
 
apToHz :: forall a . (Floating a) => AbsPitch -> a
apToHz ap = 440 * 2 ** (fromIntegral (ap - absPitch (A, 5)) / 12)
 
pchToHz :: forall a . (Floating a) => Pitch -> a
pchToHz = apToHz . absPitch