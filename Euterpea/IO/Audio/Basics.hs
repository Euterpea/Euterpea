{-# LANGUAGE Arrows, TemplateHaskell, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables #-}

-- This file is a generated file from Basics.as

{-# LINE 6 "Basics.as" #-}
module Euterpea.IO.Audio.Basics
       (outA, integral, countDown, countUp, upsampleH, upsampleH',
        upsampleI, upsample, pchToHz, apToHz)
       where
{-# LINE 14 "Basics.as" #-}
import Prelude hiding (init)
{-# LINE 16 "Basics.as" #-}
import Euterpea.Music.Note.Music
{-# LINE 17 "Basics.as" #-}
import Euterpea.IO.Audio.Types
{-# LINE 18 "Basics.as" #-}
import Control.Arrow
{-# LINE 19 "Basics.as" #-}
import Control.CCA.ArrowP
{-# LINE 20 "Basics.as" #-}
import Control.CCA.Types
{-# LINE 21 "Basics.as" #-}
import Control.CCA.CCNF
{-# LINE 23 "Basics.as" #-}
import Language.Haskell.TH
{-# LINE 24 "Basics.as" #-}
import Language.Haskell.TH.Instances
{-# LINE 25 "Basics.as" #-}
import Language.Haskell.TH.Syntax
 
{-# LINE 28 "Basics.as" #-}
outA :: forall a b. (ArrowInit a) => a b b
{-# LINE 29 "Basics.as" #-}
outA = arr' [| id |] id
 
{-# LINE 32 "Basics.as" #-}
integral :: forall a p. (ArrowInitP a p, Clock p) => ArrowP a p Double Double
{-# LINE 33 "Basics.as" #-}
integral
  = (loop
       ((arr'
           [|
             (\ (x, i) ->
                let {-# LINE 34 "Basics.as" #-}
                    i' = i + x * dt
                  in i')
             |]
           (\ (x, i) ->
              let {-# LINE 34 "Basics.as" #-}
                  i' = i + x * dt
                in i')
           >>> init' [| 0 |] 0)
          >>> arr' [| (\ i -> (i, i)) |] (\ i -> (i, i)))
       >>> outA)
  where {-# LINE 37 "Basics.as" #-}
        dt = 1 / rate (undefined :: p)
 
{-# LINE 40 "Basics.as" #-}
countDown :: forall a. (ArrowInit a) => Int -> a () Int
{-# LINE 41 "Basics.as" #-}
countDown x
  = (loop
       (arr' [| (\ (_, i) -> i - 1) |] (\ (_, i) -> i - 1) >>>
          (init' [| x |] x >>> arr' [| (\ i -> (i, i)) |] (\ i -> (i, i))))
       >>> outA)
 
{-# LINE 46 "Basics.as" #-}
countUp :: forall a. (ArrowInit a) => a () Int
{-# LINE 47 "Basics.as" #-}
countUp
  = (loop
       (arr' [| (\ (_, i) -> i + 1) |] (\ (_, i) -> i + 1) >>>
          (init' [| 0 |] 0 >>> arr' [| (\ i -> (i, i)) |] (\ i -> (i, i))))
       >>> outA)
 
{-# LINE 56 "Basics.as" #-}
upsampleH :: forall a p1 p2 x b. 
          (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1,
           Clock p2, AudioSample b) =>
            ArrowP a p1 x b -> ArrowP a p2 x b
{-# LINE 58 "Basics.as" #-}
upsampleH f
  = ((loop
        (arr' [| (\ (x, cc) -> (cc, x)) |] (\ (x, cc) -> (cc, x)) >>>
           (first
              (arr' [| (\ cc -> if cc >= r - 1 then 0 else cc + 1) |]
                 (\ cc -> if cc >= r - 1 then 0 else cc + 1)
                 >>> init' [| 0 |] 0)
              >>>
              arr' [| (\ (cc, x) -> ((cc, x), cc)) |]
                (\ (cc, x) -> ((cc, x), cc))))
        >>>
        arr' [| (\ (cc, x) -> (x, cc == 0)) |] (\ (cc, x) -> (x, cc == 0)))
       >>>
       periodicProbe zero (init' [| zero |] zero <<< ArrowP (strip f)))
  where {-# LINE 64 "Basics.as" #-}
        r = getUpsampleR (undefined :: p1) (undefined :: p2)
 
{-# LINE 68 "Basics.as" #-}
upsampleH' :: forall a p1 p2 x b. 
           (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1,
            Clock p2, AudioSample b) =>
             ArrowP a p1 x b -> ArrowP a p2 x b
{-# LINE 70 "Basics.as" #-}
upsampleH' f
  = ((loop
        (arr' [| (\ (x, cc) -> (cc, x)) |] (\ (x, cc) -> (cc, x)) >>>
           (first
              (arr' [| (\ cc -> if cc >= r - 1 then 0 else cc + 1) |]
                 (\ cc -> if cc >= r - 1 then 0 else cc + 1)
                 >>> init' [| 0 |] 0)
              >>>
              arr' [| (\ (cc, x) -> ((cc, x), cc)) |]
                (\ (cc, x) -> ((cc, x), cc))))
        >>>
        arr' [| (\ (cc, x) -> (x, cc == 0)) |] (\ (cc, x) -> (x, cc == 0)))
       >>> periodicProbe zero (ArrowP (strip f)))
  where {-# LINE 73 "Basics.as" #-}
        r = getUpsampleR (undefined :: p1) (undefined :: p2)
 
{-# LINE 81 "Basics.as" #-}
upsampleI :: forall a p1 p2 x b. 
          (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1,
           Clock p2, AudioSample b, Fractional b) =>
            ArrowP a p1 x b -> ArrowP a p2 x b
{-# LINE 83 "Basics.as" #-}
upsampleI f
  = ((loop
        (arr' [| (\ (x, cc) -> (cc, x)) |] (\ (x, cc) -> (cc, x)) >>>
           (first
              (arr' [| (\ cc -> if cc >= r - 1 then 0 else cc + 1) |]
                 (\ cc -> if cc >= r - 1 then 0 else cc + 1)
                 >>> init' [| 0 |] 0)
              >>>
              arr' [| (\ (cc, x) -> ((cc, x), cc)) |]
                (\ (cc, x) -> ((cc, x), cc))))
        >>>
        arr' [| (\ (cc, x) -> ((cc, x), cc)) |]
          (\ (cc, x) -> ((cc, x), cc)))
       >>>
       (first
          (arr' [| (\ (cc, x) -> (x, cc == 0)) |] (\ (cc, x) -> (x, cc == 0))
             >>> periodicProbe zero (ArrowP (strip f)))
          >>>
          arr' [| (\ (now, cc) -> ((cc, now), (cc, now))) |]
            (\ (now, cc) -> ((cc, now), (cc, now))))
         >>>
         (first
            (arr' [| (\ (cc, now) -> (now, cc == 0)) |]
               (\ (cc, now) -> (now, cc == 0))
               >>> periodicProbe zero (init' [| zero |] zero))
            >>>
            arr'
              [|
                (\ (prev, (cc, now)) ->
                   prev + (now - prev) * (realToFrac $ cc / r))
                |]
              (\ (prev, (cc, now)) ->
                 prev + (now - prev) * (realToFrac $ cc / r)))
           >>> outA)
  where {-# LINE 88 "Basics.as" #-}
        r = getUpsampleR (undefined :: p1) (undefined :: p2)
 
{-# LINE 91 "Basics.as" #-}
upsample :: forall a p1 p2 x b. 
         (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1,
          Clock p2, AudioSample b) =>
           ArrowP a p1 x b -> ArrowP a p2 x b
{-# LINE 93 "Basics.as" #-}
upsample = upsampleH
 
{-# LINE 96 "Basics.as" #-}
getUpsampleR :: forall p1 p2 . (Clock p1, Clock p2) => p1 -> p2 -> Double
{-# LINE 97 "Basics.as" #-}
getUpsampleR p1 p2
  = if outRate < inRate then
      error $ "Cannot upsample a signal of higher rate (" ++ show inRate
        ++ ") to lower rate ("
        ++ show outRate
        ++ ")"
      else outRate / inRate
  where {-# LINE 101 "Basics.as" #-}
        inRate = rate p1
        {-# LINE 102 "Basics.as" #-}
        outRate = rate p2
 
{-# LINE 112 "Basics.as" #-}
periodicProbe :: forall a x y . 
              (ArrowChoice a, ArrowInit a, AudioSample y) => y -> a x y -> a (x, Bool) y
{-# LINE 113 "Basics.as" #-}
periodicProbe i a
  = ((loop
        (arr' [| (\ ((x, b), t1) -> ((b, x), (b, t1))) |]
           (\ ((x, b), t1) -> ((b, x), (b, t1)))
           >>>
           (first
              (arr' [| (\ (b, x) -> if b then Left x else Right ()) |]
                 (\ (b, x) -> if b then Left x else Right ())
                 >>> (a ||| (arr . const) i))
              >>>
              arr' [| (\ (y, (b, t1)) -> ((b, t1, y), (b, y))) |]
                (\ (y, (b, t1)) -> ((b, t1, y), (b, y))))
             >>>
             (first
                (arr' [| (\ (b, t1, y) -> if b then y else t1) |]
                   (\ (b, t1, y) -> if b then y else t1)
                   >>> init' [| zero |] i)
                >>>
                arr' [| (\ (t1, (b, y)) -> ((b, t1, y), t1)) |]
                  (\ (t1, (b, y)) -> ((b, t1, y), t1))))
        >>>
        arr' [| (\ (b, t1, y) -> if b then y else t1) |]
          (\ (b, t1, y) -> if b then y else t1))
       >>> outA)
 
{-# LINE 122 "Basics.as" #-}
apToHz :: forall a . (Floating a) => AbsPitch -> a
{-# LINE 123 "Basics.as" #-}
apToHz ap = 440 * 2 ** (fromIntegral (ap - absPitch (A, 5)) / 12)
 
{-# LINE 126 "Basics.as" #-}
pchToHz :: forall a . (Floating a) => Pitch -> a
{-# LINE 127 "Basics.as" #-}
pchToHz = apToHz . absPitch
