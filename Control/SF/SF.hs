module Control.SF.SF (nth, run, unfold, SF(..)) where
import Control.Category
import Prelude hiding ((.), id, exp, init)
import GHC.Exts
import Control.Arrow
import Control.ArrowInit
import Control.ArrowInit.ArrowP

instance ArrowInitP SF p

{-# RULES
"cca/first/arr"          forall f . sfFirst (sfArr f) = sfArr (first f)
"cca/second/arr"         forall f . sfSecond (sfArr f) = sfArr (second f)
"cca/compose/first"      forall f g . (sfFirst f) `sfCompose` (sfFirst g) = sfFirst (f `sfCompose` g)
"cca/compose/second"     forall f g . (sfSecond f) `sfCompose` (sfSecond g) = sfSecond (f `sfCompose` g)
"cca/compose/arr"        forall f g. sfArr g `sfCompose` sfArr f = sfArr (g . f)

"cca/commutativity"      forall f g . sfSecond g `sfCompose` sfFirst f = sfFirst f `sfCompose` sfSecond g
"cca/product"            forall i j. sfInit i *** sfInit j = sfInit (i, j)

"cca/left tightening"    forall f i g. sfLoopD i g `sfCompose` sfArr f = sfLoopD i (g . cross f id)
"cca/right tightening"   forall f i g. sfArr g `sfCompose` sfLoopD i f = sfLoopD i (cross g id . f)
"cca/sequencing"         forall f i g j. sfLoopD j g `sfCompose` sfLoopD i f =
  sfLoopD (i,j) (assoc' (juggle' (g `cross` id) . (f `cross` id)))
"cca/extension"          forall f. sfFirst (sfArr f) = sfArr (f `cross` id)
"cca/superposing"        forall i f. sfFirst (sfLoopD i f) = sfLoopD i (juggle' (f `cross` id))
"cca/loop-extension"     forall f. sfLoop (sfArr f) = sfArr (trace f)
"cca/vanishing"          forall f i. sfLoop (sfLoopD i f) = sfLoopD i (trace (juggle' f))
"cca/sfInit"             forall i. sfInit i = sfLoopD i swap

"ccatuple/nth"           forall d i f. nth d (sfLoopD i f) = nth' d (i, f)
"ccatuple/run"           forall i f l. run (sfLoopD i f) l = run' (i,f) l

"polykiller/compose"     forall g f. g . f = g `sfCompose` f
"polykiller/>>>"         forall g f. f >>> g = g `sfCompose` f
"polykiller/arr"         forall f. arr f = sfArr f
"polykiller/first"       forall f. first f = sfFirst f
"polykiller/second"      forall f. second f = sfSecond f
"polykiller/loop"        forall f. loop f = sfLoop f
"polykiller/loopD"       forall f. loopD f = sfLoopD f
"polykiller/init"        forall f. init f = sfInit f

"inline/second"          forall f . sfSecond f = sfArr swap `sfCompose` sfFirst f `sfCompose` sfArr swap
   #-}

dup x = (x, x)
swap (x, y) = (y, x)
unassoc (x, (y, z)) = ((x, y), z)
assoc ((x, y), z) = (x, (y, z))
assoc' f = assoc . f . unassoc
juggle ((x, y), z) = ((x, z), y)
juggle' f = juggle . f . juggle
trace f x = let (y, z) = f (x, z) in y
cross f g (x, y) = (f x, g y)

newtype SF a b = SF { runSF :: (a -> (b, SF a b)) }

instance Category SF where
  id = SF h where h x = (x, SF h)
  g . f = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g y
        in f' `seq` g' `seq` (z, SF (h f' g'))

instance Arrow SF where
  arr f = g where g = SF (\x -> (f x, g))
  first f = SF (g f)
    where
      g f (x, z) = f' `seq` ((y, z), SF (g f'))
        where (y, f') = runSF f x
  f &&& g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g x 
        in ((y, z), SF (h f' g'))
  f *** g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f (fst x)
            (z, g') = runSF g (snd x) 
        in ((y, z), SF (h f' g'))

instance ArrowLoop SF where
  loop sf = SF (g sf)
    where
      g f x = f' `seq` (y, SF (g f'))
        where ((y, z), f') = runSF f (x, z)

instance ArrowChoice SF where
   left sf = SF (g sf)
       where 
         g f x = case x of
                   Left a -> let (y, f') = runSF f a in f' `seq` (Left y, SF (g f'))
                   Right b -> (Right b, SF (g f))

instance ArrowInit SF where
  init = sfInit
  loopD = sfLoopD

sfCompose :: SF b c -> SF a b -> SF a c
{-# NOINLINE sfCompose #-}
sfCompose g f = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g y
        in f' `seq` g' `seq` (z, SF (h f' g'))

sfArr :: (a -> b) -> SF a b
{-# NOINLINE sfArr #-}
sfArr f = g where g = SF (\x -> (f x, g))

sfLoop :: SF (b, d) (c, d) -> SF b c
{-# NOINLINE sfLoop #-}
sfLoop sf = SF (g sf)
    where
      g f x = f' `seq` (y, SF (g f'))
        where ((y, z), f') = runSF f (x, z)

sfFirst :: SF b c -> SF (b,d) (c,d)
{-# NOINLINE sfFirst #-}
sfFirst f = SF (g f)
    where
      g f (x, z) = f' `seq` ((y, z), SF (g f'))
        where (y, f') = runSF f x

sfInit :: b -> SF b b
{-# NOINLINE sfInit #-}
sfInit i = SF (f i)
  where f i x = (i, SF (f x))

sfLoopD :: d -> ((b,d) -> (c,d)) -> SF b c
{-# NOINLINE sfLoopD #-}
sfLoopD i g = SF (f i)
  where
    f i x = 
      let (y, i') = g (x, i)
      in (y, SF (f i'))

run :: SF a b -> [a] -> [b]
{-# NOINLINE run #-}
run (SF f) (x:xs) =
  let (y, f') = f x 
  in y `seq` f' `seq` (y : run f' xs)

run' :: (d, ((b,d) -> (c,d))) -> [b] -> [c]
{-# INLINE run' #-}
run' (i,f) = g i where g i (x:xs) = let (y,i') = f (x,i) in y : g i' xs

unfold :: SF () a -> [a]
unfold = flip run inp
  where inp = () : inp

nth :: Int -> SF () a -> a
{-# NOINLINE nth #-}
nth n (SF f) = x `seq` if n == 0 then x else nth (n - 1) f'
  where (x, f') = f ()

nth' :: Int -> (b, ((), b) -> (a, b)) -> a
{-# INLINE nth' #-}
nth' !n (i, f) = n `seq` i `seq` f `seq` aux n i
  where
    aux !n !i = x `seq` i' `seq` if n == 0 then x else aux (n-1) i'
      where (x, i') = f ((), i)

sfSecond :: SF b c -> SF (d,b) (d,c)
sfSecond f = sfArr swap >>> sfFirst f >>> sfArr swap


