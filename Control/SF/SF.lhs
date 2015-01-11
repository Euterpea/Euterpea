> {-# LANGUAGE CPP, TemplateHaskell, BangPatterns, FlexibleInstances, MultiParamTypeClasses #-}

> module Control.SF.SF where

#if __GLASGOW_HASKELL__ >= 610
> import Control.Category
> import Prelude hiding ((.), init, exp)
#else
> import Prelude hiding (init, exp)
#endif

> import Control.Arrow
> import Control.CCA.Types
> import Control.CCA.ArrowP
> import Control.Arrow.Operations


> newtype SF a b = SF { runSF :: (a -> (b, SF a b)) }

> instance ArrowInitP SF p

#if __GLASGOW_HASKELL__ >= 610
> instance Category SF where
>   id = SF h where h x = (x, SF h)
>   g . f = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f x
>             (z, g') = runSF g y
>         in f' `seq` g' `seq` (z, SF (h f' g'))

> instance Arrow SF where
>   arr f = g
>     where g = SF (\x -> (f x, g))
>   first f = SF (g f)
>     where
>       g f (x, z) = f' `seq` ((y, z), SF (g f'))
>         where (y, f') = runSF f x
>   f &&& g = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f x
>             (z, g') = runSF g x 
>         in ((y, z), SF (h f' g'))
>   f *** g = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f (fst x)
>             (z, g') = runSF g (snd x) 
>         in ((y, z), SF (h f' g'))
#else
> instance Arrow SF where
>   arr f = g
>     where g = SF (\x -> (f x, g))
>   f >>> g = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f x
>             (z, g') = runSF g y
>         in (z, SF (h f' g'))
>   first f = SF (g f)
>     where
>       g f (x, z) = ((y, z), SF (g f'))
>         where (y, f') = runSF f x
>   f &&& g = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f x
>             (z, g') = runSF g x 
>         in ((y, z), SF (h f' g'))
>   f *** g = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f (fst x)
>             (z, g') = runSF g (snd x) 
>         in ((y, z), SF (h f' g'))
#endif

> instance ArrowLoop SF where
>   loop sf = SF (g sf)
>     where
>       g f x = f' `seq` (y, SF (g f'))
>         where ((y, z), f') = runSF f (x, z)

> instance ArrowChoice SF where
>    left sf = SF (g sf)
>        where 
>          g f x = case x of
>                    Left a -> let (y, f') = runSF f a in f' `seq` (Left y, SF (g f'))
>                    Right b -> (Right b, SF (g f))
> 
> instance ArrowInit SF where
>   init i = SF (f i)
>     where f i x = (i, SF (f x))
>   loopD i g = SF (f i)
>     where
>       f i x = 
>         let (y, i') = g (x, i)
>         in (y, SF (f i'))

> instance ArrowCircuit SF where
>   delay = init

> run :: SF a b -> [a] -> [b]
> run _ [] = []
> run (SF f) (x:xs) =
>   let (y, f') = f x 
>   in y `seq` f' `seq` (y : run f' xs)
> 
> unfold :: SF () a -> [a]
> unfold = flip run inp
>   where inp = () : inp
>
> 
> nth :: Int -> SF () a -> a
> nth n (SF f) = x `seq` if n == 0 then x else nth (n - 1) f'
>   where (x, f') = f ()
> 
> nth' :: Int -> (b, ((), b) -> (a, b)) -> a
> nth' !n (i, f) = n `seq` i `seq` f `seq` aux n i
>   where
>     aux !n !i = x `seq` i' `seq` if n == 0 then x else aux (n-1) i'
>       where (x, i') = f ((), i)
> 


