> {-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

> module Control.Arrow.ArrowP where

> import Control.Arrow
> import Control.Arrow.Operations
#if __GLASGOW_HASKELL__ >= 610
> import Control.Category
> import Prelude hiding ((.), id)
#endif

> newtype ArrowP a p b c = ArrowP { strip :: a b c }

#if __GLASGOW_HASKELL__ >= 610
> instance Category a => Category (ArrowP a p) where
>   id = ArrowP id
>   ArrowP g . ArrowP f = ArrowP (g . f)

> instance Arrow a => Arrow (ArrowP a p) where
>   arr f = ArrowP (arr f)
>   first (ArrowP f) = ArrowP (first f)
#else
> instance Arrow a => Arrow (ArrowP a p) where
>   arr f = ArrowP (arr f)
>   first (ArrowP f) = ArrowP (first f)
>   ArrowP f >>> ArrowP g = ArrowP (f >>> g)
#endif

> instance ArrowLoop a => ArrowLoop (ArrowP a p) where
>   loop (ArrowP f) = ArrowP (loop f)

> instance ArrowCircuit a => ArrowCircuit (ArrowP a p) where
>   delay i = ArrowP (delay i)

> instance ArrowChoice a => ArrowChoice (ArrowP a p) where
>   left (ArrowP f) = ArrowP (left f)
>   ArrowP f ||| ArrowP g = ArrowP (f ||| g)

