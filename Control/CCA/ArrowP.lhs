> {-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

> module Control.CCA.ArrowP where

> import Control.Arrow 
> import Control.CCA.Types
> import Control.CCA.CCNF
> import Language.Haskell.TH
> import Prelude hiding (init, (.), id)

#if __GLASGOW_HASKELL__ >= 610

> import Control.Category
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


> newtype ArrowP a p b c = ArrowP (a b c)

> class (ArrowInit (ArrowP a p), ArrowInit a) => ArrowInitP a p where
>   strip :: ArrowP a p b c -> a b c
>   strip (ArrowP f) = f

> instance ArrowLoop a => ArrowLoop (ArrowP a p) where
>   loop (ArrowP f) = ArrowP (loop f)

> instance ArrowInit a => ArrowInit (ArrowP a p) where
>   init i = ArrowP (init i) -- error "use init' instead"
>   arr' f f' = ArrowP (arr' f f')
>   init' i i' = ArrowP (init' i i')

> instance ArrowChoice a => ArrowChoice (ArrowP a p) where
>   left (ArrowP f) = ArrowP (left f)
>   ArrowP f ||| ArrowP g = ArrowP (f ||| g)

> instance ArrowInitP ASyn p

> normP :: ArrowP ASyn p b c -> ExpQ
> normP (ArrowP x) = norm x

> normOptP :: ArrowP ASyn p b c -> ExpQ
> normOptP x = normOpt (strip x)


