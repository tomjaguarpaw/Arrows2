{-# LANGUAGE Arrows #-}

import qualified Control.Arrow
import qualified Control.Category
import Control.Arrow (Arrow, returnA)

type Control arr e r u v a b = (u, arr e (a, r)) -> (v, arr e (b, r))

(<<<) :: Control arr e r v w b c
      -> Control arr e r u v a b
      -> Control arr e r u w a c
(<<<) = (.)

arr1 :: (u -> v) -> Control arr e r u v a a
arr1 f (p, a) = (f p, a)

arr2 :: Arrow arr => (a -> b) -> Control arr e r u u a b
arr2 f (p, a) =
  (p, a Control.Arrow.>>> Control.Arrow.arr (Control.Arrow.first f))

first1 :: Control arr e r u v a b
       -> Control arr e r (u, w) (v, w) a b
first1 f ((u, w), a) = ((v, w), b)
  where (v, b) = f (u, a)

first2 :: Arrow arr
       => Control arr e (c, r) u v a b
       -> Control arr e r u v (a, c) (b, c)
first2 f (p, a) =
  Control.Arrow.second (Control.Arrow.>>> Control.Arrow.arr assocr)
  (f (p, a Control.Arrow.>>> Control.Arrow.arr assocl))

assocl :: ((a, c), r) -> (a, (c, r))
assocl ((a, c), r) = (a, (c, r))

assocr :: (a, (c, r)) -> ((a, c), r)
assocr (a, (c, r)) = ((a, c), r)

apply :: Arrow arr => Control arr e r (arr (e, a) b) () a b
apply (ea_b, e_ar) = ((), proc e -> do
  (a, r) <- e_ar -< e
  b <- ea_b -< (e, a)
  returnA -< (b, r))

letA :: Arrow arr => Control arr e r (arr (e, a) b) (arr (e, p) b) (p -> a) ()
letA (ea_b, e_ptar) =
  (proc (e, p) -> do
      (pta, r) <- e_ptar -< e
      b <- ea_b -< (e, pta p)
      returnA -< b
  , e_ptar Control.Arrow.>>> Control.Arrow.first (Control.Arrow.arr (const ())))

runA :: Arrow arr => Control arr a () () () a b -> arr a b
runA f = snd (f ((), Control.Arrow.arr (\x -> (x, ()))))
         Control.Arrow.>>> Control.Arrow.arr (\(x, ()) -> x)
