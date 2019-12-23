{-# LANGUAGE Arrows #-}

import qualified Control.Arrow
import qualified Control.Category
import Control.Arrow (Arrow, returnA)

-- This type is used for a concept which generalizes both "control
-- operators" and basic arrow values for an Arrow `arr`
--
-- It is "Arrow in two pairs of varibles" which we might describe as
-- being a "biarrow".
--
-- `e` (the environment) and `r` (the return type) should both be kept
-- polymorphic (like the return type in Cont and ContT).
--
-- A basic arrow value of type `arr a b` is represented by a value of
-- type `Command arr e r () () a b`.
--
-- A control operator of type
--
--     arr (e, a1) b1 -> ... -> arr (e, an) bn -> arr (e, a) b
--
-- is represented by a value of type
--
--     Command arr e r (arr (e, a1) b1, ..., arr (e, an) bn)
--                     (arr (e, a) b)
--                     ()
--                     ()
--
-- The Control combinators `letA` and `apply` defined below don't fit
-- into either of these patterns, which is why we needed to generalize
-- the concept.
type Control arr e r u v a b = (u, arr e (a, r)) -> (v, arr e (b, r))

-- "Biarrow" composition
(<<<) :: Control arr e r v w b c
      -> Control arr e r u v a b
      -> Control arr e r u w a c
(<<<) = (.)

-- "Biarrow" arr (first pair of type variables)
arr1 :: (u -> v) -> Control arr e r u v a a
arr1 f (p, a) = (f p, a)

-- "Biarrow" arr (second pair of type variables)
arr2 :: Arrow arr => (a -> b) -> Control arr e r u u a b
arr2 f (p, a) =
  (p, a Control.Arrow.>>> Control.Arrow.arr (Control.Arrow.first f))

-- "Biarrow" first (first pair of type variables)
first1 :: Control arr e r u v a b
       -> Control arr e r (u, w) (v, w) a b
first1 f ((u, w), a) = ((v, w), b)
  where (v, b) = f (u, a)

-- "Biarrow" first (second pair of type variables)
--
-- We take a slight liberty here.  The return type changes.  I don't
-- think this actually impinges on anything we want to do with it
-- though.
--
-- In fact, the only reason that the return type `r` is needed in
-- Command at all is so that we can implement `first2`.
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

-- We use this to rewrite an arrow notation line of the form
--
--     p <- action -< e
--
-- to
--
--     () <- p <- apply -< (action <<< arr snd) -< eb
apply :: Arrow arr => Control arr e r (arr (e, a) b) () a b
apply (ea_b, e_ar) = ((), proc e -> do
  (a, r) <- e_ar -< e
  b <- ea_b -< (e, a)
  returnA -< (b, r))

-- We use this to rewrite an arrow notation line of the form
--
--     let f = \p -> g -< e
--
-- to
--
--     f <- () <- letA -< g -< (\p -> e)
--
-- (This ought to be possible since none of the variables bound by p
-- are allowed to appear free in g.)
letA :: Arrow arr => Control arr e r (arr (e, a) b) (arr (e, p) b) (p -> a) ()
letA (ea_b, e_ptar) =
  (proc (e, p) -> do
      (pta, r) <- e_ptar -< e
      b <- ea_b -< (e, pta p)
      returnA -< b
  , e_ptar Control.Arrow.>>> Control.Arrow.first (Control.Arrow.arr (const ())))

-- This ought to be a morphism of arrows
runA :: Arrow arr => Control arr a () () () a b -> arr a b
runA f = snd (f ((), Control.Arrow.arr (\x -> (x, ()))))
         Control.Arrow.>>> Control.Arrow.arr (\(x, ()) -> x)
