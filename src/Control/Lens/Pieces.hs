{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE EmptyCase #-}
-- | Generic traversals on the fly using "GHC.Generics".
module Control.Lens.Pieces 
  ( -- * Types
    Traversal
  , Traversal'
    -- * Traversals 
  , pieces
  , piecesOf
  , piecesOf' 
    -- * Type Classes
  , GEach(..)
  , GEachOf(..)
  ) where

import GHC.Generics

-- | A Van Laarhoven-style traversal.
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
-- | A simple 'Traversal'.
type Traversal' s a = Traversal s s a a 

-- | Generate a 'Traversal' for any homogenous type.
class GEach s a | s -> a where
  -- | Traverse the representation of @s@.
  geach :: Applicative f => (a -> f a) -> s x -> f (s x)

instance GEach (K1 _1 a) a where
  geach f (K1 a) = K1 <$> f a

instance (GEach s a, GEach s' a) => GEach (s :*: s') a where
  geach f (s :*: s') = (:*:) <$> geach f s <*> geach f s'

instance GEach s a => GEach (M1 _x _y s) a where
  geach f (M1 s) = M1 <$> geach f s

genericEach :: (Generic s, GEach (Rep s) a) => Traversal' s a
genericEach f s = to <$> geach f (from s)

-- | A 'Traversal' that traverses all the components of a type, as long as all the components have the same type.
-- For a more general traversal, use 'piecesOf'.
--
-- >>> ('a','b','c') & pieces %~ toUpper
-- ('A','B','C')
pieces :: (Generic s, GEach (Rep s) a) => Traversal' s a
pieces = genericEach

-- | Generate many 'Traversal's for any type.
class GEachOf s a where
  -- | Traverse the representation of @s@.
  geachOf :: Applicative f => (a -> f a) -> s x -> f (s x)

instance (GEach s a, GEach s' a) => GEach (s :+: s') a where
  geach f (L1 s) = L1 <$> geach f s
  geach f (R1 s) = R1 <$> geach f s

instance GEachOf V1 a where
  geachOf _f v = case v of

instance GEachOf U1 a where
  geachOf _f U1 = pure U1

instance GEachOf (K1 _1 a) a where
  geachOf f (K1 a) = K1 <$> f a

instance {-# OVERLAPPABLE #-} GEachOf (K1 _1 b) a where
  geachOf _f = pure

instance (GEachOf s a, GEachOf s' a) => GEachOf (s :*: s') a where
  geachOf f (s :*: s') = (:*:) <$> geachOf f s <*> geachOf f s'

instance (GEachOf s a, GEachOf s' a) => GEachOf (s :+: s') a where
  geachOf f (L1 s) = L1 <$> geachOf f s
  geachOf f (R1 s) = R1 <$> geachOf f s

instance GEachOf s a => GEachOf (M1 _x _y s) a where
  geachOf f (M1 s) = M1 <$> geachOf f s

genericEachOf :: (Generic s, GEachOf (Rep s) a) => Traversal' s a
genericEachOf f s = to <$> geachOf f (from s)

-- | Like 'piecesOf', but uses a proxy argument instead of TypeApplications.
piecesOf' :: (Generic s, GEachOf (Rep s) a) => proxy a -> Traversal' s a
piecesOf' _ = genericEachOf

-- | Traverse all the components of a given type.
-- 
-- >>> ('a','b','c',True,"hello") & piecesOf @Char %~ toUpper
-- ('A','B','C',True,"hello")
piecesOf :: forall a s. (Generic s, GEachOf (Rep s) a) => Traversal' s a
piecesOf = genericEachOf