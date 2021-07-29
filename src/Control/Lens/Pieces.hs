{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Control.Lens.Pieces ( pieces, piecesOf ) where

import GHC.Generics as G

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a 

class GEach s a | s -> a where
  geach :: Applicative f => (a -> f a) -> s x -> f (s x)

instance GEach (K1 _1 a) a where
  geach f (K1 a) = K1 <$> f a

instance (GEach s a, GEach s' a) => GEach (s :*: s') a where
  geach f (s :*: s') = (:*:) <$> geach f s <*> geach f s'

instance GEach s a => GEach (M1 _x _y s) a where
  geach f (M1 s) = M1 <$> geach f s

genericEach :: (Generic s, GEach (Rep s) a) => Traversal' s a
genericEach f s = G.to <$> geach f (G.from s)

-- | A 'Traversal' that traverses all the components of a type, as long as all the components have the same type.
-- For a more general traversal, use 'piecesOf'.
pieces :: (Generic s, GEach (Rep s) a) => Traversal' s a
pieces = genericEach

class GEachOf s a where
  geachOf :: Applicative f => (a -> f a) -> s x -> f (s x)

instance GEachOf (K1 _1 a) a where
  geachOf f (K1 a) = K1 <$> f a

instance {-# OVERLAPPABLE #-} GEachOf (K1 _1 b) a where
  geachOf f = pure

instance (GEachOf s a, GEachOf s' a) => GEachOf (s :*: s') a where
  geachOf f (s :*: s') = (:*:) <$> geachOf f s <*> geachOf f s'

instance GEachOf s a => GEachOf (M1 _x _y s) a where
  geachOf f (M1 s) = M1 <$> geachOf f s

genericEachOf :: (Generic s, GEachOf (Rep s) a) => Traversal' s a
genericEachOf f s = G.to <$> geachOf f (G.from s)

-- | Like 'piecesOf', but uses a proxy argument instead of TypeApplications.
piecesOf' :: (Generic s, GEachOf (Rep s) a) => proxy a -> Traversal' s a
piecesOf' _ = genericEachOf

-- | Traverse all the components of a given type.
piecesOf :: forall a s. (Generic s, GEachOf (Rep s) a) => Traversal' s a
piecesOf = genericEachOf