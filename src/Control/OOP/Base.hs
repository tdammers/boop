{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE RankNTypes #-}

module Control.OOP.Base
where

-- | The instance-of typeclass. @a :> b@ means that @a@ is an instance of @b@.
class a :> b where
  cast :: a -> b

-- | Every interface is trivially an instance of itself.
instance a :> a where
  cast = id

-- | Member accessor: get an object member through an interface.
member :: cls :> inst
     => (inst -> inst -> a)
     -> cls
     -> a
member p obj =
  imember p (cast obj)

imember :: (inst -> inst -> a)
        -> inst
        -> a
imember p vt =
  p vt vt

-- | Applicative accessor for a pure member.
pureMember :: (cls :> inst, Applicative m)
         => (inst -> inst -> a)
         -> cls
         -> m a
pureMember p = pure . member p

-- | Accessing a pure member through a 'Functor'
mapMember :: (cls :> inst, Functor m)
         => (inst -> inst -> a)
         -> m cls
         -> m a
mapMember p = fmap (member p)

-- | Flipped operator alias for 'member'.
(-->) :: cls :> inst
      => cls
      -> (inst -> inst -> a)
      -> a
(-->) = flip member
infixl 8 -->

-- | Flipped operator alias for 'imember'.
(==>) :: inst
      -> (inst -> inst -> a)
      -> a
(==>) = flip imember
infixl 8 ==>

-- | Flipped operator alias for 'pureMember'.
(-->>) :: (cls :> inst, Applicative m)
      => cls
      -> (inst -> inst -> a)
      -> m a
(-->>) = flip pureMember
infixl 8 -->>

-- | Flipped operator alias for 'mapMember'.
(>-->) :: (cls :> inst, Functor m)
      => m cls
      -> (inst -> inst -> a)
      -> m a
(>-->) = flip mapMember
infixl 8 >-->

