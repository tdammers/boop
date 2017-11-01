{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Control.OOP.Interfaces.ToString
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Control.Monad.Identity (Identity, runIdentity)

import Control.OOP.Base

type LText = LText.Text

-- | Objects that can be converted to string
data ToString m
  = ToString
      { -- | Monadically cast an object to `Text`.
        toStringM :: ToString m -> m Text
      }

-- | Cast an object to `Text` in a pure context.
toString :: ToString Identity -> ToString Identity -> Text
toString a b = runIdentity $ toStringM a b

castShow :: (Applicative m, Show a) => a -> ToString m
castShow = cast . show

instance Applicative m => Text :> ToString m where
  cast x = ToString (const . pure $ x)

instance Applicative m => LText :> ToString m where
  cast x = ToString (const . pure $ LText.toStrict x)

instance Applicative m => String :> ToString m where
  cast x = ToString (const . pure $ Text.pack x)

instance Applicative m => Int :> ToString m where
  cast = castShow

instance Applicative m => Integer :> ToString m where
  cast = castShow

instance Applicative m => Double :> ToString m where
  cast = castShow

instance Applicative m => Rational :> ToString m where
  cast = castShow
