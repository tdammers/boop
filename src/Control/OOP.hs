{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE RankNTypes #-}

module Control.OOP
(
-- * Classes, Methods, and Interfaces
  (:>) (..)

-- * Properties / Fields / Methods
, member
, imember
, pureMember
, mapMember
, (-->)
, (==>)

-- * Mutable State
, MonadConstVar (..)
, MonadVar (..)

-- * Standard interfaces
, ToString (..)
)
where

import Control.OOP.Base
import Control.OOP.MonadVar
import Control.OOP.Interfaces.ToString
