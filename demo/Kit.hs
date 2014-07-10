{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Kit where

import Control.Applicative

import           Data.Vinyl                as V
import           Data.Vinyl.Functor        as V
import           Data.Vinyl.Idiom.Identity as V
import           Data.Vinyl.TyFun          as V
import qualified Data.Aeson                as A
import qualified Data.Aeson.Types          as A

newtype Compose f g a = Compose { runCompose :: f (g a) }

type family TotalE (el :: TyFun k * -> *) :: [k]

type JSONRenderer = V.Lift (->) V.Identity (Const A.Pair)

liftRenderer :: (a -> A.Pair) -> JSONRenderer a
liftRenderer f = Lift $ Const . f . runIdentity

renderJSON :: (tot ~ TotalE el, tot <: rs) => Rec el JSONRenderer tot -> PlainRec el rs -> A.Value
renderJSON renderer rec = A.object . rgetConsts $ cast renderer <<*>> rec

rgetConsts :: Rec el (Const t) rs -> [t]
rgetConsts RNil = []
rgetConsts (Const x :& rs) = x : rgetConsts rs

(<~#=) :: Applicative g => (forall a. h a -> g (f a)) -> Rec el h rs -> g (Rec el f rs)
op <~#= rec = rtraverse runCompose $ Compose . op <<$>> rec
infixl 8 <~#=

