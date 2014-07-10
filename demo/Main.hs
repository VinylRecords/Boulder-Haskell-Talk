{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import Kit
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.IORef
import Data.Monoid
import Data.Vinyl                as V
import Data.Vinyl.Idiom.Identity as V
import Data.Vinyl.TH             as V
import Web.Scotty

import qualified Data.Aeson         as A
import           Data.Aeson         ((.=))
import qualified Data.List          as L
import qualified Data.Map           as Map
import qualified Data.Singletons    as S
import qualified Data.Singletons.TH as S
import qualified Data.Text          as T
import qualified System.Random      as SR

data SongU
  = UId
  | Name
  | Artist
  | Genre

data MusicGenre
  = Jazz
  | Classical
  deriving Show

instance Parsable MusicGenre where
  parseParam "Jazz" = Right Jazz
  parseParam "Classical" = Right Classical
  parseParam _ = Left "Invalid genre"

S.genSingletons [''SongU]
V.makeUniverse' ''SongU "ElS"

type instance TotalE ElS
  = [ UId, Name, Artist, Genre ]
type SongE
  = [ UId, Name, Artist, Genre ]
type ArtistE
  = [ UId, Name ]

type Pointer rs
  = Either
      (PlainRec ElS '[UId])
      (PlainRec ElS rs)

V.semantics ''ElS
  [ 'UId :~> ''T.Text
  , 'Name :~> ''T.Text
  , 'Artist :~> [t| Pointer ArtistE |]
  , 'Genre :~> ''MusicGenre
  ]


steveSwallow :: PlainRec ElS ArtistE
steveSwallow
   =  SUId  =: "234234adsf"
  <+> SName =: "Steve Swallow"

fallingGrace :: PlainRec ElS SongE
fallingGrace
   =  SUId    =: "12341234"
  <+> SName   =: "Falling Grace"
  <+> SArtist =: Right steveSwallow
  <+> SGenre  =: Jazz


jsonRenderer :: Rec ElS JSONRenderer (TotalE ElS)
jsonRenderer
   =  SUId    <-: liftRenderer ("id" .=)
  <+> SName   <-: liftRenderer ("name" .=)
  <+> SArtist <-: liftRenderer
                    ( ("artist" .=)
                    . (renderItem `either` renderItem)
                    )
  <+> SGenre  <-: liftRenderer (("genre" .=) . show)

renderItem :: (TotalE ElS <: rs) => PlainRec ElS rs -> A.Value
renderItem = renderJSON jsonRenderer

data DBU
  = Songs
  | Artists

S.genSingletons [''DBU]
makeUniverse' ''DBU "ElDB"
semantics ''ElDB
  [ 'Songs   :~> [t| [PlainRec ElS SongE] |]
  , 'Artists :~> [t| [PlainRec ElS ArtistE] |]
  ]

findByUId :: IElem UId rs => T.Text -> [PlainRec ElS rs] -> ActionM (PlainRec ElS rs)
findByUId uid =
  maybe (raise "Refuted assertion") return .
    L.find (\r -> rGet SUId r == uid)

generateUId :: IO T.Text
generateUId = T.pack . take 10 . SR.randomRs ('a','z') <$> SR.newStdGen

initialState :: PlainRec ElDB [Songs, Artists]
initialState
   =  SSongs   =: [fallingGrace]
  <+> SArtists =: [steveSwallow]

main :: IO ()
main = scotty 3000 $ do
  db <- liftIO . newIORef . runIdentity <~#= initialState

  let artistsR = rGet' SArtists db
  let songsR = rGet' SSongs db

  get "/songs" $
    liftIO (readIORef songsR)
      >>= json . fmap renderItem

  get "/artists" $ do
    liftIO (readIORef artistsR)
      >>= json . fmap renderItem

  get "/songs/:uid" $ do
    uid <- param "uid"
    liftIO (readIORef songsR)
      >>= findByUId uid
      >>= json . renderItem

  get "/artists/:uid" $ do
    uid <- param "uid"
    liftIO (readIORef artistsR)
      >>= findByUId uid
      >>= json . renderItem

  post "/artists" $ do
    artist <- rdist
       $  SUId  <-: liftIO generateUId
      <+> SName <-: param "name"
    liftIO . modifyIORef artistsR $ (:) artist
    json . renderItem $ artist

  post "/songs" $ do
    song <- rdist
       $  SUId    <-: liftIO generateUId
      <+> SName   <-: param "name"
      <+> SArtist <-: do
            aid <- param "artist"
            liftIO (readIORef artistsR) >>=
              fmap Right . findByUId aid
      <+> SGenre  <-: param "genre"
    liftIO . modifyIORef songsR $ (:) song
    json . renderJSON jsonRenderer $ song


test :: PlainRec ElS [UId, UId]
test = SUId =: "asdfasdf"
  <+> SUId =: "xxx"
