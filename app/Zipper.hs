{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Zipper where

import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe ( fromMaybe )

class Walkable d t where
    -- walkd where container = maybe (new container, omitted part)
    walkd :: d -> t -> Maybe (t, t)

class (Walkable d t) => BackWalkable d t where
    -- reverse of walkd
    walkbackd :: d -> (t, t) -> t

-- Zipper Context
data ZCtx dir con = Null | ZCtx dir (ZCtx dir con) con
    deriving (Show)
-- Zipper Location
data ZLoc dir con = ZLoc {struct :: con, context :: ZCtx dir con}



-- Types supporting a "Zipper"
class (Walkable d t) => Zippable d t where
    zipper :: t -> ZLoc d t
    zipper x = ZLoc x Null

    -- This is the main trick here.
    -- We need to define an unzip operation depending on previous move.
    -- For example in lists: move left -> move right, should be equal identity.
    -- For moves that are not inverse of each other, we return Nothing.
    -- zinverse current_dir last_dir current_val saved_val should_we_combine
    zinverse :: d -> d -> t -> t -> Maybe t
    
    -- Move the Zipper in the direction d
    zmove :: d -> ZLoc d t ->  ZLoc d t
    zmove d loc@(ZLoc x Null) =
        fromMaybe loc $ fmap (\(n,o) -> ZLoc n (ZCtx d Null o)) (walkd d x)
    zmove d loc@(ZLoc x ctx@(ZCtx d' ctx' y)) = case zinverse d d' x y of
        Just combined   -> ZLoc combined ctx'
        Nothing         -> fromMaybe loc $ fmap (\(n,o) -> ZLoc n (ZCtx d ctx o)) (walkd d x)

    
    modifyHere :: (t -> t) -> ZLoc d t -> ZLoc d t
    modifyHere f (ZLoc str ctx) = ZLoc (f str) ctx

class (BackWalkable d t, Zippable d t) => Unzippable d t where
    zunmove :: (ZLoc d t) -> (ZLoc d t)
    zunmove loc@(ZLoc _ Null) = loc
    zunmove loc@(ZLoc x (ZCtx d ctx' y)) = ZLoc (walkbackd d (x,y)) ctx'

    unzipper :: ZLoc d t -> t
    unzipper (ZLoc x Null) = x
    unzipper loc = unzipper (zunmove loc)


-- Standard move sets --

data TreeMoves = TLeft | TRight | TUp
    deriving (Eq)
instance Show TreeMoves where
    show TLeft = "L"
    show TRight = "R"
    show TUp = "U"

data ListMoves = LLeft | LRight
    deriving (Eq)
instance Show ListMoves where
    show LLeft = "L"
    show LRight = "R"

-- Example instance for lists --

instance Walkable ListMoves [a] where
    walkd LRight (x:xs) = Just (xs, [x])
    walkd _ _ = Nothing

instance Zippable ListMoves [a] where
    zinverse LLeft LRight xs (y:ys) = Just (y:xs)
    zinverse _ _ _ _ = Nothing


instance (Show a) => Show (ZLoc ListMoves [a]) where
    show (ZLoc x ctx) = "(" ++ show x ++ "|" ++ show ctx ++ ")"


-- Zipper Monad --
type ZipperT d t = StateT (ZLoc d t)
type Zipper d t = ZipperT d t Identity

zmoveM :: (Monad m, Zippable d t) => d -> ZipperT d t m (ZLoc d t)
zmoveM d = state (\s -> let x = zmove d s in (x,x))

zmoveM_ :: (Monad m, Zippable d t) => d -> ZipperT d t m ()
zmoveM_ d = state (\s -> ((), zmove d s))

modifyHereM :: (Monad m, Zippable d t) => (t -> t) -> ZipperT d t m (ZLoc d t)
modifyHereM f = state (\s -> let x = modifyHere f s in (x,x))

modifyHereM_ :: (Monad m, Zippable d t) => (t -> t) -> ZipperT d t m ()
modifyHereM_ f = state (\s -> ((), modifyHere f s))

lastMove :: (Monad m, Zippable d t) => ZipperT d t m (Maybe d)
lastMove = do
    ctx <- gets context
    return $ case ctx of
        Null        -> Nothing
        ZCtx d _ _  -> Just d

unzipperM :: (Monad m, Unzippable d t) => ZipperT d t m t
unzipperM = state (\s -> let x = unzipper s in (x, ZLoc x Null))

unzipperM_ :: (Monad m, Unzippable d t) => ZipperT d t m ()
unzipperM_ = state (\s -> ((), ZLoc (unzipper s) Null))



runZipperT :: (Zippable d t) => ZipperT d t m a -> (ZLoc d t) -> m (a, (ZLoc d t))
runZipperT = runStateT

evalZipperT :: (Monad m, Zippable d t) => ZipperT d t m a -> (ZLoc d t) -> m a
evalZipperT = evalStateT

execZipperT :: (Monad m, Zippable d t) => ZipperT d t m a -> (ZLoc d t) -> m (ZLoc d t)
execZipperT = execStateT






