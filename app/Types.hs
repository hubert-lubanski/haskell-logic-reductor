{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import Control.Monad ( when, unless )
import Control.Monad.Logic
import Control.Monad.State
import Control.Applicative

import qualified Data.Map as M

import Data.List ( uncons )
import Data.Bits
import Debug.Trace ( traceIO )

import Common 
import Zipper

type Name = String
data Def = Def { defMatches :: [Match] }

prettyList :: Show a => [a] -> [Char]
prettyList l = prettyList' l ""
    where   prettyList' [] = ([] ++)
            prettyList' [x] = (show x ++)
            prettyList' (x:xs) = ((show x ++ " ") ++) . prettyList' xs

instance Show Def where
    show (Def ms) = show ms

data Match = Match
    { matchName :: Name
    , matchPats :: [Pat]
    , matchRhs  :: Expr
    }

instance Show Match where
    show (Match n [] e) = n ++ " = " ++ show e
    show (Match n ps@(_:_) e) = n ++ " (" ++ prettyList ps ++ ")" ++ " = " ++ show e
        

infixl 9 :$
data Expr = Var Name | Con Name | Expr :$ Expr
    deriving (Eq) -- debug purpose
data Pat = PVar Name | PApp Name [Pat]

instance Show Pat where
    show (PVar n) = n
    show (PApp n []) = n ++ "*"
    show (PApp n ps@(_:_)) = n ++ " (" ++ prettyList ps ++ ")"


instance Show Expr where
    showsPrec p (e1 :$ e2) =
        showParen (p > 6) $ showsPrec 6 e1 . showString " " . showsPrec 7 e2
    
    showsPrec _ (Var n) = showString n
    showsPrec _ (Con n) = showString n
        

instance Walkable TreeMoves Expr where
    walkd TLeft (e1 :$ e2) = Just (e1, e2)
    walkd TRight (e1 :$ e2) = Just (e2, e1)
    walkd _ _ = Nothing

instance BackWalkable TreeMoves Expr where
    walkbackd TLeft (l,r) = l :$ r
    walkbackd TRight (r,l) = l :$ r
    walkbackd _ _ = undefined

instance Zippable TreeMoves Expr where
    zinverse TUp TLeft l r = Just (l :$ r)
    zinverse TUp TRight r l = Just (l :$ r)
    zinverse _ _ _ _ = Nothing

instance Unzippable TreeMoves Expr




-- showCtx repr context = string evaluation, where
--   repr is the representation of previous expression that knows when to
--   put parentheses around itself.
-- NOTE: Someone pointed to me, that giving only (String -> String) from
--       subexpression might be insufficient and that (Int -> String -> String)
--       can be a possible workaround. It turned out to be way cleaner too.
showCtx :: (Int -> String -> String) -> ZCtx TreeMoves Expr -> (String -> String)
showCtx repr Null = repr 0
showCtx repr (ZCtx d ctx' y) = case d of
    -- Place our representation on the left of the previous representation
    TLeft   -> showCtx repr' ctx' where
        repr' p = showParen (p > 6) $ (repr 6) . showString " " . showsPrec 7 y
    -- Place our representation on the right of the previous representation
    TRight  -> showCtx repr' ctx' where
        repr' p = showParen (p > 6) $ shows y . showString " " . repr 7

showZLoc_color :: (String -> String) -> ZLoc TreeMoves Expr -> String
showZLoc_color color (ZLoc x ctx) =
    let sx = const $ showString (color "{") . shows x . showString (color "}")
    in showCtx sx ctx ""

showZLoc :: ZLoc TreeMoves Expr -> String
showZLoc loc = showZLoc_color id loc

instance Show (ZLoc TreeMoves Expr) where
    show = showZLoc


--   Reduction Monad Stack   --

data Subst = Subst { patterns :: [Pat], rhs :: Expr }
    deriving (Show)
type ReduMap = M.Map Name [Subst]

type VerboseMode = Int

standard, extended, full :: VerboseMode
standard = bit 0
extended = bit 1
full     = bit 2

data ReductionState = ReduState {
        reduMap     :: ReduMap,
        reduPath    :: [String],
        maxdepth    :: Int,
        depth       :: Int,

        -- Additional fields --
        verbose :: VerboseMode,
        useColors :: Bool,
        showSearch :: Bool,
        goals :: [String]
    }

type ReductionM a = (ZipperT TreeMoves Expr (StateT ReductionState (LogicT IO))) a

-- We use "observeT" instead of "observeAllT" since backtracking in reduction
-- should be used only when reduction fails, thus "forced backtracking"
-- generates bad reductions.
runReduction loc rstate mio = observeT (runStateT (runZipperT mio loc) rstate)

-- Helpful functions --

rstate :: ReductionM ReductionState
rstate = lift get

incdepth :: ReductionM ()
incdepth = do
    r <- depth <$> rstate
    m <- maxdepth <$> rstate
    
    unless (r < m) $ do
        searchInfo (  altmagentaColor "max depth ("
                   ++ show m
                   ++ altmagentaColor ") reached.")
        failure

    lift (modify (\s -> s{depth = r + 1}))

savedepth :: ReductionM Int
savedepth = depth <$> rstate

resetdepth :: Int -> ReductionM ()
resetdepth d = lift (modify (\s -> s{depth = d}))


savestep :: VerboseMode -> (String -> String) -> ReductionM ()
savestep v color = do
    s <- rstate
    when ((v .&. verbose s) /= 0) $ do
        loc <- get
        let c = if useColors s then color else id
            text = showZLoc_color c loc
        lift $ modify (\s -> s{reduPath = text:(reduPath s)})


setgoal :: String -> ReductionM ()
setgoal g = do
    searchInfo (yellowColor "goal: " ++ g)
    lift (modify (\s -> s{goals = g:(goals s)}))

dropgoal :: ReductionM ()
dropgoal = do
    lift (modify (\s -> s{goals = drop 1 (goals s)}))

substitutionGoal :: String -> ReductionM ()
substitutionGoal s = do
    setgoal s
    searchInfo (yellowColor "subst: " ++ s)

matchGoal :: String -> ReductionM ()
matchGoal s = do
    setgoal s
    searchInfo (yellowColor "match: " ++ s)


failure :: ReductionM ()
failure = do
    s <- rstate
    let mg = uncons (goals s)
    case mg of
        Nothing         -> searchInfo (redColor "fail.")
        (Just (g,_))    -> 
            searchInfo (redColor "fail." ++ altblackColor (" (" ++ g ++ ")"))
    
    dropgoal
    empty -- this actually means "failure"

success :: ReductionM ()
success = do
    s <- rstate
    let mg = uncons (goals s)
    case mg of
        Nothing         -> searchInfo (greenColor "success.")
        (Just (g,_))    ->
            searchInfo (greenColor "success." ++ altblackColor (" (" ++ g ++ ")"))
    dropgoal


searchInfo :: String -> ReductionM ()
searchInfo s = do
    r <- rstate
    when (showSearch r) (liftIO (traceIO s))
    



