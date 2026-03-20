{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

-- haskell-src
import Language.Haskell.Parser
import Language.Haskell.Syntax

-- containers
import qualified Data.Map as M
import Data.Maybe
import Data.Bits

import Control.Monad 
import Control.Monad.State 

-- logict: backtracking monad
import Control.Monad.Logic

import Control.Applicative

-- optparse-applicative: comandline options parsing
import Options.Applicative

-- Inner modules
import Types
import Common
import Zipper


import Debug.Trace ( traceIO )


-- "Rozpakowywanie" struktur z bibloteki --
printLoc :: SrcLoc -> [Char]
printLoc x = "line: " ++ (show . srcLine) x ++ " column: " ++ (show . srcColumn) x

unpackParse :: ParseResult a -> a
unpackParse (ParseOk x) = x
unpackParse (ParseFailed x msg) = error (altredColor (msg ++ "!\n") ++ redColor "Error in source file at " ++ altmagentaColor (printLoc x) ++ defaultColor)

unpackModule :: HsModule -> [HsDecl]
unpackModule (HsModule _ _ _ _ decls) = decls

unpackName :: HsName -> String
unpackName (HsIdent str) = str
unpackName (HsSymbol str) = str

unpackQName :: HsQName -> String
unpackQName (UnQual hn) = unpackName hn
unpackQName x = unexpectedValue x

unpackPattern :: HsPat -> Pat
unpackPattern (HsPVar hname) = PVar (unpackName hname) 
unpackPattern (HsPParen p) = unpackPattern p
unpackPattern (HsPApp qname pats) = PApp (unpackQName qname) (map unpackPattern pats)
unpackPattern x = unexpectedValue x

unpackExp :: HsExp -> Expr
unpackExp (HsVar hqn) = Var (unpackQName hqn)
unpackExp (HsCon hqn) = Con (unpackQName hqn)
unpackExp (HsApp e1 e2) = (unpackExp e1) :$ (unpackExp e2)
unpackExp (HsParen e) = unpackExp e
unpackExp (HsLit literal) = Var (unpackLiteral literal)
unpackExp x = unexpectedValue x

unpackLiteral :: HsLiteral -> Name
unpackLiteral (HsChar x) = (show x)
unpackLiteral (HsString x) = (show x)
unpackLiteral (HsInt x) = (show x)
unpackLiteral (HsFrac x) = (show x)
unpackLiteral (HsCharPrim x) = (show x)
unpackLiteral (HsStringPrim x) = (show x)
unpackLiteral (HsIntPrim x) = (show x)
unpackLiteral (HsFloatPrim x) = (show x)
unpackLiteral (HsDoublePrim x) = (show x)

unpackRhs :: HsRhs -> Expr
unpackRhs (HsUnGuardedRhs hexp) = unpackExp hexp
unpackRhs x = unexpectedValue x

unpackMatch :: HsMatch -> Match
unpackMatch (HsMatch _loc name pats rhs _wheredecls) = Match (unpackName name) (map unpackPattern pats) (unpackRhs rhs)

unpackDecl :: HsDecl -> [Match]
unpackDecl (HsFunBind matches) = map unpackMatch matches
unpackDecl (HsPatBind _loc pat rhs _wheredecls) =
    let p = unpackPattern pat; PVar pn = p in
        [Match pn [] (unpackRhs rhs)]
unpackDecl x = unexpectedValue x



makeReductionMap :: [Match] -> ReduMap
makeReductionMap ms = let splitMatch = (\m -> (matchName m, [Subst (matchPats m) (matchRhs m)]))
                          combine = (++)
                      in M.map reverse $ M.fromListWith combine (map splitMatch ms)


-- Typy przydatne przy redukcji, wygodniejsze niż Prog i DefMap

type SubstMap = M.Map Name Expr
type Substitution = (Name -> Maybe Expr)

-- Apply substitution to the given expression
substitute :: Substitution -> Expr -> Expr
substitute f (e1 :$ e2) = (substitute f e1) :$ (substitute f e2)
substitute f e@(Var n) = fromMaybe e (f n)
substitute f e@(Con n) = fromMaybe e (f n)


-- Originally: choose = foldr ((<|>) . pure) empty
-- We want to underline the undergoing backtracking
choose :: [a] -> ReductionM a
choose [] = empty
choose (x:xs) = pure x <|> (choose' xs) where
    choose' [] = empty
    choose' (y:ys) = do{searchInfo (altyellowColor "redo.") >> pure y} <|> (choose' ys)


-- Reduction down to constructor
reduceTo :: Name -> ReductionM ()
reduceTo constName = do
    incdepth    -- this *fails* if we are too deep
    
    expr <- get
    case struct expr of
        (_ :$ _)    -> do -- we skip right-move backtracking point
            searchInfo (showZLoc_color altyellowColor expr)
            savestep full altyellowColor

            zmoveM_ TLeft
            reduceTo constName

        (Var _)     -> do
            searchInfo (showZLoc_color altyellowColor expr)
            savestep (extended .|. full) altyellowColor
            
            checkPattern

            savestep (standard .|. extended) altyellowColor

            reduceTo constName
        (Con c)     -> if (c == constName) then do
                           searchInfo (showZLoc_color altgreenColor expr)
                           savestep full altgreenColor
                       else failure

-- Normal reduction that succeeds, when *any* reduction happens
-- tries to reduce the expression until no reduction is possible.
reduce :: ReductionM ()
reduce = do
    incdepth    -- this *fails* if we are too deep

    expr <- get
    
    searchInfo (showZLoc_color blueColor expr)
    savestep (extended .|. full) blueColor

    case struct expr of
        (_ :$ _)    -> do
            -- Even if we didn't use "backtracking" we would still need
            -- a stack frame for returning from irreducible lhs
            -- thus we are not changine the memory-cost order
            
            -- We could've used logical "soft cut" a.k.a 'ifte', however
            -- we only care about one branch.
            once $ do
                dir <- choose [TLeft, TRight]
                zmoveM_ dir
                reduce
                
        (Var _)     -> do
            checkPattern
            success
            
            savestep standard blueColor
            
            -- this is beautiful: optional reduce
            x <- optional $ do
                    unzipperM_  -- reset location, to top-most
                    searchInfo (cyanColor "continue.")    

                    e <- gets struct
                    setgoal ("reduce " ++ show e)
                    reduce

            when (isNothing x) $ do
                -- report last step of reduction
                savestep (extended .|. full) blueColor -- this is not needed in
                                                       -- standard mode
            
        (Con _)     -> failure      -- we didn't reduce anything

checkPattern :: ReductionM ()
checkPattern = do
    rmap <- reduMap <$> rstate
    (Var n) <- gets struct

    let clauses = fromMaybe [] (M.lookup n rmap)
    clause <- choose clauses        -- fails if there is no reductions!
    

    if null (patterns clause) then
        setgoal (n ++ " -> " ++ show (rhs clause))
    else
        setgoal (n ++ " " ++ show (patterns clause) ++ " -> " ++ show (rhs clause))

    smap <- matchAgainst M.empty (patterns clause)
    
    modifyHereM_ (const (substitute (`M.lookup` smap ) (rhs clause)))
    success

matchAgainst :: SubstMap -> [Pat] -> ReductionM SubstMap
matchAgainst smap [] = do
    return smap

matchAgainst smap (p:ps) = do
    -- Matching is only possible if the last move was to the left.
    lm <- lastMove
    guard (lm == (Just TLeft))

    
    (_ :$ e2) <- struct <$> zmoveM TUp
    setgoal (show p ++ " == " ++ show e2)

    zmoveM_ TRight
    m <- patternMatch p
    zmoveM_ TUp

    success
    matchAgainst (M.union m smap) ps
        

patternMatch :: Pat -> ReductionM SubstMap
patternMatch (PVar n) = do
    expr <- gets struct
    return (M.singleton n expr)

patternMatch (PApp c pats) = do
    
    setgoal ("Forced reduction to " ++ c)
    
    d <- savedepth
    reduceTo c
    resetdepth d

    success
    smap <- matchAgainst M.empty pats
    
    return smap



data Options = Options
  { target     :: String
  , search     :: Bool
  , verbosity  :: Int
  , usecolors  :: Bool
  , setdepth   :: Int}

sample  :: Parser Options
sample  = Options
        <$> strArgument
            (   metavar "file")
        <*> switch
            (   long "search"
            <>  short 's'
            <>  help "Wether to display actions along search path")
        <*> option auto
            (   long "verbose"
            <>  short 'v'
            <>  help "Verbosity level from 0 to 2"
            <>  value 0
            <>  metavar "INT"
            <>  showDefault)
        <*> switch
            (   long "no-color"
            <>  help "Wether to disable ANSI ESC Codes in display. (Does not apply to the search path)")
        <*> option auto
            (   long "depth"
            <>  short 'd'
            <>  help "Maximal reduction depth"
            <>  value 30
            <>  metavar "DEPTH"
            <>  showDefault)

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (sample <**> helper)
        (   fullDesc
        <>  progDesc "Show steps of reduction with pattern matching"
        <>  header "Solution to Task 3 - Reductions with Pattern Matching" )

run :: Options -> IO ()
run (Options filename s v c d) = do
    let parse = makeReductionMap . (concatMap unpackDecl) . unpackModule
              . unpackParse . parseModule

    rmap <- parse <$> (readFile filename)

    let startLoc = zipper (Var "main")
        startState = ReduState {
                        reduMap     = rmap,
                        reduPath    = [],
                        maxdepth    = d,
                        depth       = 0,
                        verbose     = (bit v),
                        useColors   = not c,
                        showSearch  = s,
                        goals       = ["reduce main"]
                    }
    
    result <- runReduction startLoc startState reduce

    -- We need to printout the path in reverse
    -- so we use (String -> String) evaluation builiding
    let path = reduPath (snd result)
        ps = foldr1 (flip (.)) $ map (\x -> showString x . showChar '\n') path

    when (s) $ traceIO "\t-- result --"  -- just to separate search from result
    putStrLn (ps "")