module Hw2 where

--Exercise 1:	A Stack Language
--import Debug.Trace 

type Prog 	= [Cmd]
type Stack	= [Int]
type D		= Stack -> Maybe Stack

type State	= (Macros, Stack)
type Macros	= [(String, Prog)]
type E		= State -> Maybe State

data Cmd	= LD Int
		| ADD
		| MULT
		| DUP
		| DEF String Prog
		| CALL String
		deriving Show

sem :: Prog -> D
sem []       s 	= Just s
sem (x:xs)   s 	= case (semCmd x s) of
		  Nothing -> Nothing
		  Just s' -> sem xs s'

semCmd :: Cmd -> D
semCmd (LD i)       (x) = Just (i:x)
semCmd (DUP)     (x:xs) = Just (x:x:xs)
semCmd (DUP)         _  = Nothing
semCmd (ADD)   (x:y:xs) = Just ((x+y):xs)
semCmd (ADD)  	     _	= Nothing
semCmd (MULT)  (x:y:xs) = Just ((x*y):xs)
semCmd (MULT)        _  = Nothing

--Exercise 1-2: Extended 

sem2 :: Prog -> E
sem2 []	      s	= Just s
sem2 (x:xs)   s	= case (semCmd2 x s) of
		  Nothing -> Nothing
		  Just s' -> sem2 xs s'

semCmd2 :: Cmd -> E
semCmd2 (LD i) (m, s) = case (semCmd (LD i) s) of
			Nothing -> Nothing
			Just s' -> Just (m, s')
semCmd2 (DUP)  (m, s) = case (semCmd (DUP) s) of
			Nothing -> Nothing
			Just s' -> Just (m, s')
semCmd2 (ADD)  (m, s) = case (semCmd (ADD) s) of
			Nothing -> Nothing
			Just s' -> Just (m, s')
semCmd2 (MULT) (m, s) = case (semCmd (MULT) s) of
			Nothing -> Nothing
			Just s' -> Just (m, s')

semCmd2 (DEF str p) (x,y) = Just ((str, p):x, y)
semCmd2 (CALL str)  (x,y) = case (lookup str x) of
			    Nothing  -> Nothing
			    Just com -> sem2 com (x,y)

--sandbox
m1 :: Macros
m1 = [("temp1", [LD 1, DUP]),("temp2", [LD 3, DUP])]

t1 = (m1, [1,2,3])

fin = semCmd2 (CALL "temp2") t1
testDef = semCmd2 (DEF "temp3" [LD 4, DUP]) (m1, [])

test0 :: Prog
test0 = [LD 3, DUP, ADD, DUP, MULT]

test1 :: Prog
test1 = [LD 3, ADD]

test2 :: Prog
test2 = []


--Exercise 3
type State2 = (Mode, Int, Int)
type Line  = (Int, Int, Int, Int)
type Lines = [Line]

data CMD = Pen Mode
	 | MoveTo Int Int
	 | Seq CMD CMD

data Mode = Up | Down

--semS :: CMD -> State2 -> (State2, Lines)

--sem' :: CMD -> Lines








