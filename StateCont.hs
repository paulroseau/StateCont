{-# LANGUAGE RankNTypes #-}

module StateCont (
   -- | Injects pure value inside a stateful computation
   inject,
   -- | Chains stateful computations
   (>>>=),
   -- | Consults the state
   get,
   -- | Changes the state
   put,
   -- | Modifies the state using the given function
   modify,
   -- | Maps a given function throught a given state
   mapS,
   -- | States type constructor
   State)
where


newtype State s a = State { runState :: forall r. s -> (a -> s -> r) -> r }

 -- | Injects pure value inside a stateful computation
inject :: a -> State s a
inject a = State (\s -> ($s) . ($a))

-- | Maps a given function throught a given state
mapS :: (a -> b) -> State s a -> State s b
mapS f st = State (\s -> let g = runState st $ s
                         in \fb -> g $ (fb . f))

-- | Joins (helps to deduce bind)
joinS :: State s (State s a) -> State s a
joinS stst = State (\s -> let f = runState stst $ s
                          in f $ g)
             where g st s = runState st $ s

-- | Chains stateful computations
bind ::  State s a -> (a -> State s b) -> State s b
bind st f = joinS . (mapS f) $ st

-- | Infix equivalent of bind
infixr 1 >>>=
(>>>=) :: State s a -> (a -> State s b) -> State s b
(>>>=) = bind

-- | Gets current state
get :: State s s
get = State (\s -> ($s) . ($s)) 

-- | Updates current state by the give value
put :: s -> State s ()
put s = State (\_ -> ($s) . ($()))

-- | Applies the given function to current State
modify  :: (s -> s) -> State s ()
modify f = State (\s -> ($(f s)) . ($()))

-- | Extracts current state and discards the result
execState :: s -> State s a -> s
execState s st = (runState st $ s) $ (\a s -> s)

-- | Extracts current result and discards the state
evalState :: s -> State s a -> a
evalState s st = (runState st $ s) $ (\a s -> a)

-- | making State s a an instance of Monad to make do notation available
instance Monad (State s) where
 return = inject
 (>>=) = (>>>=)

-----------------------------------------------------------------------------
-- Test
-----------------------------------------------------------------------------

-- | simple function that manipulates the State, just to show how we can use it
useState :: State Int Int
useState = do
          s <- get
          let r = s*3 + 1
          put r
          modify (* 3)
          return r

funcToMap :: Show a => a -> String
funcToMap a = show a ++ "_mapped"

-- | Run computations using State S a
{- When execute main the output will be :
  4
  3
  4
using execState with get and put :
  12
using evalState with get and put :
  4
  "6_mapped"
-}
main = do
      print $ (runState $ return 1) 3 (+)
      print $ (runState $ get >>= return . (+2)) 1 const
      print $ (runState $ get >>= return . (+2)) 1 (+)
      putStrLn "using execState with get and put : "
      print $  execState 1 useState
      putStrLn "using evalState with get and put : "
      print $ evalState 1  useState
      print $ evalState 4 (mapS funcToMap (get >>= return . (+2)))
