import Control.Monad

main = do putStrLn "Hello"
          s <- getLine
          return ()
          when (s == "foo") (putStrLn s)

maybeToEitherUnit :: Maybe a -> Either () a
maybeToEitherUnit Nothing = Left ()
maybeToEitherUnit (Just x) = Right x

eitherUnitToMaybe :: Either () a -> Maybe a
eitherUnitToMaybe (Left ()) = Nothing
eitherUnitToMaybe (Right x) = Just x

pairWithBoolToEither :: (Bool,a) -> (Either a a)
--pairWithBoolToEither (b, x) = if b then Left x else Right x
pairWithBoolToEither (False, x) = Left x
pairWithBoolToEither (True, x) = Right x

eitherToPairWithBool :: Either a a -> (Bool,a)
eitherToPairWithBool (Left x) = (False, x)
eitherToPairWithBool (Right x) = (True, x)

functionFromBoolToPair :: (Bool -> a) -> (a,a)
functionFromBoolToPair f = ((f True), (f False))

pairToFunctionFromBool :: (a,a) -> (Bool -> a)
--pairToFunctionFromBool (x,y) = \b -> if b then x else y
pairToFunctionFromBool (x,_) True = x
pairToFunctionFromBool (_,y) False = y

*Main> :t Left
Left :: a -> Either a b
*Main> :t Left 42
Left 42 :: Num a => Either a b
*Main> :t (Left 42 :: Either Int Int)
(Left 42 :: Either Int Int) :: Either Int Int
*Main> pairWithBoolToEither $ eitherToPairWithBool $ (Left 42)
Left 42
*Main> (Just (+5)) <*> (Just 5)
Just 10
*Main> (Just (+5)) <*> Nothing
Nothing
*Main> Nothing <*> (Just 5)
Nothing
*Main> :t putStrLn
putStrLn :: String -> IO ()
*Main> "Hello world"
"Hello world"
*Main> putStrLn "Hello world"
Hello world
*Main> :t putStrLn
putStrLn :: String -> IO ()
*Main> :t getLine
getLine :: IO String
*Main> putStrLn $ "alma"++['.']
alma.
*Main> getLine
alma
"alma"
*Main> putStrLn . getLine

<interactive>:37:12: error:
• Couldn't match expected type ‘a -> String’
with actual type ‘IO String’
• In the second argument of ‘(.)’, namely ‘getLine’
In the expression: putStrLn . getLine
In an equation for ‘it’: it = putStrLn . getLine
• Relevant bindings include
it :: a -> IO () (bound at <interactive>:37:1)
*Main> :t getLine 
getLine :: IO String
*Main> :t putStrLn
putStrLn :: String -> IO ()
*Main> getLine >>= putStrLn
alma
alma
*Main> getLine >>= putStrLn >>= putStrLn

<interactive>:41:26: error:
• Couldn't match type ‘()’ with ‘[Char]’
Expected type: () -> IO ()
Actual type: String -> IO ()
• In the second argument of ‘(>>=)’, namely ‘putStrLn’
In the expression: getLine >>= putStrLn >>= putStrLn
In an equation for ‘it’: it = getLine >>= putStrLn >>= putStrLn
*Main> :t return
return :: Monad m => a -> m a
*Main> :t sequence
sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)
*Main> sequence [putStrLn "alma", putStrLn "korte"]
alma
korte
[(),()]