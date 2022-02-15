countABs :: IO () 
countABs =  go (0,0)

-- pree the string must end with q
go :: (Int,Int)  -> IO ()
go score@(a,b)  = do
    print  score
    x <- getLine 
    case x of 
        "a" -> go (a+1,b)
        "b" -> go (a,b+1)
        _  -> return () 
        