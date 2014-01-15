solve p n u = fst$foldl (\(s,t)y->(s+(t/(2*(fromIntegral u)+y)),-t)) (p,(-1)^u) (take (n-u) [1,3..])

main = do
     s<-fmap(map read.tail.words)getContents::IO[Int]
     mapM_ print $ tail $ foldl(\a n -> a++[solve (last a) (snd n) (fst n)]) [0] (zip (0:s) s)