import Data.List
import Data.Maybe

row :: Int -> Int
row x = quot x 9

col :: Int -> Int
col x = x `mod` 9

block :: Int -> Int
block x = ((row x) `quot` 3) * 3 + ((col x) `quot` 3)                   

type Position = Int
type PossibleValues = [Int]
type Entry = Int

positionFilterCondition :: Position -> Position -> Bool
positionFilterCondition pos p = (row pos == row p) || (col pos == col p) || (block pos == block p)

solve :: [(Position, Entry)] -> [(Position, PossibleValues)] -> Maybe [(Position, Entry)]
solve sudoko holes = case holes of
                     [] -> Just sudoko
                     ((p, vs) : hs) -> case find isJust (map (\v -> solve ((p,v) : sudoko) (map (\(q,ws) -> (q, if positionFilterCondition p q then (delete v ws) else ws)) hs)) vs) of
                                            Nothing -> Nothing
                                            Just x -> x

myfilter :: [(Position, Entry)] -> Position -> [Entry]
myfilter sudoko pos = nub $ map snd (filter (\(p,_) -> positionFilterCondition pos p) sudoko)
 
print_solution :: [(Position, Entry)] -> IO ()
print_solution xs = do
               let lines = groupBy (\(m,_) (n,_) -> row m == row n) xs
               putStrLn $ unlines (map (foldl (\acc x -> acc ++ " " ++ (show (snd x))) "") lines)

solveSudoko :: [(Position, Entry)] -> IO ()
solveSudoko xs =  print_solution solution where
                  solution = fromJust $ fmap sort (solve sudoko holes)
                  holes = map (\(p,_) -> (p, [1..9] \\ (myfilter sudoko p))) holes_positions
                  (holes_positions, sudoko) = partition ((==0).snd) xs
         
main = do
     s <- fmap (map read.tail.words) getContents :: IO [Int]
     mapM_ (solveSudoko) (groupBy (\(m,_) (n,_) -> quot m 81 == quot n 81) (zip [0..] s))  
     

     
        