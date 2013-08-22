import System.IO
import Data.List.Split
--Dependencies splitOn

main = do
	handle <- openFile "codigo.c" ReadMode
	contents <- hGetContents handle
	putStr contents
	hClose handle
	cars<-splitOn contents " "
	putStr cars