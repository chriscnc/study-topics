import System.IO

main = do
    handle <- openFile "printfile.hs" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle