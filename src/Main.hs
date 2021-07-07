import System.IO
import System.Environment (getArgs)
import Data.IORef
import Data.Array.IO
import Data.Char (ord, chr)
import Data.Text
import Data.Text.IO
import Debug.Trace (trace)

add :: Int -> Int -> Int
add x y = x + y

sub :: Int -> Int -> Int
sub x y = x - y

increment :: IORef Int -> IO ()
increment ptr = modifyIORef ptr (`add` 1)

decrement :: IORef Int -> IO ()
decrement ptr = modifyIORef ptr (`sub` 1)

input :: IO Int
input = do
    s <- System.IO.getContents
    let c = s !! 0
    return (ord c)

untilIdx :: Text -> Int -> Int -> Int
untilIdx str idx n = do
    if (Data.Text.index str idx) == '[' then
        untilIdx str (idx + 1) (n + 1)
    else if (Data.Text.index str idx) == ']' then
        if n == 0 then
            idx
        else
            untilIdx str (idx + 1) (n - 1)
    else
        untilIdx str (idx + 1) n

run :: Text -> Int -> Int -> IOUArray Int Int -> IORef Int -> IO ()
run str idx maxIdx tape ptr
    | idx > maxIdx = return ()
    | (Data.Text.index str idx) == '>' = do
        increment ptr
        run str (idx + 1) maxIdx tape ptr
    | (Data.Text.index str idx) == '<' = do
        decrement ptr
        run str (idx + 1) maxIdx tape ptr
    | (Data.Text.index str idx) == '+' = do
        p <- readIORef ptr
        v <- readArray tape p
        writeArray tape p (v + 1)
        run str (idx + 1) maxIdx tape ptr
    | (Data.Text.index str idx) == '-' = do
        p <- readIORef ptr
        v <- readArray tape p
        writeArray tape p (v - 1)
        run str (idx + 1) maxIdx tape ptr
    | (Data.Text.index str idx) == '.' = do
        p <- readIORef ptr
        v <- readArray tape p
        let c = chr v
        putChar c
        run str (idx + 1) maxIdx tape ptr
    | (Data.Text.index str idx) == ',' = do
        p <- readIORef ptr
        v <- input
        writeArray tape p v
        run str (idx + 1) maxIdx tape ptr
    | (Data.Text.index str idx) == '[' = do
        p <- readIORef ptr
        v <- readArray tape p
        let i = untilIdx str (idx + 1) 0
        if v == 0 then
            run str (i + 1) maxIdx tape ptr
        else do
            run str (idx + 1) (i - 1) tape ptr
            run str idx maxIdx tape ptr
    | otherwise = run str (idx + 1) maxIdx tape ptr

main :: IO ()
main = do
    args <- getArgs
    str <- Data.Text.IO.readFile (args !! 0)

    ptr <- newIORef 0
    tape <- newArray (0, 30000) 0 :: IO (IOUArray Int Int)

    run str 0 (Data.Text.length str - 1) tape ptr
