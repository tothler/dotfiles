module Main where

import Lib
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Zipper as TZ
import qualified Control.Monad.State.Lazy as ST
import qualified System.Environment as SE
import           Control.Monad.IO.Class
import qualified System.Console.Terminal.Size as TS
import qualified System.Console.ANSI          as AC


type Buffer = TZ.TextZipper LT.Text

data Position = Pos
    { x :: Int
    , y :: Int
    }

data EditorState = ES
    { buf       :: Buffer
    , cursorPos :: Position
    , screenPos :: Int        -- line number at top of screen
    , filename  :: LT.Text
    } 

newtype Uber a = U (ST.StateT EditorState IO a)
    deriving ()

evalU :: Uber a -> EditorState -> IO a
evalU (U x) = ST.evalStateT x

main :: IO ()
main = do
    args <- SE.getArgs
    return ()

pb :: EditorState -> IO a
pb e = evalU $ U $ do
    put e
    liftIO terminalSetup
    args <- liftIO SE.getArgs
    buffer <- liftIO $ getFileContentsAsZipper (head args)
    liftIO $ fullRedraw state


    c <- liftIO getChar
    
    

terminalSetup :: IO ()
terminalSetup = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    AC.setCursorPosition 0 0

getFileContentsAsZipper :: FilePath -> IO (TZ.TextZipper LT.Text)
getFileContentsAsZipper f = do
    contents <- LTIO.readFile f
    return (lazyTextZipper (LT.lines contents) Nothing)
    
lazyTextZipper :: [LT.Text] -> Maybe Int -> TZ.TextZipper LT.Text
lazyTextZipper = TZ.mkZipper 
        LT.singleton
        (LT.drop . fromIntegral)
        (LT.take . fromIntegral)
        (fromIntegral . LT.length)
        LT.last
        LT.init
        LT.null
        LT.lines
        LT.unpack

emptyLazyTextZipper :: TZ.TextZipper LT.Text
emptyLazyTextZipper = lazyTextZipper [LT.empty] Nothing

fullRedraw :: EditorState -> IO ()
fullRedraw e = do
    terminalSize <- TS.size
    case terminalSize of
        Nothing -> return ()
        Just ts  -> do
            AC.clearScreen
            AC.setCursorPosition 0 0
            LTIO.putStr (LT.unlines $ take (TS.height ts) $ drop (screenPos e) $ TZ.getText (buf e))
            AC.setCursorPosition (x (cursorPos e)) (y (cursorPos e) - screenPos e)

getPosAsTuple :: Position -> (Int, Int)
getPosAsTuple p = (x p, y p)

defaultEditorState :: EditorState
defaultEditorState = ES 
    { buf = emptyLazyTextZipper
    , cursorPos = Pos {x = 0, y = 0}
    , screenPos = 0
    , filename = LT.empty
    }
