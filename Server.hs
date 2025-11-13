-- Server.hs

import Control.Concurrent (forkFinally)
import Control.Concurrent.STM (TVar, atomically, readTVar, newTVarIO, writeTVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import Data.Maybe (isNothing)
import Network.Socket
import System.IO (Handle, hClose, hGetLine, hPutStrLn, hSetBuffering, BufferMode(LineBuffering), IOMode(ReadWriteMode), hFlush, stdout, hSetEncoding, utf8, stdin, stderr)
import qualified Data.Map as M
import System.Random (randomIO) 
import Data.List (isPrefixOf)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- Định nghĩa Kiểu dữ liệu và Trạng thái Server
type ClientID = String
type Connection = Handle
type ClientMap = M.Map ClientID Connection 
type ClientsVar = TVar ClientMap

-- Hàm Dọn dẹp Tài nguyên
cleanup :: IORef ClientID -> Connection -> ClientsVar -> IO ()
cleanup clientIDRef handle clientsVar = do
    clientID <- readIORef clientIDRef 
    
    currentTime <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%H:%M" currentTime
    
    if clientID /= ""
    then do
        atomically $ do
            clientMap <- readTVar clientsVar
            writeTVar clientsVar (M.delete clientID clientMap)
        
        putStrLn $ timeStr ++ " | Client " ++ clientID ++ " disconnected."
        hFlush stdout 
        broadcast "SERVER" (timeStr ++ " | " ++ clientID ++ " left the chat") clientsVar
    else do
        putStrLn $ timeStr ++ " | Unregistered client disconnected (pre-registration error)."
    
    catch (hClose handle) (\(_ :: SomeException) -> return ())
    
    clientCount <- atomically $ M.size <$> readTVar clientsVar
    putStrLn $ "Active users remaining: " ++ show clientCount
    hFlush stdout 

-- Hàm Gửi tin nhắn đến Tất cả Client
broadcast :: ClientID -> String -> ClientsVar -> IO ()
broadcast senderID message clientsVar = do
    clientMap <- atomically $ readTVar clientsVar
    
    mapM_ (\(cID, handle) -> 
        if cID /= senderID 
        then hPutStrLn handle message 
        else return ()
        ) (M.toList clientMap)

-- Hàm Xử lý Client
handleClient :: IORef ClientID -> Connection -> ClientsVar -> IO ()
handleClient clientIDRef handle clientsVar = do
    hSetBuffering handle LineBuffering
    
    nameLine <- catch (hGetLine handle) (\e -> ioError e) 
    
    let userName = if "NAME:" `isPrefixOf` nameLine
                   then drop (length "NAME:") nameLine
                   else "Unknown_User"
                   
    currentTime <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%H:%M" currentTime 
    
    r :: Int <- randomIO
    let finalID = if userName == "Anonymous" 
                  then "Anonymous" ++ show r 
                  else userName
                  
    writeIORef clientIDRef finalID 
                  
    atomically $ do
        clientMap <- readTVar clientsVar
        writeTVar clientsVar (M.insert finalID handle clientMap)
        
    putStrLn $ timeStr ++ " | Client " ++ finalID ++ " added."
    hFlush stdout 
    broadcast "SERVER" (timeStr ++ " | " ++ finalID ++ " joined the chat") clientsVar
    
    forever $ do
        message <- catch (hGetLine handle) (\e -> ioError e)
        
        msgTime <- getCurrentTime
        let msgTimeStr = formatTime defaultTimeLocale "%H:%M" msgTime 
        
        broadcast finalID (msgTimeStr ++ " | " ++ finalID ++ ": " ++ message) clientsVar

    where
        ioError :: SomeException -> IO a
        ioError _ = error "Connection closed" 

-- Hàm Chính Chạy Server
runServer :: Int -> IO ()
runServer port = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just (show port))
    let serveraddr = head addrinfos
    
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1 
    bind sock (addrAddress serveraddr)
    listen sock 5
    
    putStrLn $ "Chat Server listening on port " ++ show port ++ "..."
    hFlush stdout
    
    clients <- newTVarIO M.empty
    
    forever $ do
        (conn, hostAddr) <- accept sock
        handle <- socketToHandle conn ReadWriteMode
        
        putStrLn $ "New client connected from " ++ show hostAddr ++ "."
        hFlush stdout
        
        clientIDRef <- newIORef "" 

        void $ forkFinally (handleClient clientIDRef handle clients) 
                           (\_ -> cleanup clientIDRef handle clients)

-- Hàm Main của Server
main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    runServer 3000
