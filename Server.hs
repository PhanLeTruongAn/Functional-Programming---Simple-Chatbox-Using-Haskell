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
import Data.List (isPrefixOf) -- FIX: Thêm isPrefixOf
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- Định nghĩa Kiểu dữ liệu và Trạng thái Server
type ClientID = String
type Connection = Handle
type ClientMap = M.Map ClientID Connection 
type ClientsVar = TVar ClientMap

-- Hàm Dọn dẹp Tài nguyên
cleanup :: ClientID -> Connection -> ClientsVar -> IO ()
cleanup clientID handle clientsVar = do
    -- LẤY DẤU THỜI GIAN (HH:MM)
    currentTime <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%H:%M" currentTime -- <--- SỬA FORMAT
    
    -- Xóa client khỏi danh sách
    atomically $ do
        clientMap <- readTVar clientsVar
        writeTVar clientsVar (M.delete clientID clientMap)
    
    -- Đóng kết nối (bọc trong catch để tránh lỗi nếu đã đóng)
    catch (hClose handle) (\(_ :: SomeException) -> return ())
    
    -- Thông báo Dọn dẹp (và buộc in ra ngay lập tức)
    putStrLn $ timeStr ++ " | Client " ++ clientID ++ " disconnected."
    hFlush stdout 
    -- FORMAT: HH:MM | <Tên> left the chat
    broadcast "SERVER" (timeStr ++ " | " ++ clientID ++ " left the chat") clientsVar
    
    clientCount <- atomically $ M.size <$> readTVar clientsVar
    putStrLn $ "Active users remaining: " ++ show clientCount
    hFlush stdout 

-- Hàm Gửi tin nhắn đến Tất cả Client
broadcast :: ClientID -> String -> ClientsVar -> IO ()
broadcast senderID message clientsVar = do
    clientMap <- atomically $ readTVar clientsVar
    
    -- Gửi tin nhắn đến tất cả các handle
    mapM_ (\(cID, handle) -> 
        if cID /= senderID 
        then hPutStrLn handle message 
        else return ()
        ) (M.toList clientMap)

-- Hàm Xử lý Client (chạy trên luồng riêng)
handleClient :: ClientID -> Connection -> ClientsVar -> IO ()
handleClient clientID handle clientsVar = do
    hSetBuffering handle LineBuffering
    
    -- 1. ĐỌC TÊN NGƯỜI DÙNG TỪ CLIENT
    nameLine <- catch (hGetLine handle) (\e -> ioError e) 
    
    let userName = if "NAME:" `isPrefixOf` nameLine
                   then drop (length "NAME:") nameLine
                   else "Unknown_User"
                   
    -- LẤY DẤU THỜI GIAN
    currentTime <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%H:%M" currentTime -- <--- SỬA FORMAT
    
    -- CẬP NHẬT CLIENT ID MỚI VỚI TÊN ĐƯỢC CHỌN
    r :: Int <- randomIO
    let finalID = if userName == "Anonymous" 
                  then "Anonymous" ++ show r 
                  else userName
                  
    -- 2. Thêm client mới vào ClientsVar
    atomically $ do
        clientMap <- readTVar clientsVar
        writeTVar clientsVar (M.insert finalID handle clientMap)
        
    putStrLn $ timeStr ++ " | Client " ++ finalID ++ " added."
    hFlush stdout 
    -- FORMAT: HH:MM | <Tên> joined the chat
    broadcast "SERVER" (timeStr ++ " | " ++ finalID ++ " joined the chat") clientsVar
    
    -- 3. Vòng lặp nhận và broadcast tin nhắn
    forever $ do
        message <- catch (hGetLine handle) (\e -> ioError e)
        
        -- THÊM DẤU THỜI GIAN VÀO MỖI TIN NHẮN
        msgTime <- getCurrentTime
        let msgTimeStr = formatTime defaultTimeLocale "%H:%M" msgTime -- <--- SỬA FORMAT
        
        -- Định dạng: HH:MM | Username: message
        broadcast finalID (msgTimeStr ++ " | " ++ finalID ++ ": " ++ message) clientsVar

    where
        ioError :: SomeException -> IO a
        ioError _ = error "Connection closed" 

-- Hàm Chính Chạy Server
runServer :: Int -> IO ()
runServer port = withSocketsDo $ do
    -- ... (Cấu hình Socket) ...
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just (show port))
    let serveraddr = head addrinfos
    
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1 
    bind sock (addrAddress serveraddr)
    listen sock 5
    
    putStrLn $ "Chat Server listening on port " ++ show port ++ "..."
    hFlush stdout
    
    -- Khởi tạo trạng thái
    clients <- newTVarIO M.empty
    
    -- 2. Vòng lặp chính chấp nhận kết nối
    forever $ do
        (conn, hostAddr) <- accept sock
        handle <- socketToHandle conn ReadWriteMode
        
        putStrLn $ "New client connected from " ++ show hostAddr ++ "."
        hFlush stdout
        
        -- Tạo luồng mới và đảm bảo dọn dẹp tài nguyên
        -- Client ID sẽ được đặt trong handleClient sau khi nhận được tên
        void $ forkFinally (handleClient "" handle clients) -- Truyền ID rỗng tạm thời
                           (\_ -> cleanup "" handle clients)

-- Hàm Main của Server
main :: IO ()
main = do
    -- Đặt ENCODING SANG UTF-8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    runServer 3000
