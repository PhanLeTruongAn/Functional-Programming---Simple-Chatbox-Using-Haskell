-- Client.hs
import System.IO (hSetEncoding, utf8, stdout, stdin, stderr)
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import Network.Socket (AddrInfo(..), Socket, Family(..), SocketType(Stream), defaultProtocol, getAddrInfo, connect, socket, withSocketsDo, socketToHandle)
import System.IO (Handle, hClose, hGetLine, hPutStrLn, hSetBuffering, BufferMode(LineBuffering), IOMode(ReadWriteMode))

-- Hàm nhận tin nhắn (receiver) - Bắt lỗi I/O và báo hiệu dừng
receiver :: Handle -> MVar () -> IO ()
receiver handle done = forever $ do
    catch (do
        message <- hGetLine handle
        putStrLn message
        ) (\(_ :: SomeException) -> do
             putStrLn "\n[INFO: Mất kết nối với Server.]"
             putMVar done () -- Báo hiệu dừng khi Server ngắt kết nối
             return () -- Thoát khỏi forever
        )

-- Hàm gửi tin nhắn (sender). Chỉ gửi và báo hiệu dừng khi lỗi I/O.
sender :: Handle -> MVar () -> IO ()
sender handle done = do
    line <- getLine
    catch (hPutStrLn handle line) 
          (\(_ :: SomeException) -> do
              putStrLn "\n[ERROR: Server/Client đã đóng kết nối, không thể gửi tin nhắn. Báo hiệu thoát.]"
              putMVar done () -- Báo hiệu dừng khi lỗi gửi
          )
    sender handle done -- Tiếp tục vòng lặp gửi

-- Hàm Chính Chạy Client
runClient :: String -> Int -> String -> IO () -- <--- Bổ sung đối số name
runClient host port userName = withSocketsDo $ do
    -- 1. Cấu hình và kết nối Socket
    addrinfos <- getAddrInfo Nothing (Just host) (Just (show port))
    let serveraddr = head addrinfos
    
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    
    -- Tạo MVar để báo hiệu dừng
    done <- newEmptyMVar
    
    putStrLn $ "Connected to " ++ host ++ ":" ++ show port ++ ". Chatting as: " ++ userName
    
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering
    
    -- gửi tên người dùng lên server ngay sau khi kết nối
    hPutStrLn handle ("NAME:" ++ userName) -- Gửi tên qua socket
    
    -- 2. Chia luồng (Concurrency)
    void $ forkIO (receiver handle done)
    sender handle done 
    
    -- Đợi cho đến khi một trong hai luồng báo hiệu dừng (do lỗi I/O)
    takeMVar done 
    
    -- Dọn dẹp sau khi chương trình kết thúc
    hClose handle
    putStrLn "Disconnected."

-- Hàm Main của Client (Nhập IP và Tên)
main :: IO ()
main = do
    putStrLn "Enter Server IP (e.g., 127.0.0.1 or 192.168.x.x):"
    ip <- getLine
    
    putStrLn "Enter your name (or leave blank for Anonymous):"
    nameInput <- getLine
    
    let finalName = if null nameInput 
                      then "Anonymous" 
                      else nameInput
                      
    runClient ip 3000 finalName
