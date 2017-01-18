import Control.Concurrent
import System.IO
import Data.List
import Control.Monad
import Control.Exception
import System.Exit
import Data.Maybe
import System.Environment
import System.Directory
import System.Process
import Data.Time
import Control.Concurrent.Async
import System.Timeout
import Data.Char
import System.Posix.Terminal 
import System.Posix.IO (stdInput)
import Network.Socket (sendTo,defaultProtocol,inet_addr,socket,Family(AF_INET),SocketType(Datagram),SockAddr(SockAddrInet))
import Network

--uDPSendPort = 1012
--uDPSendIP = "127.0.0.1"
--port = ?

name = "MessageServer"
pass_msg = "password\r"
init_msg = "connect\r"

--protocol 
--   init
--   'custom_name'
--   pass

-- send 'info' to check connection other side
-- send 'heartbeat' for presence server
-- send 'quit' to quit

-- inconvenience in send 'info', need to send two times in case of other side disconnection





check_pass :: String -> Bool
check_pass x
  | x == pass_msg = True
  | otherwise = False

check_init :: String -> Bool
check_init x
  | x == init_msg = True
  | otherwise = False

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    case length args of
        3 -> do
            args <- System.Environment.getArgs
            path <- getCurrentDirectory
            let uDPSendPort = args !! 0
            let uDPSendIP = args !! 1
            let port = args !! 2 
            sock <- listenOn $ PortNumber (fromIntegral $ read port)
            m <- newEmptyMVar
            o <- newEmptyMVar
            loop sock m o [] [] [] port (fromIntegral $ read uDPSendPort) uDPSendIP
        _ -> do
            writeToFile "UDPSendPort UDPSendIP port" "ERROR args"

writeToFile :: String -> String -> IO ()
writeToFile msg ip = do
    time <- getZonedTime
    putStr $ "[" ++ (show time)  ++ "] " ++ ip ++ ": " ++ msg ++ "\n"
    hFlush stdout
    return ()

writeToFileSendUDP :: String -> String -> PortNumber -> String -> IO ()
writeToFileSendUDP msg ip uDPSendPort uDPSendIP = do
    time <- getZonedTime
    let message = "[" ++ (show time)  ++ "]" ++ "," ++ ip ++ "," ++ msg ++ "\n"
    putStr message
    hFlush stdout
    sendUDP message uDPSendIP uDPSendPort

sendUDP :: String -> String -> PortNumber -> IO ()
sendUDP msg host port = withSocketsDo $ do
        s <- socket AF_INET Datagram defaultProtocol
        hostAddr <- inet_addr host
        Network.Socket.sendTo s msg (SockAddrInet port hostAddr)
        sClose s
        return ()

loop :: Socket -> MVar (Handle,String,ThreadId) -> MVar ([Handle],[String],[ThreadId]) -> [Handle] -> [String] -> [ThreadId] -> String  -> PortNumber -> String -> IO ()
loop sock m o list nameList threadList port uDPSendPort uDPSendIP = do
    --print list
    threadDelay 100000 -- microseconds
    (h,ip,_) <- accept sock
    msg1 <- readMessage h "System" ip
    case check_init msg1 of
        True -> do
            msg2 <- readMessage h "System" ip
            case elem msg2 nameList of
                False -> 
                    goWithoutDuplicates msg1 h msg2 sock m o list nameList threadList ip port uDPSendPort uDPSendIP
                True -> do
                    let (Just index) = elemIndex msg2 nameList
                    let updatedList = list\\[list !! index]
                    let updatedNameList = nameList\\[nameList !! index]
                    let updatedThreadList = threadList\\[threadList !! index]
                    throwTo (threadList !! index) ThreadKilled 
                    goWithoutDuplicatesWaitForThreadKilled msg1 h msg2 sock m o updatedList updatedNameList updatedThreadList ip port uDPSendPort uDPSendIP
        False -> do 
            hClose h
            let message = "1" ++ "," ++ name ++ "," ++ port ++ "," ++ msg1 
            writeToFileSendUDP message ip uDPSendPort uDPSendIP
            loop sock m o list nameList threadList port uDPSendPort uDPSendIP

goWithoutDuplicatesWaitForThreadKilled :: String -> Handle -> String -> Socket -> MVar (Handle,String,ThreadId) -> MVar ([Handle],[String],[ThreadId]) -> [Handle] -> [String] -> [ThreadId] -> String -> String -> PortNumber -> String -> IO ()       
goWithoutDuplicatesWaitForThreadKilled msg1 h name sock m o list nameList threadList ip port uDPSendPort uDPSendIP = do
    threadDelay 100000 -- microseconds
    (closedHandle,closedName,closedThread) <- takeMVar o
    --print "something"
    updateClosedHandles msg1 (closedHandle,closedName,closedThread) h name sock m o list nameList threadList ip port uDPSendPort uDPSendIP

updateClosedHandles :: String -> ([Handle],[String],[ThreadId]) -> Handle -> String -> Socket -> MVar (Handle,String,ThreadId) -> MVar ([Handle],[String],[ThreadId])  -> [Handle] -> [String] -> [ThreadId] -> String -> String -> PortNumber -> String -> IO ()
updateClosedHandles msg1 (closedHandle,closedName,closedThread) h name sock m o list nameList threadList ip port uDPSendPort uDPSendIP= do
    let updatedList = h:(list\\closedHandle)
    let updatedNameList = name:(nameList\\closedName)
    aThread <- async $ auth msg1 name updatedList updatedNameList (threadList\\closedThread) m o ip port uDPSendPort uDPSendIP
    let thread = asyncThreadId aThread
    let updatedThreadList = thread:(threadList\\closedThread)
    loop sock m o updatedList updatedNameList updatedThreadList port uDPSendPort uDPSendIP

goWithoutDuplicates :: String -> Handle -> String -> Socket -> MVar (Handle,String,ThreadId) -> MVar ([Handle],[String],[ThreadId])  -> [Handle] -> [String] -> [ThreadId] -> String ->  String -> PortNumber -> String -> IO ()
goWithoutDuplicates msg1 h name sock m o list nameList threadList ip port uDPSendPort uDPSendIP = do
    closedHandle <- tryTakeMVar o
    case closedHandle of
        Nothing -> do
            --print "nothing"
            let updatedList = h:list
            let updatedNameList = name:nameList
            case length updatedList of
                3 -> do
                    writeToFile "ALREADY 2 CONNECTED" ip
                    hClose h
                    loop sock m o list nameList threadList port uDPSendPort uDPSendIP
                _ -> do
                    aThread <- async $ auth msg1 name updatedList updatedNameList threadList m o ip port uDPSendPort uDPSendIP
                    let thread = asyncThreadId aThread
                    let updatedThreadList = thread:threadList
                    loop sock m o updatedList updatedNameList updatedThreadList port uDPSendPort uDPSendIP
        Just (closedHandle,closedName,closedThread) -> 
            updateClosedHandles msg1 (closedHandle,closedName,closedThread) h name sock m o list nameList threadList ip port uDPSendPort uDPSendIP

auth :: String -> String -> [Handle] -> [String] -> [ThreadId] -> MVar (Handle,String,ThreadId) -> MVar ([Handle],[String],[ThreadId]) -> String -> String -> PortNumber -> String -> IO ()
auth msg1 msg2 (h:h2) (nameH:nameH2) threadH2 m o ip port uDPSendIP uDPSendPort= do
     threadH <- myThreadId
     msg3 <- readMessage h nameH ip
     case check_pass msg3 of 
        True -> do
            writeToFile ("=========> connected users: " ++ show (nameH:nameH2)) ip          
            server h h2 nameH nameH2 threadH threadH2 m o ip
        False -> do
            hClose h
            let message = "1" ++ "," ++ name ++ "," ++ port ++ "," ++ msg1 ++ " | " ++ msg2 ++ " | " ++ msg3
            writeToFileSendUDP message ip uDPSendIP uDPSendPort
            putSomethingInMVar o ([h],[nameH],[threadH]) h nameH threadH ip


server :: Handle -> [Handle] -> String -> [String] -> ThreadId-> [ThreadId] -> MVar (Handle,String,ThreadId) -> MVar ([Handle],[String],[ThreadId]) -> String -> IO ()
server h [] nameH [] threadH [] m o ip = 
     loopServer h Nothing nameH Nothing threadH Nothing m o ip

server h (h2:[]) nameH (nameH2:[]) threadH (threadH2:[]) m o ip = do
     empty <- isEmptyMVar m
     case empty of 
        True -> do
            putMVar m (h,nameH,threadH)
            loopServer h (Just h2) nameH (Just nameH2) threadH (Just threadH2) m o ip
        False -> do
            swapMVar m (h,nameH,threadH)
            loopServer h (Just h2) nameH (Just nameH2) threadH (Just threadH2) m o ip
         
loopServer :: Handle -> Maybe Handle -> String -> Maybe String -> ThreadId-> Maybe ThreadId -> MVar (Handle,String,ThreadId) -> MVar ([Handle],[String],[ThreadId]) -> String -> IO ()
loopServer h (Just h2) nameH (Just nameH2) threadH (Just threadH2) m o ip = do
    msg <- readMessage h nameH ip
    putMessage h h2 nameH nameH2 threadH threadH2 m o msg ip

loopServer h Nothing nameH Nothing threadH Nothing m o ip = do
    threadDelay 100000 --microseconds
    msg <- readMessage h nameH ip
    case msg of 
        "info\r" -> do
                returnMessage h nameH "info: No connection to other end.\r" ip
                tryPutMessage h nameH threadH m o msg ip
        _ -> tryPutMessage h nameH threadH m o msg ip
      
readMessage :: Handle -> String -> String -> IO String
readMessage h nameH ip = do
     result <- try (hGetLine h) :: IO (Either SomeException String)
     case result of
        Left ex -> do
             --putStr "error: "
             let valid = onlyValidSigns $ show ex
             writeToFile valid ip
             return "quit\r"
        Right msg -> do
             --putStr "message: "
             let valid = onlyValidSigns msg
             case valid of 
                "heartbeat\r" -> do
                    returnMessage h nameH "heartbeat\r" ip
                    readMessage h nameH ip
                _             -> do
                    writeToFile (show nameH ++ ": " ++ valid) ip
                    return msg

returnMessage :: Handle -> String -> String -> String -> IO String
returnMessage h nameH msg ip = do
    result <- try (hPutStrLn h msg) :: IO (Either SomeException ())
    case result of
        Right val -> do 
            hFlush h
            return "" -- haskell requires [Char]
        Left _ -> readMessage h nameH ip

putMessage :: Handle -> Handle -> String -> String -> ThreadId -> ThreadId -> MVar (Handle,String,ThreadId) -> MVar ([Handle],[String],[ThreadId]) -> String -> String -> IO ()
putMessage h h2 nameH nameH2 threadH threadH2 m o msg ip = do
     result <- try (hPutStrLn h2 msg) :: IO (Either SomeException ())
     case result of
        Left ex  -> 
            tryPutMessage h nameH threadH m o msg ip
        Right val -> do
            hFlush h2
            case msg of 
                "quit\r" -> putSomethingInMVar o ([h],[nameH],[threadH]) h nameH threadH ip
                _ -> loopServer h (Just h2) nameH (Just nameH2) threadH (Just threadH2) m o ip

tryPutMessage :: Handle -> String -> ThreadId -> MVar (Handle,String,ThreadId) -> MVar ([Handle],[String],[ThreadId]) -> String -> String -> IO ()
tryPutMessage h nameH threadH m o msg ip = do
     handle <- tryTakeMVar m 
     case handle of
        Nothing -> do 
            --hPutStr h "No connection to other end."
            --hFlush h
            writeToFile "No connection to other end." ip
            case msg of 
                "quit\r" -> putSomethingInMVar o ([h],[nameH],[threadH]) h nameH threadH ip
                _ -> loopServer h Nothing nameH Nothing threadH Nothing m o ip
        Just (h2,nameH2,threadH2) -> 
            putMessage h h2 nameH nameH2 threadH threadH2 m o msg ip
     
putSomethingInMVar :: MVar ([Handle],[String],[ThreadId]) -> ([Handle],[String],[ThreadId]) -> Handle -> String -> ThreadId -> String -> IO ()
putSomethingInMVar mvar var h nameH threadH ip = do
        empty <- isEmptyMVar mvar
        case empty of 
            True -> do
                putMVar mvar var
                hClose h 
                writeToFile (show nameH ++ " has quitted!") ip
                --cancel threadH
            False -> do
                (h2,nameH2,threadH2) <- takeMVar mvar
                putMVar mvar (h2++[h],nameH2++[nameH],threadH2++[threadH])
                hClose h 
                writeToFile (show nameH ++ " has quitted!") ip
                --cancel threadH

onlyValidSigns :: String -> String
onlyValidSigns string = [ char | char <- string, isAlpha char || isDigit char || char == '\r' || char == ':' || char == ' ' || char == ',']
    


