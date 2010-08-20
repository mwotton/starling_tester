import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as L
oeuuoe=aoeuhs


printIt (msg, envelope) = ackEnv envelope >> putStrLn (L.unpack (msgBody msg))

main = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn
        
    -- declare a queue, exchange and binding
    declareQueue chan newQueue {queueName = "jobs"}
    declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
    bindQueue chan "jobs" "myExchange" "myKey"
    consumeMsgs chan "jobs" Ack printIt
    getLine 