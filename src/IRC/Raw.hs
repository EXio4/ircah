module IRC.Raw (
    module IRC.Raw.Types,
    module IRC.Raw.Parser,
    module IRC.Raw.Network,
    irc_read,
    irc_send
) where
    
import IRC.Raw.Types
import IRC.Raw.Parser
import IRC.Raw.Network

import Control.Concurrent.Chan


irc_read :: IRC -> IO Message
irc_read (IRC reader _) = readChan reader

irc_send :: IRC -> Message -> IO ()
irc_send (IRC _ writer) = writeChan writer 