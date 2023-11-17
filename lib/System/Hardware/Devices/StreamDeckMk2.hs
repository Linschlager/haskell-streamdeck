module System.Hardware.Devices.StreamDeckMk2 where

import System.Hardware.StreamDeck

data StreamDeckMk2

instance IsDevice StreamDeckMk2 where
    deviceIdentifier = 0x0080

instance IsStreamDeck StreamDeckMk2 where
    buttonRows = 3
    buttonCols = 5

instance IsStreamDeckWithDisplayButtons StreamDeckMk2
