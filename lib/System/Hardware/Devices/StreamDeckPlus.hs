module System.Hardware.Devices.StreamDeckPlus where

import System.Hardware.StreamDeck

data StreamDeckPlus

instance IsDevice StreamDeckPlus where
    deviceIdentifier = 0x0084

instance IsStreamDeck StreamDeckPlus where
    buttonRows = 2
    buttonCols = 4

instance IsStreamDeckWithDisplayButtons StreamDeckPlus
