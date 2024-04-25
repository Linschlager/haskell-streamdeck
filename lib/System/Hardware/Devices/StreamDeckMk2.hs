{-# LANGUAGE OverloadedLists #-}

module System.Hardware.Devices.StreamDeckMk2 where

import System.Hardware.StreamDeck
import Prelude

data StreamDeckMk2

instance IsDevice StreamDeckMk2 where
    deviceIdentifier = 0x0080

instance IsStreamDeck StreamDeckMk2

instance IsStreamDeckWithDisplayButtons StreamDeckMk2 where
    displayButtonCount = 3 * 5
    displayButtonPressEventCode = [0x01, 0x00, 0x0F, 0x00]
