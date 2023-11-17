module System.Hardware.Devices.StreamDeckPedal where

import Internal.Prelude
import System.Hardware.StreamDeck

data StreamDeckPedal

instance IsDevice StreamDeckPedal where
    -- TODO Find this
    deviceIdentifier = undefined

instance IsStreamDeck StreamDeckPedal where
    buttonRows = 1
    buttonCols = 3
