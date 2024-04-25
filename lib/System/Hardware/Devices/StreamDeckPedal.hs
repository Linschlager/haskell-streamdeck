module System.Hardware.Devices.StreamDeckPedal where

import System.Hardware.StreamDeck
import Prelude

data StreamDeckPedal

instance IsDevice StreamDeckPedal where
    -- TODO Find this
    deviceIdentifier = undefined

instance IsStreamDeck StreamDeckPedal

instance IsStreamDeckWithButtons StreamDeckPedal where
    buttonCount = 1 * 3

    -- TODO Find this
    buttonPressEventCode = undefined
