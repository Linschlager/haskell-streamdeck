{-# LANGUAGE OverloadedLists #-}

module System.Hardware.Devices.StreamDeckPlus where

import System.Hardware.StreamDeck
import Prelude

data StreamDeckPlus

instance IsDevice StreamDeckPlus where
    deviceIdentifier = 0x0084

instance IsStreamDeck StreamDeckPlus

instance IsStreamDeckWithDisplayButtons StreamDeckPlus where
    displayButtonCount = 2 * 4
    displayButtonPressEventCode = [0x01, 0x00, 0x08, 0x00]
    buttonImageWidth = 144
    buttonImageHeight = 144

instance IsStreamDeckWithKnobs StreamDeckPlus where
    knobEventCode = [0x01, 0x03, 0x05, 0x00]
    knobCount = 4
