module System.Hardware.Devices.StreamDeckPedal where

import Internal.Prelude
import System.HIDAPI qualified as HID
import System.Hardware.StreamDeck

newtype StreamDeckPedal = StreamDeckPedal
  { deviceInfo :: HID.DeviceInfo
  }
  deriving stock (Generic)

instance IsDevice StreamDeckPedal where
    fromDeviceInfo = StreamDeckPedal

instance IsStreamDeck StreamDeckPedal where
    -- TODO Find this
    deviceIdentifier = undefined
    buttonRows = 1
    buttonCols = 3
    