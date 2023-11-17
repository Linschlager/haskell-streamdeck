module System.Hardware.Devices.StreamDeckMk2 where

import Internal.Prelude
import System.HIDAPI qualified as HID
import System.Hardware.StreamDeck

newtype StreamDeckMk2 = StreamDeckMk2
    { deviceInfo :: HID.DeviceInfo
    }
    deriving stock (Generic)

instance IsDevice StreamDeckMk2 where
    fromDeviceInfo = StreamDeckMk2

instance IsStreamDeck StreamDeckMk2 where
    deviceIdentifier = 0x0080
    buttonRows = 3
    buttonCols = 5

instance IsStreamDeckWithDisplayButtons StreamDeckMk2
