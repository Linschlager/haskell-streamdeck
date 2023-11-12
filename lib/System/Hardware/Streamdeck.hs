{-# LANGUAGE ViewPatterns #-}

module System.Hardware.Streamdeck where

import Data.ByteString qualified as BS
import Data.ByteString.Extra qualified as BS
import Data.List (findIndex)
import Internal.Prelude
import System.HIDAPI qualified as HID

keyCount :: (Num a) => a
keyCount = 15

imageReportLength :: (Num a) => a
imageReportLength = 1024

imageReportHeaderLength :: (Num a) => a
imageReportHeaderLength = 8

imageReportPayloadLength :: (Num a) => a
imageReportPayloadLength = imageReportLength - imageReportHeaderLength

-- Constant across all Elgato hardware
vendorID :: Word16
vendorID = 0x0fd9

-- TODO:
-- - Support multiple device types
-- - Support multiple devices connected at the same time
enumerate :: IO [HID.DeviceInfo]
enumerate = HID.enumerate (Just vendorID) (Just 0x0080)

withDevice :: HID.DeviceInfo -> (HID.Device -> IO ()) -> IO ()
withDevice deviceInfo = HID.withHIDAPI . (HID.openDeviceInfo deviceInfo >>=)

readKeyStates :: HID.Device -> IO [Bool]
readKeyStates deck = do
    states <- HID.read deck (4 + keyCount)
    let buttons = BS.drop 4 states
    pure $ (0 /=) <$> BS.unpack buttons

readActiveKey :: HID.Device -> IO (Maybe Int)
readActiveKey deck = do
    findIndex id <$> readKeyStates deck

resetKeyStream :: HID.Device -> IO ()
resetKeyStream deck = do
    let payload = BS.pack $ 0x02 : replicate (imageReportLength - 1) 0
    void $ HID.write deck payload

reset :: HID.Device -> IO ()
reset deck = do
    let payload = BS.pack $ 0x02 : replicate 30 0
    void $ HID.sendFeatureReport deck 0x03 payload

setBrightness :: Word8 -> HID.Device -> IO ()
setBrightness (clamp (0, 100) -> percent) deck = do
    let payload = BS.pack [0x08, percent]
    void $ HID.sendFeatureReport deck 0x03 payload

getSerialNumber :: HID.Device -> IO ByteString
getSerialNumber deck = do
    (_reportId, serialNumber) <- HID.getFeatureReport deck 0x06 32
    pure $ BS.drop 2 serialNumber

getFirmwareVersion :: HID.Device -> IO ByteString
getFirmwareVersion deck = do
    (_reportId, serialNumber) <- HID.getFeatureReport deck 0x05 32
    pure $ BS.drop 6 serialNumber

setKeyImage :: Word8 -> ByteString -> HID.Device -> IO ()
setKeyImage key _ _ | clamp (0, keyCount) key /= key = fail $ "Key index out of bounds: " <> show key
setKeyImage key image deck = do
    let chunks = BS.chunksOf imageReportPayloadLength image
    let lastIndex = fromIntegral $ length chunks - 1

    forM_ (zip [0 ..] chunks) $ \(pageNumber, chunk) -> do
        let len = BS.length chunk
        let isLastChunk = lastIndex == pageNumber
        let header =
                BS.pack
                    [ 0x02
                    , 0x07
                    , key
                    , if isLastChunk then 1 else 0
                    , fromIntegral $ len .&. 0xFF
                    , fromIntegral $ len .>>. 8
                    , pageNumber .&. 0xFF
                    , pageNumber .>>. 8
                    ]
        let padding = BS.replicate (imageReportPayloadLength - len) 0
        let payload = header <> chunk <> padding
        void $ HID.write deck payload
