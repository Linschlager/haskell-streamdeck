{-# LANGUAGE AllowAmbiguousTypes #-}

module System.Hardware.StreamDeck where

import Data.ByteString qualified as BS
import Data.ByteString.Extra qualified as BS
import Internal.Prelude
import System.HIDAPI qualified as HID

class IsDevice d where
    -- | TODO document this field
    deviceIdentifier :: Word16

    -- | Constant across all Elgato hardware
    vendorIdentifier :: Word16
    vendorIdentifier = 0x0fd9

    serialNumber :: Maybe HID.SerialNumber
    serialNumber = Nothing

    enumerate :: (MonadIO m) => m [HID.DeviceInfo]
    enumerate = do
        devices <-
            liftIO $ HID.enumerate (Just $ vendorIdentifier @d) (Just $ deviceIdentifier @d)
        let sn = serialNumber @d
        pure
            $ devices
            & filter (\d -> isNothing sn || sn == d.serialNumber)

    withDevice
        :: (MonadUnliftIO m)
        => (HID.DeviceInfo -> HID.Device -> m a)
        -> m [a]
    withDevice f = do
        devices <- enumerate @d
        unliftIO <- askRunInIO
        liftIO $ HID.withHIDAPI $ forM devices $ \deviceInfo -> do
            dev <- liftIO $ HID.openDeviceInfo deviceInfo
            res <- unliftIO $ f deviceInfo dev
            HID.close dev
            pure res

class (IsDevice s) => IsStreamDeck s where
    -- | TODO document this field
    imageReportLength :: Int
    imageReportLength = 1024

    -- | TODO document this field
    imageReportHeaderLength :: Int
    imageReportHeaderLength = 8

    -- | TODO document this field
    imageReportPayloadLength :: Int
    imageReportPayloadLength = imageReportLength @s - imageReportHeaderLength @s

    -- | TODO document this func
    readInput :: (MonadIO m) => StreamDeckT m s ByteString
    readInput = do
        deck <- view #device
        liftIO $ HID.read deck 512

    -- | TODO document this func
    resetKeyStream :: (MonadIO m) => StreamDeckT m s ()
    resetKeyStream = do
        deck <- view #device
        let payload = BS.pack $ 0x02 : replicate (imageReportLength @s - 1) 0
        void $ liftIO $ HID.write deck payload

    -- | TODO document this func
    reset :: (MonadIO m) => StreamDeckT m s ()
    reset = do
        deck <- view #device
        let payload = BS.pack $ 0x02 : replicate 30 0
        void $ liftIO $ HID.sendFeatureReport deck 0x03 payload

    -- | TODO document this func
    setBrightness
        :: (MonadIO m, IsStreamDeckWithDisplayButtons s)
        => Int
        -> StreamDeckT m s ()
    setBrightness (clamp (0, 100) -> percent) = do
        deck <- view #device
        let payload = BS.pack [0x08, fromIntegral percent]
        void $ liftIO $ HID.sendFeatureReport deck 0x03 payload

    -- | TODO document this func
    getSerialNumber :: (MonadIO m) => StreamDeckT m s ByteString
    getSerialNumber = do
        deck <- view #device
        (_reportId, sn) <- liftIO $ HID.getFeatureReport deck 0x06 32
        pure $ BS.drop 2 sn

    -- | TODO document this func
    getFirmwareVersion :: (MonadIO m) => StreamDeckT m s ByteString
    getFirmwareVersion = do
        deck <- view #device
        (_reportId, sn) <- liftIO $ HID.getFeatureReport deck 0x05 32
        pure $ BS.drop 6 sn

class (IsStreamDeck s) => IsStreamDeckWithButtons s where
    buttonPressEventCode :: ByteString
    buttonCount :: Int
    parseButtonStates :: ByteString -> Maybe [Bool]
    parseButtonStates bs
        | eventCode /= buttonPressEventCode @s = Nothing
        | otherwise = Just buttonFlags
      where
        (eventCode, message) = BS.splitAt (BS.length $ buttonPressEventCode @s) bs
        buttonCodes = BS.take (buttonCount @s) message
        buttonFlags = parseButtonCode <$> BS.unpack buttonCodes
        parseButtonCode :: Word8 -> Bool
        parseButtonCode 0x00 = False
        parseButtonCode 0x01 = True
        parseButtonCode _ = undefined

class (IsStreamDeck s) => IsStreamDeckWithDisplayButtons s where
    displayButtonPressEventCode :: ByteString
    displayButtonCount :: Int
    parseDisplayButtonStates :: ByteString -> Maybe [Bool]
    parseDisplayButtonStates bs
        | eventCode /= displayButtonPressEventCode @s = Nothing
        | otherwise = Just buttonFlags
      where
        (eventCode, message) = BS.splitAt (BS.length $ displayButtonPressEventCode @s) bs
        buttonCodes = BS.take (displayButtonCount @s) message
        buttonFlags = parseButtonCode <$> BS.unpack buttonCodes
        parseButtonCode :: Word8 -> Bool
        parseButtonCode 0x00 = False
        parseButtonCode 0x01 = True
        parseButtonCode _ = undefined

    -- | TODO document this field
    buttonImageWidth :: Int
    buttonImageWidth = 72

    -- | TODO document this field
    buttonImageHeight :: Int
    buttonImageHeight = 72

    -- | TODO document this func
    setButtonImage
        :: (MonadFail m, MonadIO m)
        => Int
        -> ByteString
        -> StreamDeckT m s ()
    setButtonImage key _
        | clamp (0, displayButtonCount @s) key /= key =
            fail $ "Key index out of bounds: " <> show key
    setButtonImage key image = do
        deck <- view #device

        let chunks = BS.chunksOf (imageReportPayloadLength @s) image
        let lastIndex = fromIntegral $ length chunks - 1

        forM_ (zip [0 ..] chunks) $ \(pageNumber, chunk) -> do
            let len = BS.length chunk
            let isLastChunk = lastIndex == pageNumber
            let header =
                    BS.pack
                        [ 0x02
                        , 0x07
                        , fromIntegral key
                        , if isLastChunk then 1 else 0
                        , fromIntegral $ len .&. 0xFF
                        , fromIntegral $ len .>>. 8
                        , pageNumber .&. 0xFF
                        , pageNumber .>>. 8
                        ]
            let padding = BS.replicate (imageReportPayloadLength @s - len) 0
            let payload = header <> chunk <> padding
            void $ liftIO $ HID.write deck payload

class (IsStreamDeck s) => IsStreamDeckWithTouchScreen s where
    screenTouchEventCode :: ByteString

    parseScreenTouchEvent :: ByteString -> Maybe (Int, Int)
    parseScreenTouchEvent bs
        | eventCode /= screenTouchEventCode @s = Nothing
        | otherwise = Just (encode rawX, encode rawY)
      where
        (eventCode, message) = BS.splitAt (BS.length $ screenTouchEventCode @s) bs
        (rawX, rawY) = BS.splitAt 2 $ BS.take 4 message
        encode :: ByteString -> Int
        encode = BS.foldr (\b acc -> acc .<<. 8 .&. fromIntegral b) 0

    screenSwipeEventCode :: ByteString

    parseScreenSwipeEvent :: ByteString -> Maybe (Int, Int)
    parseScreenSwipeEvent _ = undefined

class (IsStreamDeck s) => IsStreamDeckWithKnobs s

data StreamDeckState s = StreamDeckState
    { deviceInfo :: HID.DeviceInfo
    , device :: HID.Device
    }
    deriving stock (Generic)

newtype StreamDeckT m s a = StreamDeck {_runApp :: ReaderT (StreamDeckState s) m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadUnliftIO
        , MonadReader (StreamDeckState s)
        , MonadFail
        , MonadFix
        )

instance (MonadIO m) => MonadBase IO (StreamDeckT m s) where
    liftBase = liftIO

runStreamDeck
    :: forall s m a
     . (IsDevice s, MonadUnliftIO m)
    => StreamDeckT m s a
    -> m [a]
runStreamDeck f =
    withDevice @s $ \deviceInfo device -> do
        let state = StreamDeckState{..}
        runReaderT f._runApp state
