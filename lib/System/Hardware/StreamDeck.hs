{-# LANGUAGE AllowAmbiguousTypes #-}

module System.Hardware.StreamDeck where

import Data.ByteString qualified as BS
import Data.ByteString.Extra qualified as BS
import System.HIDAPI qualified as HID
import Prelude

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
        pure $ filter (\d -> isNothing sn || sn == d.serialNumber) devices

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
        deck <- asks (.device)
        liftIO $ HID.read deck 512

    -- | TODO document this func
    resetKeyStream :: (MonadIO m) => StreamDeckT m s ()
    resetKeyStream = do
        deck <- asks (.device)
        let payload = BS.pack $ 0x02 : replicate (imageReportLength @s - 1) 0
        void $ liftIO $ HID.write deck payload

    -- | TODO document this func
    reset :: (MonadIO m) => StreamDeckT m s ()
    reset = do
        deck <- asks (.device)
        let payload = BS.pack $ 0x02 : replicate 30 0
        void $ liftIO $ HID.sendFeatureReport deck 0x03 payload

    -- | TODO document this func
    setBrightness
        :: (MonadIO m, IsStreamDeckWithDisplayButtons s)
        => Int
        -> StreamDeckT m s ()
    setBrightness (clamp (0, 100) -> percent) = do
        deck <- asks (.device)
        let payload = BS.pack [0x08, fromIntegral percent]
        void $ liftIO $ HID.sendFeatureReport deck 0x03 payload

    -- | TODO document this func
    getSerialNumber :: (MonadIO m) => StreamDeckT m s ByteString
    getSerialNumber = do
        deck <- asks (.device)
        (_reportId, sn) <- liftIO $ HID.getFeatureReport deck 0x06 32
        pure $ BS.drop 2 sn

    -- | TODO document this func
    getFirmwareVersion :: (MonadIO m) => StreamDeckT m s ByteString
    getFirmwareVersion = do
        deck <- asks (.device)
        (_reportId, sn) <- liftIO $ HID.getFeatureReport deck 0x05 32
        pure $ BS.drop 6 sn

class (IsStreamDeck s) => IsStreamDeckWithButtons s where
    buttonPressEventCode :: ByteString
    buttonCount :: Int

    parseActiveButtons :: ByteString -> Maybe [Int]
    parseActiveButtons bs
        | eventCode /= buttonPressEventCode @s = Nothing
        | otherwise = Just $ fst <$> activeButtons
      where
        (eventCode, message) = BS.splitAt (BS.length $ buttonPressEventCode @s) bs
        buttonCodes = BS.take (buttonCount @s) message
        activeButtons = filter (isActive . snd) $ zip [0 ..] $ BS.unpack buttonCodes
        isActive :: Word8 -> Bool
        isActive 0x00 = False
        isActive 0x01 = True
        isActive _ = undefined

class (IsStreamDeck s) => IsStreamDeckWithDisplayButtons s where
    displayButtonPressEventCode :: ByteString
    displayButtonCount :: Int
    parseActiveDisplayButtons :: ByteString -> Maybe [Int]
    parseActiveDisplayButtons bs
        | eventCode /= displayButtonPressEventCode @s = Nothing
        | otherwise = Just $ fst <$> activeButtons
      where
        (eventCode, message) = BS.splitAt (BS.length $ displayButtonPressEventCode @s) bs
        buttonCodes = BS.take (displayButtonCount @s) message
        activeButtons = filter (isActive . snd) $ zip [0 ..] $ BS.unpack buttonCodes
        isActive :: Word8 -> Bool
        isActive 0x00 = False
        isActive 0x01 = True
        isActive _ = undefined

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
        deck <- asks (.device)

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

class (IsStreamDeck s) => IsStreamDeckWithKnobs s where
    knobEventCode :: ByteString
    knobCount :: Int

    parseActiveKnobs :: ByteString -> Maybe [(Int, Int)]
    parseActiveKnobs bs
        | eventCode /= knobEventCode @s || BS.null message = Nothing
        | BS.head message == 0x00 = Just $ (,0) . fst <$> pressedKnobs
        | BS.head message == 0x01 = Just rotatedKnobs
        | otherwise = Nothing
      where
        (eventCode, message) = BS.splitAt (BS.length $ knobEventCode @s) bs
        knobCodes = BS.take (knobCount @s) $ BS.tail message
        pressedKnobs = filter (isActive . snd) $ zip [0 ..] $ BS.unpack knobCodes
        isActive :: Word8 -> Bool
        isActive 0x00 = False
        isActive 0x01 = True
        isActive _ = undefined
        knobRotationCodes = BS.take (knobCount @s) $ BS.tail message
        rotatedKnobs =
            zip [0 ..] $ fromIntegral @Int8 . fromIntegral <$> BS.unpack knobRotationCodes

data StreamDeckState s = StreamDeckState
    { deviceInfo :: HID.DeviceInfo
    , device :: HID.Device
    }

newtype StreamDeckT m s a = StreamDeck {unStreamDeck :: ReaderT (StreamDeckState s) m a}
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
     . (MonadUnliftIO m, IsStreamDeck s)
    => StreamDeckT m s a
    -> m [a]
runStreamDeck StreamDeck{..} =
    withDevice @s $ \deviceInfo device -> do
        let state = StreamDeckState{..}
        runReaderT unStreamDeck state
