module Main where

import System.Hardware.Streamdeck
import Control.Concurrent (threadDelay)
import Debug.Trace (traceM, traceShowM)
import Data.ByteString qualified as BS
import System.HIDAPI qualified as HID

import Codec.Picture
import Codec.Picture.Types (ColorSpaceConvertible(convertImage))
import Codec.Picture.Extra (flipVertically, crop, flipHorizontally)
import Control.Monad

main :: IO ()
main = do
    devices <- enumerate
    case devices of
        [] -> fail "no devices found"
        [ device ] -> withDevice device $ \deck -> do
            reset deck
            fillAllKeysWithImg deck

        _ -> fail "multiple devices found"

mkImageFromColor :: Pixel px => px -> Image px
mkImageFromColor color = generateImage (const $ const color) 72 72

doStuff :: HID.Device -> IO ()
doStuff deck = do
    --Right inImg <- readImage "./cat/cat-optimize.bmp"
    --let outImg = BS.toStrict $ encodeBitmap $ convertRGBA8 inImg
    let outImg = BS.toStrict $ encodeBitmap $ mkImageFromColor $ PixelRGB8 255 0 0
    BS.writeFile "out.bmp" outImg
    maybeKey <- readActiveKey deck
    traceShowM maybeKey
    case maybeKey of
        Just key -> setKeyImage (fromIntegral key) outImg deck
        Nothing -> pure ()

    doStuff deck

fillAllKeysWithImg :: HID.Device -> IO ()
fillAllKeysWithImg deck = do
    Right inImg <- readImage "./cat/luna.jpg"
    let dynamicImage = convertImage $ convertRGB8 inImg
    -- let transformedImg = flipVertically dynamicImage
    let size = 72

    let newWidth = size * 5
    let newHeight = size * 3

    let resizedImage = generateImage (\x y -> pixelAt dynamicImage (x * imageWidth dynamicImage `div` newWidth) (y * imageHeight dynamicImage `div` newHeight)) newWidth newHeight

    let mkChunk x y = crop (x * size) (y * size) size size resizedImage
    let chunks = [ (y * 5 + x, mkChunk x y) | x <- [0..4], y <- [0..2] ]

    forM_ chunks $ \(key, chunk) -> do
        let img = BS.toStrict $ encodeJpegAtQuality 100 $ flipHorizontally $ flipVertically chunk
        setKeyImage (fromIntegral key) img deck

    _ <- readActiveKey deck
    fillAllKeysWithImg deck

-- Read Buttons
readKeys :: HID.Device -> IO ()
readKeys deck = do
    print "Looking for buttons"
    buttons <- readKeyStates deck
    print "Found buttons"
    traceM $ show buttons
    return ()

alternateBrightness :: HID.Device -> IO ()
alternateBrightness = go 50
    where
        go brightness deck = do
            setBrightness brightness deck
            threadDelay 1_000_000
            go ( brightness + 30 `mod` 100) deck

sendBlankImage :: HID.Device -> IO ()
sendBlankImage = setKeyImage 2 blankKeyImage