module Main where

import System.Hardware.Streamdeck
import Control.Concurrent (threadDelay)
import Debug.Trace (traceM, traceShowM)
import Data.ByteString qualified as BS
import System.HIDAPI qualified as HID

import Codec.Picture
import Codec.Picture.Types (ColorSpaceConvertible(convertImage))
import Codec.Picture.Extra (flipVertically, crop)
import qualified Data.ByteString as P

main :: IO ()
main = do
    devices <- enumerate
    case devices of
        [] -> fail "no devices found"
        [ device ] -> withDevice device $ \deck -> do
            reset deck
            doStuff2 deck

        _ -> fail "multiple devices found"

mkImageFromColor :: Pixel px => px -> Image px
mkImageFromColor color = generateImage (const $ const color) 72 72

doStuff :: HID.Device -> IO ()
doStuff deck = do
    Right inImg <- readImage "./cat/cat.jpg"
    let rawImg = convertImage $ convertRGB8 inImg
    let transformedImg = flipVertically rawImg
    let outImg = BS.toStrict $ encodeJpegAtQuality 100 transformedImg
    -- traceShowM =<< readKeyStates deck
    maybeKey <- readActiveKey deck
    traceShowM maybeKey
    case maybeKey of
        Just key -> setKeyImage (fromIntegral key) outImg deck
        Nothing -> pure ()
    
    doStuff deck

doStuff2 :: HID.Device -> IO ()
doStuff2 deck = do
    Right inImg <- readImage "./cat/luna.jpg"
    let dynamicImage = convertImage $ convertRGB8 inImg
    -- let transformedImg = flipVertically dynamicImage

    let newWidth = 72 * 5
    let newHeight = 72 * 3

    let resizedImage = generateImage (\x y -> pixelAt dynamicImage (x * imageWidth dynamicImage `div` newWidth) (y * imageHeight dynamicImage `div` newHeight)) newWidth newHeight

    let outImg = BS.toStrict $ encodeJpegAtQuality 100 resizedImage

    BS.writeFile "out.jpg" outImg
    -- traceShowM =<< readKeyStates deck
    maybeKey <- readActiveKey deck
    traceShowM maybeKey
    case maybeKey of
        Just key -> setKeyImage (fromIntegral key) outImg deck
        Nothing -> pure ()
    
    doStuff deck


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