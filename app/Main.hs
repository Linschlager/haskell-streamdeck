module Main where

import System.Hardware.Streamdeck
import Control.Concurrent (threadDelay)
import Debug.Trace (traceM, traceShowM)
import Data.ByteString qualified as BS
import System.HIDAPI qualified as HID

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types (ColorSpaceConvertible (convertImage), TransparentPixel (dropTransparency))

main :: IO ()
main = do
    devices <- enumerate
    case devices of
        [] -> fail "no devices found"
        [ device ] -> withDevice device $ \deck -> do
            reset deck
            doStuff deck

        _ -> fail "multiple devices found"

mkImageFromColor :: Pixel px => px -> Image px
mkImageFromColor color = generateImage (const $ const color) 72 72

doStuff :: HID.Device -> IO ()
doStuff deck = do
    --Right (dynImg, meta) <- readImageWithMetadata "./streamdeck_key.png"
    --let rgbImg = convertRGB8 dynImg

    let rgbaImg = mkImageFromColor $ PixelRGBA8 255 0 0 255
    let scaledImg = scaleBilinear 72 72 rgbaImg

    let jpg = BS.toStrict $ encodeJpeg $ convertImage $ pixelMap dropTransparency scaledImg
    BS.writeFile "out.jpg" jpg

    let bmp = BS.toStrict $ encodeBitmap scaledImg  
    BS.writeFile "out.bmp" bmp
    
    -- traceShowM =<< readKeyStates deck
    maybeKey <- readActiveKey deck
    traceShowM maybeKey
    case maybeKey of
        Just key -> do
            setKeyImage (fromIntegral key) bmp deck
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