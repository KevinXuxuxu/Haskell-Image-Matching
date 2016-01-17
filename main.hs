module Main where

import Codec.BMP
import Data.ByteString
import System.IO.Unsafe
import System.Environment
import GHC.Word
import Data.Sequence

readin :: String -> IO(ByteString, (Int, Int))

readin filename = do
     Right bmp <- readBMP filename
     let rgba =  (unpackBMPToRGBA32 bmp)
     let (width, height) = bmpDimensions bmp
     return (rgba, (width, height))

wti :: Word8 -> Int
wti p = read (show p)::Int

itw :: Int -> Word8
itw p = read (show p)::Word8

data Image = Image (Seq Int) Int Int deriving Show

getPix :: Image -> (Int, Int) -> [Int]
getPix (Image p w h) (x,y) = [ind p i, ind p (i+1), ind p (i+2), ind p (i+3)]
    where i = 4 * (x + y * w); ind = Data.Sequence.index

getH :: [Int] -> Int
getH [r,g,b,a]
    | ma == mi = 0
    | ma == r && g >= b = (div (60*(g - b)) (ma - mi))
    | ma == r && g < b = (div (60*(g - b)) (ma - mi)) + 360
    | ma == g = (div (60*(b - r)) (ma - mi)) + 120
    | otherwise = (div (60*(r - g)) (ma - mi)) + 240
    where ma = max (max r g) b; mi = min (min r g) b

close :: [Int] -> [Int] -> Int
close [r1,g1,b1,a1] [r2,g2,b2,a2] = (abs (r1 - r2)) + (abs (g1 - g2)) + (abs (b1 - b2)) + (abs (a1 - a2))

close_noise :: [Int] -> [Int] -> Int
close_noise pix1 pix2 = abs ((getH pix1) - (getH pix2))

checkaround :: Image -> (Int, Int) -> Int -> Bool
checkaround img (x,y) i
    | i > 7 = False
    | x+x'<0 || x+x'>=w || y+y'<0 || y+y'>=h = checkaround img (x,y) (i+1)
    | (i+1) == 7 = (getPix img (x+x', y+y') == [255,255,255,255])
    | getPix img (x+x', y+y') == [255,255,255,255] = True
    | otherwise = checkaround img (x,y) (i+1)
    where arounds = [(2,0), (2,2), (0,2), (-2,2), (-2,0), (-2,-2), (0,-2), (2,-2)]; (x',y') = arounds!!i; (Image _ w h) = img

match :: Image -> Image -> (Int, Int) -> Int -> Int -> (Int, Int)
match img1 img2 (x,y) i up = let
    fclose = close;
    (Image _ w h) = img2;
    anchors = [(0,0), (w-1,0), (0,h-1), (w-1,h-1), (div w 2, div h 2), (div w 4, div h 4), (3*(div w 4), 3*(div h 4)), (div w 4, 3*(div h 4)), (3*(div w 4), div h 4)];
    (x',y') = anchors!!i;
    (p1, p2) = (getPix img1 (x+x', y+y'), getPix img2 (x',y'));
    flag = checkaround img2 (x',y') 0
    val = fclose p1 p2
    (v, n) = match img1 img2 (x,y) (i+1) up
    in if flag then (v,n) else (if val > up then (100000,1) else (if (i+1) < Prelude.length anchors then (val+v, n+1) else (val, 1)))

updateCoo :: (Int,(Int,Int)) -> (Int,(Int,Int)) -> (Int,(Int,Int))
updateCoo (m,(x,y)) (m',(x',y')) = if m' < m then (m',(x',y')) else (m,(x,y))

findPlace :: Image -> Image -> (Int, Int) -> (Int,(Int,Int)) -> IO(Int, Int)
findPlace img1 img2 (x,y) (m,(x',y')) = do
    print ((x,y),m)
    let (rst, n) = match img1 img2 (x,y) 0 m
    let updated = updateCoo (m,(x',y')) (div (rst*10) n,(x,y))
    let ans = (if rst < d then (x,y) else (if x < (w-w2-1) then unsafePerformIO (findPlace img1 img2 (x+1, y) updated) else (if y < (h-h2-1) then unsafePerformIO (findPlace img1 img2 (0, y+1) updated) else (x',y') ) )) where (Image _ w h) = img1; (Image _ w2 h2) = img2; d = 1
    return ans

hehe :: Image -> Int -> Int -> Int -> Int -> Int -> Int -> [Int]
hehe img w h x y sx sy
    | x < w-1 = (getPix img ((sx+x), (sy+y))) ++ (hehe img w h (x+1) y sx sy)
    | y < h-1 = (getPix img ((sx+x), (sy+y))) ++ (hehe img w h 0 (y+1) sx sy)
    | otherwise = getPix img ((sx+x), (sy+y))

main :: IO()
main = do
    let args = unsafePerformIO getArgs
    let img = "img5"
    let (rgba1, (width1, height1)) = unsafePerformIO (readin ("/Users/Kevin/Desktop/xingqu/benchmark/"++img++"/"++img++".bmp"))
    let p1 = fromList (Prelude.map wti (unpack rgba1))
    let (rgba2, (width2, height2)) = unsafePerformIO (readin ("/Users/Kevin/Desktop/xingqu/benchmark/"++img++"/"++img++"_partial_taint.bmp"))
    let p2 = fromList (Prelude.map wti (unpack rgba2))
    let img1 = Image p1 width1 height1
    let img2 = Image p2 width2 height2
    let (xx,yy) = unsafePerformIO (findPlace img1 img2 (0,000) (10000, (0,0)))
    print (xx,yy)
    let rgba = Data.ByteString.pack (Prelude.map itw (hehe img1 width2 height2 0 0 xx yy))
    let bmp = packRGBA32ToBMP width2 height2 rgba
    writeBMP "sample.bmp" bmp
    return ()
