{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, OverloadedStrings #-}

module Main where

import Graphics.PDF
import Control.Monad
import Data.Text (pack, Text)

-- grid(2,2) * circle(5)
-- circle(5) * grid(1,2)
-- (circle(5) - 1) * grid(2,2) - 1
-- (circle(5) - 1) * (grid(2,2) - 1)
-- also (grid(2,2) + 1) is permitted

data DotPattern =
  Grid Integer Integer |
  Ring Integer |
  DotPattern :* DotPattern |
  Stack [ DotPattern ] |
  DotPattern :- Integer
 deriving (Show)

patterns :: [DotPattern] 
patterns =
 [ Grid 2 1 :- 1
 , Grid 2 1 
 , Grid 2 2 :- 1 -- = 3
 , Ring 3 -- = 3
 , Grid 2 2 -- = 4
 , Ring 4 -- = 4
 , Ring 5 -- = 5
 , Grid 3 2 -- = 6
 , Ring 3 :* Grid 2 1 -- = 6
 , Ring 6 -- = 6
 , Grid 2 5 :- 3 -- = 7
 , Grid 3 3 :- 2 -- = 7
 , Ring 8 :- 1 -- = 7
 , (Grid 2 1 :* Grid 2 1) :* Grid 1 2 -- = 8
 , Grid 3 3 :- 1 -- = 8
 , Grid 3 3 
 , Ring 5 :* Grid 1 2 -- = 10
 , Grid 4 3 :- 1 -- = 11
 , Grid 3 5 :- 3 -- = 12
 , Ring 6 :* Grid 2 1 -- = 12
 , Grid 4 4 :- 3 -- = 13
 , Grid 2 1 :* Grid 4 2 :- 2 
 , Grid 4 4 :- 1 -- = 15
 , (Ring 4 :* Ring 4) :- 1 -- = 15
 , Ring 5 :* Ring 3 -- = 15
 , (Ring 6 :- 1) :* Grid 3 1 -- = 15
 , ((Grid 1 2 :* Grid 2 1) :* Grid 2 1) :* Grid 1 2 -- = 16
 , Grid 2 1 :* Ring 8 -- = 16
 , Ring 4 :* Grid 2 2 -- = 16
 , Ring 4 :* Ring 4 -- = 16
 , (Ring 6 :* Grid 3 1) :- 1 -- = 17
 , (Ring 3 :* (Ring 3 :- 1)) :* Ring 3 -- = 18
 , Ring 6 :* Grid 3 1 -- = 18
 , (Grid 1 2 :* Grid 5 2) :- 1 -- = 19
 , Grid 1 2 :* Grid 5 2 -- = 20
 , Ring 5 :* Ring 4 -- = 20
 , (Ring 8 :- 1) :* (Grid 2 2 :- 1) -- = 21
 , (Ring 6 :* Grid 2 2) :- 1 -- = 23
 , (Ring 3 :* Ring 4) :* Ring 2 -- = 24
 , Ring 6 :* Grid 2 2 -- = 24
 , ((Ring 3 :* Ring 3) :* Ring 3) :- 2 -- = 25
 , Ring 5 :* Ring 5 -- = 25
 , (Ring 3 :* Ring 3) :* Ring 3 -- = 27
 , Grid 2 1 :* (Grid 4 4 :- 1) -- = 30
 , (Grid 2 1 :* Grid 4 4) :- 2 -- = 30
 , Grid 2 1 :* Grid 4 4 -- = 32
 , Grid 8 4 -- = 32
 , Grid 2 2 :* Grid 3 3 -- = 36
 , Grid 3 3 :* Grid 2 2 -- = 36
 , Grid 3 3 :* Ring 4 -- = 36
 , (Ring 3 :* Ring 4) :* Ring 3 -- = 36
 , (Ring 4 :* Ring 3) :* Ring 3 -- = 36
 , Ring 5 :* (Grid 3 3 :- 1) -- = 40
 , Grid 3 3 :* Ring 5 -- = 45
 , Ring 5 :* Grid 3 3 -- = 45
 , Grid 7 7 -- = 49
 , Ring 8 :* (Ring 8 :- 1) -- = 56
 , Grid 3 3 :* (Ring 8 :- 1) -- = 63
 , Grid 2 2 :* (Grid 2 2 :* Grid 2 2) -- = 64
 , (Grid 3 3 :- 1) :* Grid 3 3 -- = 72
 , (Grid 3 3 :* Grid 3 3) :- 1 -- = 80
 , (Grid 3 3 :* Grid 3 3)  -- = 81
 , Grid 2 2 :* (Grid 5 5 :- 1) -- = 96
 , (Grid 2 2 :* Grid 5 5) :- 1 -- = 99
 , Grid 2 2 :* Grid 5 5 -- = 100
 , Ring 5 :* Grid 5 4 -- = 100
 , Ring 5 :* Grid 5 5 -- = 125
 , Grid 2 1 :* (Grid 2 2 :* Grid 5 5) -- = 200
 , (Grid 2 2 :- 1) :* (Grid 2 2 :* Grid 5 5) -- = 300
 , (Grid 2 2 :* (Grid 2 2 :* Grid 5 5)) :- 1 -- = 399
 , Grid 2 2 :* (Grid 2 2 :* Grid 5 5) :- 5
 , Grid 2 2 :* (Grid 2 2 :* Grid 5 5) -- = 400
 ]

inch :: Double
inch = 72

mm :: Double
mm = 2.83465
  
cardWidth :: Double
cardWidth = 70 * mm

cardHeight :: Double
cardHeight = 70 * mm 

bleedMargin :: Double
bleedMargin = 0.25 * inch

withCardContext :: Draw () -> Draw ()
withCardContext draw = do
  withNewContext $ do
    applyMatrix $ translate (0 :+ cardHeight / 2)
    applyMatrix $ translate $ bleedMargin :+ 0
    applyMatrix $ uniformScale ((cardWidth - 2 * bleedMargin) / 100)
    applyMatrix $ translate $ 0 :+ (-50)
    draw
  where uniformScale x = scale x x
    
showBorders :: Bool
showBorders = False 

centerText :: AnyFont -> Text -> Draw ()
centerText font s = do
  let p = 12
  let f = PDFFont font p 
  let w = textWidth f s
  let h = getHeight font p 
  let d = getDescent font p
  let factor = min (100 / w) (100 / h)
  withNewContext $ do
    applyMatrix $ scale factor factor
    applyMatrix $ translate ((-w/2) :+ 0)
    applyMatrix $ translate (0 :+ ((d-h)/2))
    drawText $ text f 0 0 s

listPattern :: DotPattern -> [Draw ()]

listPattern (Grid a b) = do
  x <- [0..a-1]
  y <- [0..b-1]

  let a' :: PDFFloat
      a' = fromIntegral a
      b' :: PDFFloat
      b'  = fromIntegral b
      w = 90 / a'
      h = 90 / b'
      width = min w h
  
      marginX = (100 - width * a') / (a' - 1)
      marginY = (100 - width * b') / (b' - 1)
      margin = min marginX marginY
  
      offsetX :: PDFFloat
      offsetX = (100 - ((a' - 1) * margin + a' * width)) / 2
      offsetY :: PDFFloat
      offsetY = (100 - ((b' - 1) * margin + b' * width)) / 2
      
  pure $ do 
    applyMatrix $ translate $ offsetX :+ offsetY 
    applyMatrix $ translate $ ((width + margin)*fromIntegral x) :+ ((width + margin)*fromIntegral y)
    applyMatrix $ scale (width / 100) (width / 100) 
    
listPattern (Ring count) = do
  i <- [0..count-1]

  pure $ do 
    applyMatrix $ translate $ 50 :+ 50 
    applyMatrix $ rotate (Degree ((360 :: PDFFloat) * (fromIntegral i) / (fromIntegral count)))
    let s = sin (pi / fromIntegral count) / 2.4
    applyMatrix $ translate $ (50 * (1 - s) * 1.05) :+ 0
    applyMatrix $ scale s s
    applyMatrix $ translate $ (-50) :+ (-50)

listPattern (p1 :* p2) = do
  q1 <- listPattern p1
  q2 <- listPattern p2

  pure $ do
    q1
    q2
    
listPattern (Stack ps) = []

listPattern (p :- n) = take (length p' - fromInteger n) p'
  where p' = listPattern p

countDots :: DotPattern -> Integer
countDots pattern = fromIntegral $ length $ listPattern pattern

drawPattern :: DotPattern -> Draw ()
drawPattern pattern = do
  forM_ (listPattern pattern) $ \d -> do
    withNewContext $ do
      d
      fill $ Rectangle 0 (100 :+ 100) 

backgroundColor :: Integer -> Color
backgroundColor n
 | n <= 10 = Rgb 0.235 0.431 0.443
 | n <= 100 = Rgb 0.440 0.682 0.431
 | otherwise = Rgb 0.282 0.235 0.274
  
createAnswerContent :: DotPattern -> AnyFont -> PDFReference PDFPage -> PDF ()
createAnswerContent pattern font page = drawWithPage page $ do
  let count = countDots pattern
  withNewContext $ do
    withNewContext $ do
      fillColor $ backgroundColor count 
      fill $ Rectangle ((-10) :+ (-10)) ((cardWidth + 10) :+ (cardHeight + 10))
    withCardContext $ do
      applyMatrix $ translate (50 :+ 50)
      setFillAlpha 0.5
      fillColor white
      centerText font (pack $ show count)
  
createPageContent :: DotPattern -> AnyFont -> PDFReference PDFPage -> PDF ()
createPageContent pattern font page = drawWithPage page $ do
  withCardContext $ do
    when showBorders $ withNewContext $ do 
      strokeColor red
      setWidth 0.5
      stroke $ Rectangle 0 (100 :+ 100)
    drawPattern pattern

myFrontDocument :: AnyFont -> PDF ()
myFrontDocument font = do
    forM_ patterns $ \pattern -> do
      page <- addPage Nothing
      createPageContent pattern font page

myBackDocument :: AnyFont -> PDF ()
myBackDocument font = do
    forM_ patterns $ \pattern -> do
      page' <- addPage Nothing
      createAnswerContent pattern font page'
 
main :: IO()
main = do
    forM_ patterns $ \pattern -> do
      putStrLn $ ", " ++ (show pattern) ++ " -- = " ++ (show $ countDots pattern)

    let rect = PDFRect 0 0 cardWidth cardHeight
    Right helvetica <- mkStdFont Helvetica_Bold
    runPdf "front.pdf" (standardDocInfo { author="Jim Fowler", compressed = False}) rect $ do
        myFrontDocument helvetica
    runPdf "back.pdf" (standardDocInfo { author="Jim Fowler", compressed = False}) rect $ do
        myBackDocument helvetica
  
