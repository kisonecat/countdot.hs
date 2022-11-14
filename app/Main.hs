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

patterns = [ (Ring 8 :- 1) :* (Grid 2 2 :- 1)
           , Ring 4
           , Grid 2 2
           ]

data Dot = Dot
  { dotX :: PDFFloat
  , dotY :: PDFFloat
  , dotScale :: PDFFloat
  , dotAngle :: Angle 
  }

cardWidth :: Double
cardWidth = 158.4567

cardHeight :: Double
cardHeight = 246.8976

inch :: Double
inch = 72

bleedMargin :: Double
bleedMargin = 0.25 * inch

showBorders :: Bool
showBorders = True 

centerText :: AnyFont -> Text -> Draw ()
centerText font s = do
  let p = 12
  let f = PDFFont font p 
  let w = textWidth f s
  let h = getHeight font p 
  let d = getDescent font p
  let factor = 100 / w
  withNewContext $ do
    applyMatrix $ translate ((-50) :+ 0)
    applyMatrix $ scale factor factor
    applyMatrix $ translate (0 :+ ((d-h)/2))
    drawText $ text f 0 0 s

withDotContext :: Dot -> Draw () -> Draw ()
withDotContext (Dot x y s a) d = do
  withNewContext $ do
    applyMatrix $ translate (x :+ y)
    applyMatrix $ scale (s / 100) (s / 100)
    applyMatrix $ rotate a
    d
    
drawDot :: Dot -> Draw ()
drawDot dot = do
  withDotContext dot $ do
    fill $ Rectangle 0 (100 :+ 100) 

listPattern :: DotPattern -> [Draw ()]

listPattern (Grid a b) = do
  x <- [0..a-1]
  y <- [0..b-1]
  
  let w :: PDFFloat
      w = 100 / fromIntegral a
      h :: PDFFloat
      h = 100  / fromIntegral b

  pure $ do 
    applyMatrix $ translate $ (w*fromIntegral x) :+ (h*fromIntegral y)
    applyMatrix $ scale (w/120) (h/120) 
    
listPattern (Ring count) = do
  i <- [0..count-1]

  pure $ do 
    applyMatrix $ translate $ 50 :+ 50 
    applyMatrix $ rotate (Degree ((360 :: PDFFloat) * (fromIntegral i) / (fromIntegral count)))
    let s = sin (pi / fromIntegral count) / 1.7
    applyMatrix $ translate $ (50 * (1 - s)) :+ 0
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
  
createPageContent :: AnyFont -> PDFReference PDFPage -> PDF ()
createPageContent font page = drawWithPage page $ do
  withNewContext $ do
    applyMatrix $ translate (0 :+ cardHeight / 2)
    applyMatrix $ translate $ bleedMargin :+ 0
    applyMatrix $ uniformScale ((cardWidth - 2 * bleedMargin) / 100)
    applyMatrix $ translate $ 0 :+ (-50)
    when showBorders $ withNewContext $ do 
      strokeColor red
      setWidth 0.5
      stroke $ Rectangle 0 (100 :+ 100)
    let pattern = (Ring 8 :- 1) :* (Grid 2 2 :- 1)
    fillColor blue
    withNewContext $ do
      applyMatrix $ translate (50 :+ 50)
      centerText font (pack $ show (countDots pattern))
    fillColor black
    drawPattern pattern
  where uniformScale x = scale x x

myDocument :: AnyFont -> PDF ()
myDocument font = do
    page1 <- addPage Nothing
    newSection ("Section") Nothing Nothing $ do
     newSection ("Subsection") Nothing Nothing $ do
        createPageContent font page1
 
main :: IO()
main = do
    let rect = PDFRect 0 0 cardWidth cardHeight
    Right helvetica <- mkStdFont Helvetica_Bold
    runPdf "cards.pdf" (standardDocInfo { author="Jim Fowler", compressed = False}) rect $ do
        myDocument helvetica
