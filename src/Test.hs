module Test where

	import Data.Word
	import System.Hardware.Arduino
	--import qualified Data.Foldable as F

	digpin :: Word8 -> PinMode -> Arduino Pin
	digpin n m = do
		let p = digital n
		setPinMode p m
		return p

	run :: Arduino ()
	run = do
		p  <- digpin 42 OUTPUT
		g1 <- digpin 22 OUTPUT
		g2 <- digpin 23 OUTPUT
		g3 <- digpin 24 OUTPUT
		digitalWrite p True
		digitalWrite g1 False
		digitalWrite g2 True
		digitalWrite g3 True

	button :: Arduino ()
	button = do
		l <- digpin 13 OUTPUT
		b <- digpin 2  INPUT
		let go :: Arduino ()
		    go = do
			r <- digitalRead b
			if r then digitalWrite l True >> delay 10000
			     else go
		go


{-
	run :: Arduino () -> IO ()
	run = withArduino False "COM6"

	setOutPins :: [Pin] -> Arduino ()
	setOutPins pins = sequence_ $ map (\p -> setPinMode p OUTPUT) pins

	preparePins :: [Word8] -> Arduino [Pin]
	preparePins nums = sequence $ do
		num <- nums
		let pin = digital num
		setPinMode pin OUTPUT
		return pin

	lightUp :: [Pin] -> Arduino ()
	lightUp pins = F.mapM_ f pins where
		f :: Pin -> Arduino ()
		f pin = digitalWrite pin True
-}
