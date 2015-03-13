module BiLedOut where

	import System.Hardware.Arduino

	type BiLedOut = (Pin, Pin)

	biLedOn :: BiLedOut -> Bool -> Arduino ()
	biLedOn (f, s) b = do
		digitalWrite f b
		digitalWrite s (not b)

	biLedOff :: BiLedOut -> Arduino ()
	biLedOff (f, s) = do
		digitalWrite f False
		digitalWrite s False

	biLedWrite :: BiLedOut -> Maybe Bool -> Arduino ()
	biLedWrite fs Nothing  = biLedOff fs
	biLedWrite fs (Just b) = biLedOn  fs b
