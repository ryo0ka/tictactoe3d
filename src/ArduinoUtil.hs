module ArduinoUtil where

	import System.Hardware.Arduino

	blink :: Pin -> Int -> Arduino ()
	blink p c = do
		digitalWrite p True
		delay c
		digitalWrite p False