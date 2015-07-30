module BiLed where
	import System.Hardware.Arduino

	data BiLed = BiLed Pin Pin

	biLedOn :: BiLed -> Bool -> Arduino ()
	biLedOn (BiLed p q) b = do
		digitalWrite p b
		digitalWrite q (not b)

	biLedOff :: BiLed -> Arduino ()
	biLedOff (BiLed p q) = do
		digitalWrite p False
		digitalWrite q False

	biLedWrite :: BiLed -> Maybe Bool -> Arduino ()
	biLedWrite pq Nothing  = biLedOff pq
	biLedWrite pq (Just b) = biLedOn  pq b
