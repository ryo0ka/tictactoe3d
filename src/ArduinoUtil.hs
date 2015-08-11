-- Defines utility functions for Arduino components
module ArduinoUtil (
	digpin,
	bipin,
	Button,
	initButton,
	readButton,
	BiLed,
	biLedOn,
	biLedOff,
	biLedWrite
) where
	import Data.Word
	import Data.Functor
	import System.Hardware.Arduino

	-- Activates a digital pin.
	digpin :: PinMode -> Word8 -> Arduino Pin
	digpin m n = do
		let p = digital n
		setPinMode p m
		return p

	-- Represents a bi-color LED lamp.
	-- Assign two positive terminals of a lamp.
	data BiLed = BiLed Pin Pin -- this constructor is smart.

	-- Activates a bi-color LED pin (output).
	bipin :: (Word8, Word8) -> Arduino BiLed
	bipin (p, q) = do
		p' <- digpin OUTPUT p
		q' <- digpin OUTPUT q
		return $ BiLed p' q'

	-- Turns on the given bi-color LED
	-- lamp in the specified color.
	biLedOn :: BiLed -> Bool -> Arduino ()
	biLedOn (BiLed p q) b = do
		digitalWrite q (not b)
		digitalWrite p b

	-- Turns off the given bi-color LED lamp.
	biLedOff :: BiLed -> Arduino ()
	biLedOff (BiLed p q) = do
		digitalWrite p False
		digitalWrite q False

	-- Turns on/off the given bi-color LED lamp.
	-- Assign `Nothing` to turn off the lamp.
	-- Assign `Just Bool` to turn on the lamp
	-- in the specified color.
	biLedWrite :: BiLed -> Maybe Bool -> Arduino ()
	biLedWrite pq (Just b) = biLedOn  pq b
	biLedWrite pq Nothing  = biLedOff pq

	-- Represents a state of a button.
	-- The Pin represents the button.
	-- The Bool represents the button's previously read value.
	data Button = Button Bool Pin -- this constructor is smart.

	-- Initializes a button of the given pin.
	initButton :: Word8 -> Arduino Button
	initButton n = Button False <$> digpin INPUT n

	-- Reads the given button and retrieves
	-- whether the button was pressed or not
	-- with a new state of the button.
	readButton :: Button -> Arduino (Bool, Button)
	readButton (Button prev btn) = do
		crnt <- digitalRead btn
		let pressed = not prev && crnt
		return $ (pressed, Button crnt btn)
