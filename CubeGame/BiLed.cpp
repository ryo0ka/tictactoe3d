/* BiLed.cpp ryo0ka 2/21/2015
   Utility for Arduino bi-color LED lamps. */

#include "DigitalPin.cpp"

/* Represents a set of two positive pins of a bi-color LED lamp. 
   Provides an intuitive control of the light's condition. */
class BiLed {
  private:
  DigitalPin* p1;
  DigitalPin* p2;

  public:
  /* Make sure that
     the ground pin is set and
     so is the given pins' mode. */
  BiLed(DigitalPin& pin1, DigitalPin& pin2) {
    p1 = &pin1;
    p2 = &pin2;
  }
  
  /* Switches the color or turn off the light;
     NULL to turn off, false for pin1, true for pin2. */
  void write(boolean v) {
    if (v == NULL) {
      p1->write(0);
      p2->write(0);
    } else {
      p1->write(!v);
      p2->write( v);
    }
  }
};

