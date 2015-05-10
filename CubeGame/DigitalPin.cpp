/* DigitalPin.cpp ryo0ka 2/21/2015
   Object-oritented Arduino digital pins.*/

#include <Arduino.h>

/* Injects functions to a given pin. */
class DigitalPin {
  private:
  int p; // immutable!
  
  public:
  DigitalPin(int pin) {
    p = pin;
  }
  
  int pin() {
    return p;
  }
  
  /* Sets the mode of the pin. */
  void setMode(boolean m) {
    pinMode(p, m);
  }
  
  /* Writes a value into the pin. */
  void write(boolean v) {
    digitalWrite(p, v);
  }
};

