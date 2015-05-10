/* BiLedCube.cpp ryo0ka 2/22/2015
   Unility for 3-by-3 bi-color LED lamp cubes. */

#include "BiLed.cpp"

/* Each position of 9 lamps on a layer. */
enum Horizon {
  H_NW, H_NN, H_NE,
  H_CW, H_CC, H_CE,
  H_SW, H_SS, H_SE,
};

/* Each position of 3 layers. */
enum Vertical {
  V_TOP, V_MID, V_BTM,
};

/* Positive pins of all lamps on a layer. */
typedef map<Horizon, BiLed> BiLedPosPins;

/* All pins of all lamps on a layer. */
typedef tuple<DigitalPin, BiLedPosPins> BiLeds;

/* All pins of all lamps on all layers. */
typedef map<Vertical, BiLeds> BiLedCube;

/* Injects utility functions to BiLedCube.
   Note that all functions assume that
   the given cube totally maps both Vertical and Horizon. */
class BiLedCubeOp { public:
  BiLedCube c;
  
  BiLedCubeOp(BiLedCube& c) {
    c = cube;
  }
  
  /* Returns a ground pin of the specified layer,
     or an intial instance of DigitalPin
     if the BiLedCube maps nothing to the layer. */
  DigitalPin gndAt(Vertical v) {
    return get<1>(c[v]);
  }
  
  /* Returns a pair of positive pins of
     a lamp at the specified location,
     or an initial instance of BiLed
     if the BiLedCube maps nothing to the location. */
  BiLed outAt(Vertical v, Horizontal h) {
    return get<2>(c[v])[h];
  }
  
  /* Executes the given function for each lamp on the cube.
     Note that the function may not be executed for lamps
     that the BiLedCube maps nothing to.  */
  void foreach(void(f)(Vertical, Horizon, DigitalPin&, BiLed&)) {
    for (auto& m : c) {
      Vertical ver = m.first;
      BiLed leds = m.second;
      DigitalPin gnd = leds.first;
      for (auto& n : leds.second) {
        Horizon hor = n.first;
        BiLed out = n.second;
        f(ver, hor, &gnd, &out);
      }
    }
  }
};

