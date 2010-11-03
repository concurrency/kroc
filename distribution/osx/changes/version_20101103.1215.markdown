# Changes Since 20100721.1513

## User Visible Changes

* Support for Arduino Uno
* Plumbing's LEVEL type now supports NOT
* New `PROC simple.servo (VAL INT board.pin, umin, umax, CHAN BYTE p?)` in
  `servo.module`
* Upgraded KRoC source tree to revision 7044

## Bugs Fixed

* There is an LED on the Mega board, make it accessible from plumbing
* `digital.module`'s digital read and write functions now take a `LEVEL`
* Various documentation fixes (occamdoc)
* Fix in m1280 adc code

## Developer Changes

* Can now build Mac package on Snow Leopard
