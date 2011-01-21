# Changes Since 20101104.0001

<div class='warningbox'>

<h1>Warning</h1>

The location where bytecode is uploaded to Arduino devices has been changed.
You must therefore re-upload the Transterpreter firmware on any Arduino device
you are using so that you are using the new firmware supplied with this update.

</div>

## Changelogs

* The Windows updater now displays changelogs.

## Arduino Specific Changes

* Added TWI module
* Change of `simple.servo` prototype
* Added `serial.write.dec.int` to `printing.module`
* Support for Arduino Uno
* Plumbing's LEVEL type now supports NOT
* New `PROC simple.servo (VAL INT board.pin, umin, umax, CHAN BYTE p?)` in
  `servo.module`
* Remove Arduino Pro as a compile target (as it is not actually supported at the moment)
* In the `printing.moudle`, print hex constants in upper case to follow occams  syntax

## User Visible Changes

* Read JAVA_HOME from the registry to support Java installations where java.exe is not located in \windows\system32
* KRoC source updated to r7137

## Bugs Fixed

* There is an LED on the Mega board, make it accessible from plumbing
* `digital.module`'s digital read and write functions now take a `LEVEL`
* Various documentation fixes (occamdoc)
* Fix in m1280 adc code
* Fix Seeeduino Mega compile target
* Fix Arduino Mega compile target
* Fixes occ21 memory allocation error in constant folding

