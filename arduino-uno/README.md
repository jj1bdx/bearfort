# Bearfort sensor shield Arduino UNO R3 code

* The first 2 bytes of EEPROM (little endian) is the device ID
* Set EESAVE bit of High Fuse Bits programmed to preserve EEPROM from erasing the flash
* Use Optiboot for fast development (note: Optiboot does *not* allow rewriting EEPROM)
