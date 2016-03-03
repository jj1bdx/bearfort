footer: Kenji Rikitake / Erlang Factory SF Bay 2016
slidenumbers: true

# [fit] Fault-tolerant Sensor Nodes
# [fit] with Erlang/OTP and Arduino 

<!-- Use Deckset 1.6.0, Zurich theme, 16:9 aspect ratio -->
<!-- target: 30 slides -->

---

# Kenji Rikitake

10-MAR-2016
Erlang Factory SF Bay 2016
San Francisco, CA, USA
@jj1bdx

Professional Internet Engineer

Erlang Factory SF Bay 2010-2016 speaker (for *seven* consecutive years!)

![right, fit](kenjiface-20150328-2.jpg)

---

# Executive summary

* IoT is a buzzword: *back to basics*
* Designing fault-tolerant systems is *hard*: a practical way is giving more redundancy
* Devices/components *fail*: prepare
* Abstraction in Erlang helps a lot

---

# NOT in this talk

* Protocols over TCP/IP: MQTT, CoAP
* Cryptographic security
* Host OS device drivers
* non-8bit Arduino boards
* Erlang/ALE (good for directly connecting devices to Raspberry Pi, but not for Arduino)

---

# In this talk

* Bearfort system design
* Sensor and 8-bit Arduino basics
* Mounting redundant sensors on Arduino
* Wire protocols
* How Erlang talks with Arduino
* Functional abstraction with Erlang

---

# Bearfort[^1] system diagram

* (diagram of Internet-BEAM-Arduino-Sensors)

[^1]: Bearfort = {BEam, ARduino, FORTified} / Bearfort ridge, NJ, USA

---

# Arduino Uno R3

* Atmel AVR ATmega328P
* 16MHz clock
* 32K Flash (program only)
* 2K RAM, 1K EEPROM
* Powered by USB (5V) or external power supply (7~12V)
* 4 Analog Input + I2C + SPI
* USB Serial I/F by ATmega16U2
* USD24.95[^2] as of March 2016

[^2]: Photo: [Wikimedia Commons](https://commons.wikimedia.org/wiki/File:Arduino_Uno_006.jpg), License: CC-BY-SA-2.0

![right,fit](Arduino_Uno_006.jpg)

---

# Software development on Arduino

* Easy way: Arduino IDE (not in this talk)
* More pragmatic way: 8bit AVR dev tools
  * [avr-libc](http://savannah.nongnu.org/projects/avr-libc/), avr-binutils, avr-gcc
* Program loader: [avrdude](http://savannah.nongnu.org/projects/avrdude)
* Boot loader: [optiboot](https://github.com/optiboot/optiboot/) 
  * [My fork for EEPROM read/write](https://github.com/jj1bdx/optiboot/)
* In-circuit debuggers/chip programmers: [AVR Dragon](http://www.atmel.com/tools/AVRDRAGON.aspx), STK500, AVRISP mkII


---

# AVR chip programmers[^3]

* Controllable from avrdude
* Chip diagnostics
* High-voltage parallel programming
* Required to write boot loaders
* Required to write fuse bits
* Other hardware debugging

![right,fit](avrdragon-20080108-ppcable2.jpg)

[^3]: Photo: AVR Dragon configured for ATmega168/328p with a zero-pressure DIP socket, by Kenji Rikitake, circa January 2008

---

# Bearfort sensor shield

* Arduino add-on board
* Five temperature sensors
* Four Texas Instruments LM60s on ATmega328p ADC0-3
    * Output fault protection (100kohm to GND)
* One Analog Devices ADT7410 on ATmega328p TWI(I2C)
* All sensors are 5V powered
* All sensors are replaceable

![right,fit](bearfort-sensor-v2.jpg)

---

# What Bearfort shield can do

* Measuring (ambient) temperature
* Device/Arduino-level fault tolerance
   * Redundancy of five sensors
* Controllable via ATmega328p
* Can be updated via avrdude through USB

---

# Temperature sensor characteristics

* Thermal time constant: 60~100 seconds
* LM60: -40°C to +125°C, TO-92 package
   * $$T$$[°C]$$ = (V_o$$[mV]$$ - 424)/6.25$$
   * Accuracy: $$\pm{}3$$[°C]
* ADT7410: -55°C to +125°C, SOIC (smaller)
   * $$T$$[°C]$$ = 0.0625\times{}X$$ ($$X$$: output value)
   * Accuracy: $$\pm{}1$$[°C]
* Calibration required

---

# How sensors fail

* Open circuit (contact failure, output disruption)
   * Affects LM60s (Arduino ADCs)
* Short circuit (communication disruption)
   * Affects ADT7410 (Arduino I2C/TWI)
   * I2C clock disrupted and device hangs

---

# Failure mode model for open circuit

(Diagram here)

---

# Wire protocols(1): sensors - Arduino

* LM60: analog voltage
   * 0.17~1.2V
   * ATmega328P ADC: 0~1.1V
* ADT7410: I2C
   * Two wires: clock and data
   * Polled from ATmega328p (master)
   * Slow but sufficient (100kHz clock)

---

# Wire protocols(2): Arduino - host

* Arduino: USB Generic HID
   * Serial device
   * 115200bps for avrdude
   * 9600bps is sufficient for Bearfort
* Polling from host > broadcasting to host
   * 10 times/second is sufficient
   * Listening to USB consumes host CPU power
   * Requestint data on demand consumes less power

---

# Future directions and issues

* Field testing in the outdoor environment
* Testing with small Erlang/OTP computers
* Recovering from USB failure is the hardest part, which cannot be solely conducted by Erlang/OTP without OS facility

---

# Related work

* [Omer Kiric's talk on EF SF Bay 2013](http://www.erlang-factory.com/conference/ErlangUserConference2013/speakers/OmerKilic)
   * [Erlang/ALE on GitHub](https://github.com/esl/erlang_ale)
* [Frank Hunleth's talk on EF SF Bay 2015](http://www.erlang-factory.com/sfbay2015/frank-hunleth)
   * Embedded Elixir with Erlang/OTP and making a robot called *sumobot*
   * [elixirbot on GitHub](https://github.com/fhunleth/elixirbot)

---

# [fit] Thanks

# [fit] Questions?
