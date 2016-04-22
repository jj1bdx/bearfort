##############################################################################
# Makefile for the project avr-adctest
###############################################################################

## NOTE: GNU Make only

## General Flags
PROJECT = bearfort-arduino
MCU = atmega328p
TARGET = bearfort-arduino.elf
CC = avr-gcc

## Options common to compile, link and assembly rules
COMMON = -mmcu=$(MCU)

## Compile options common for all C compilation units.
CFLAGS = $(COMMON)
CFLAGS += -Wall -O3 -g -DF_CPU=16000000UL -std=c99
CFLAGS += -MD -MP -MT $(*F).o -MF $(@F).d 

## Linker flags
LDFLAGS = $(COMMON)
LDFLAGS += -Wl,-Map=bearfort-arduino.map

## Intel Hex file production flags
HEX_FLASH_FLAGS = -R .eeprom -R .fuse -R .lock -R .signature

HEX_EEPROM_FLAGS = -j .eeprom
HEX_EEPROM_FLAGS += --set-section-flags=.eeprom="alloc,load"
HEX_EEPROM_FLAGS += --change-section-lma .eeprom=0 --no-change-warnings

## Objects that must be built in order to link
OBJECTS = bearfort-arduino.o uart.o i2c.o

## Objects explicitly added by the user
LINKONLYOBJECTS = 

## Build
all: $(TARGET) bearfort-arduino.hex bearfort-arduino.eep bearfort-arduino.lss size

## Compile

%.o: %.c
	$(CC) -c $< $(CFLAGS) $(LDFLAGS) -o $@

##Link
$(TARGET): $(OBJECTS)
	 $(CC) $(LDFLAGS) $(OBJECTS) $(LINKONLYOBJECTS) $(LIBDIRS) $(LIBS) -o $(TARGET)

%.hex: $(TARGET)
	avr-objcopy -O ihex $(HEX_FLASH_FLAGS)  $< $@

%.eep: $(TARGET)
	-avr-objcopy -O ihex $< $@ || exit 0

%.lss: $(TARGET)
	avr-objdump -h -S $< > $@

size: ${TARGET}
	@echo
	@avr-size ${TARGET}
#	@avr-size -C --mcu=${MCU} ${TARGET}

## Clean target
.PHONY: clean
clean:
	-rm -rf $(OBJECTS) bearfort-arduino.elf dep/* bearfort-arduino.hex bearfort-arduino.eep bearfort-arduino.lss bearfort-arduino.map
