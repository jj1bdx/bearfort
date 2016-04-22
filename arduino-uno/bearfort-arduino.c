/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015-2016 Kenji Rikitake
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
/*
 * Bearfort sensor firmware
 * for Arduino UNO R3 and
 * Bearfort sensor shield
 * (ADT7410 on TWI/I2C,
 *  LM60 (or LM61) on ADC0 to ADC3 (four LM60s)
 * by Kenji Rikitake
 */

/* #define F_CPU (16000000UL) */

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/eeprom.h>
#include <util/delay.h>

/* UART library */

#include "uart.h"

FILE uart_str = FDEV_SETUP_STREAM(uart_putchar, uart_getchar, _FDEV_SETUP_RW);

/* I2C library */

#include <util/twi.h>
#include <alloca.h>
#include "i2c.h"

#define ADT7410_ADDRESS 0x48

/* temperature macros */

#define TC1 ((double)1.065) // 1.090/1023*1000
#define LM60TEMP(x) ((((double)x) * TC1 - 424.0) / 6.25)
#define ADTTEMP(x) (((double)((int16_t)x)) / (double)128.0)

uint16_t device_id;
char buffer[8];

// Initialize ADT7410 via I2C

void init_ADT7410(void) {

    i2c_txn_t *t;
    // 15-bit precision mode
    uint8_t msg[] = {0x03, 0x80};

    t = (i2c_txn_t *)alloca(sizeof(*t) + 2 * sizeof(t->ops[0]));
    i2c_txn_init(t, 1);
    // initialize ADT7410
    i2c_op_init_wr(&t->ops[0], ADT7410_ADDRESS , msg, sizeof(msg));

    i2c_post(t);
    // Wait until completion of the transaction
    while (!(t->flags & I2C_TXN_DONE)) {}
}

/* initialize IO ports */

static void ioinit(void) {

    // disable interrupt before initialization
    cli();

    // forced initialization of unnecessary interrupts

    MCUCR = 0x00;
    EICRA = 0x00;
    EIMSK = 0x00;
    TIMSK0 = 0x00;
    TIMSK1 = 0x00;
    TIMSK2 = 0x00;
    PCMSK0 = 0x00;
    PCMSK1 = 0x00;
    PCMSK2 = 0x00;
    PCICR = 0x00;
    PRR = 0x00;

    // TXD/PD1/Pin 1 to output
    // RXD/PD0/Pin 0 to input 

    DDRD = 0xfe;
    PORTD = 0xff;

    // PCx all input, no pullup
    DDRC = 0x00;
    PORTC = 0x00;

    // PB5 = LED on Arduino 2009
    DDRB = 0xff;
    PORTB = 0x3f;

    /* USART 0 */
    /* no USART IRQ, disable TX/RX */
    UCSR0B = 0x00;
    /* clk 1x */
    UCSR0A = 0x00;
    /* async, no parity, 8bit */
    UCSR0C = 0x06;
    /* see uart.c */
    uart_init();

    /* Timer 0 */
    /* timer stop */
    TCCR0B = 0x00;
    /* reset counter */
    TCNT0 = 0;
    /* no external output, CTC */
    TCCR0A = 0x02;

    /* Enable the ADC */
    /* ADC clock 1/128, interrupt disabled, no auto trigger */
    ADCSRA = 0x07;
    ADCSRB = 0x00;
    ADMUX = 0xC0;
    ACSR = 0x80;
    /* Enable ADC0 to ADC3 */
    DIDR0 = 0x0f;

    ADCSRA |= _BV(ADEN);

    /* Read device ID from EEPROM */
    eeprom_busy_wait();

    uint16_t address = 0;
    /* little endian from address 0x00 */
    device_id = eeprom_read_word((uint16_t *)address);

    /* enable interrupt for avr-i2c library */
    sei();

    /* initialize I2C */
    i2c_init();

    // initialize ADT7410
    init_ADT7410();

}

uint16_t adc_read(uint8_t adcx) {

    ADMUX &= 0xf0;
    ADMUX |= adcx;
    /* XXX: Wait 500us here to measure internal VBG */
    /* _delay_us(500.0); */
    ADCSRA |= _BV(ADSC);
    while ( (ADCSRA & _BV(ADSC)) );
    return ADC;
}

uint16_t get_ADT7410(void) {

    i2c_txn_t *t;
	uint16_t temp = 0x8000;
    uint8_t msg[] = {0x00};
    uint8_t tempbytes[2];
    
    t = (i2c_txn_t *)alloca(sizeof(*t) + 2 * sizeof(t->ops[0]));
    i2c_txn_init(t, 2);
    // Read from temperature registers
    i2c_op_init_wr(&t->ops[0], ADT7410_ADDRESS, msg, sizeof(msg));
    i2c_op_init_rd(&t->ops[1], ADT7410_ADDRESS, tempbytes, sizeof(tempbytes));

    i2c_post(t);
    // Wait until completion of the transaction
    while (!(t->flags & I2C_TXN_DONE)) {}

    if (t->flags & I2C_TXN_ERR) {
        // error
        temp = 0x8000;
    } else {
        temp = (tempbytes[0] << 8) | tempbytes[1];
    }

    return temp;
}

/* main function */

int main() {

    /* initialize ports, timers, serial, and IRQ */
    ioinit();
    stdin = &uart_str;
    stdout = &uart_str;

    int16_t adt0, a0, a1, a2, a3;
    int ch;
    uint8_t *p;

    for (;;) {

        ch = getchar();
        // 0x20 == " " (space)
        // if not space, skip rest of code
        if (ch != 0x20) continue;
        // rest in for loop skipped if not space

        adt0 = (int16_t)get_ADT7410();
        a0 = adc_read(0);
        a1 = adc_read(1);
        a2 = adc_read(2);
        a3 = adc_read(3);

        // 16 bytes in total
        // 0x02 0x51 0x82
        // device_id adt0 a0 a1 a2 a3 (little endian, 2-bytes each)
        // 0x03 

        fputc(0x02, stdout);
        fputc(0x51, stdout);
        fputc(0x82, stdout);

        p = (uint8_t *)&device_id;
        fputc(*p++, stdout);
        fputc(*p, stdout);

        p = (uint8_t *)&adt0;
        fputc(*p++, stdout);
        fputc(*p, stdout);

        p = (uint8_t *)&a0;
        fputc(*p++, stdout);
        fputc(*p, stdout);

        p = (uint8_t *)&a1;
        fputc(*p++, stdout);
        fputc(*p, stdout);

        p = (uint8_t *)&a2;
        fputc(*p++, stdout);
        fputc(*p, stdout);

        p = (uint8_t *)&a3;
        fputc(*p++, stdout);
        fputc(*p, stdout);

        fputc(0x03, stdout);

        fflush(stdout);
    }
    /* NOTREACHED */
}

/* end of code */
