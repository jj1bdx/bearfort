/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2009-2015 Kenji Rikitake
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
 * for hardware randomizer connected between
 * AIN1 (PD7, digital pin 7) and
 * AIN0 (PD6, digital pin 6)
 * of Arduino Duemilanove hardware (ATmega168)
 * by Kenji Rikitake
 */

/* #define F_CPU (16000000UL) */

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>

/* UART library */

#include "uart.h"

FILE uart_str = FDEV_SETUP_STREAM(uart_putchar, uart_getchar, _FDEV_SETUP_RW);

/* I2C library */

#include <util/twi.h>
#include "i2c_master.h"

#define ADT7410_WRITE 0x90
#define ADT7410_READ 0x91

/* temperature macros */

#define TC1 ((double)1.065) // 1.090/1023*1000
#define LM60TEMP(x) ((((double)x) * TC1 - 424.0) / 6.25)
#define ADTTEMP(x) (((double)((int16_t)x)) / (double)128.0)

char buffer[8];

/* initialize IO ports */
static void ioinit(void) {

    /* disable interrupt before initialization*/
    cli();

    /*
     * forced initialization
     * of unnecessary interrupts
     */

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

    /*
     * TXD/PD1/Pin 1 to output
     * RXD/PD0/Pin 0 to input 
     */

    DDRD = 0xfe;
    PORTD = 0xff;

    /* PCx all input, no pullup */
    DDRC = 0x00;
    PORTC = 0x00;

    /* PB5 = LED on Arduino 2009 */
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
    /* timer period: 4 microseconds = 64 machine cycles */
    OCR0A = (4*2) - 1;
    /* clk/8 (0.5 microseconds / count) */
    /* start timer */
    TCCR0B = 0x02;

    /* Enable the ADC */
    /* ADC clock 1/128, interrupt disabled, no auto trigger */
    ADCSRA = 0x07;
    ADCSRB = 0x00;
    ADMUX = 0xC0;
    ACSR = 0x80;
    /* Enable ADC0 to ADC3 */
    DIDR0 = 0x0f;

    ADCSRA |= _BV(ADEN);

    /* initialize I2C */
    i2c_init();
    /* initialize ADT7410 */
    if(!i2c_start(ADT7410_WRITE) &&
       !i2c_write(0x03) &&
       // 15-bit precision mode
       !i2c_write(0x80)) {
        i2c_stop();
    }

    /* enable interrupt after initialization*/
    sei();
}

uint16_t adc_read(uint8_t adcx) {

    ADMUX &= 0xf0;
    ADMUX |= adcx;
    /* XXX: Wait 500us here to measure internal VBG */
    _delay_us(500.0);
    ADCSRA |= _BV(ADSC);
    while ( (ADCSRA & _BV(ADSC)) );
    return ADC;
}

uint16_t get_ADT7410(void){

	uint16_t temp;
    
    // temperature register
    if(!i2c_start(ADT7410_WRITE) &&
       !i2c_write(0x00)) {
        i2c_stop();
    }

    if(!i2c_start(ADT7410_READ)) {
        temp = ((uint8_t)i2c_read_ack() << 8);
        temp |= ((uint8_t)i2c_read_nack());
        i2c_stop();
    }

    return temp;
}

/* main function */

int main() {

    /* initialize ports, timers, serial, and IRQ */
    ioinit();
    stdin = &uart_str;
    stdout = &uart_str;

    double adt0, a0, a1, a2, a3;
    for (;;) {
        _delay_ms(90.0);
        adt0 = ADTTEMP(get_ADT7410());
        a0 = LM60TEMP(adc_read(0));
        a1 = LM60TEMP(adc_read(1));
        a2 = LM60TEMP(adc_read(2));
        a3 = LM60TEMP(adc_read(3));
        // fprintf(stdout, "{ %7.2f, %3.2f, %3.2f, %3.2f, %3.2f } \n",
        //        adt0, a0, a1, a2, a3);
        fputs("{ ", stdout);
        dtostrf(adt0, 6, 2, buffer);
        fputs(buffer, stdout);
        fputs(", ", stdout);
        dtostrf(a0, 6, 2, buffer);
        fputs(buffer, stdout);
        fputs(", ", stdout);
        dtostrf(a1, 6, 2, buffer);
        fputs(buffer, stdout);
        fputs(", ", stdout);
        dtostrf(a2, 6, 2, buffer);
        fputs(buffer, stdout);
        fputs(", ", stdout);
        dtostrf(a3, 6, 2, buffer);
        fputs(buffer, stdout);
        fputs("}.\n", stdout);
        fflush(stdout);
    }
    /* NOTREACHED */
    return 0;
}

/* end of code */
