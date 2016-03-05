// The MIT License (MIT)
// 
// Copyright (c) 2009-2016 Kenji Rikitake
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

/*
 * ----------------------------------------------------------------------------
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <joerg@FreeBSD.ORG> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.        Joerg Wunsch
 * ----------------------------------------------------------------------------
 *
 * Based on <http://www.nongnu.org/avr-libc/examples/stdiodemo/uart.c>
 * $Id: uart.c 1008 2005-12-28 21:38:59Z joerg_wunsch $
 */

#include <stdint.h>
#include <stdio.h>
#include <avr/io.h>

#include "uart.h"

/*
 * Initialize the UART to 9600 Bd, tx/rx, 8N1.
 */
void
uart_init(void)
{
  UBRR0L = (F_CPU / (16UL * UART_BAUD)) - 1;
  UCSR0B = _BV(TXEN0) | _BV(RXEN0); /* tx/rx enable */
}

/*
 * Send character c down the UART Tx, wait until tx holding register
 * is empty.
 * No editing needed.
 */
int
uart_putchar(char c, FILE *stream)
{
  loop_until_bit_is_set(UCSR0A, UDRE0);
  UDR0 = c;

  return 0;
}

/*
 * Receive a character from the UART Rx.
 * No editing needed.
 */
int
uart_getchar(FILE *stream)
{
    uint8_t c;

    loop_until_bit_is_set(UCSR0A, RXC0);
	c = UDR0;
    return c;
}

