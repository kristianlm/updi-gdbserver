#include <avr/io.h>
#include <util/delay.h>

int main(void) {
  PORTA.DIR = (1 << PIN6_bp);
  while (1) {
    PORTA.OUT |=  (1 << PIN6_bp); _delay_ms(100);
    PORTA.OUT &= ~(1 << PIN6_bp); _delay_ms(100);
  }
}

