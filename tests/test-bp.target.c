#include <avr/io.h>
#include <util/delay.h>

int main(void) {
  PORTA.DIR = (1 << PIN6_bp);
  uint8_t on = (1 << PIN6_bp);
  uint8_t off = 0;
  while (1) {
    PORTA.OUT = on;
    PORTA.OUT = off;
  }
}
