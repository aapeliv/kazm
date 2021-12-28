#include <stdio.h>
#include <malloc.h>

void println(char* str) {
  printf("%s\n", str);
}

void print(char* str) {
  printf("%s", str);
}

void int_print(int val) {
  printf("%d", val);
}

void int_println(int val) {
  printf("%d\n", val);
}

void double_print(double val) {
  printf("%f", val);
}

void double_println(double val) {
  printf("%f\n", val);
}

// Useful before variables work to test loops, etc
int _the_next_int = 0;
int next_int() {
  return _the_next_int++;
}
