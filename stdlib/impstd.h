#ifndef IMPSTD_H
#define IMPSTD_H
#include <stdint.h>
#include <stdbool.h>

int32_t _IMP_input_integer(void);
bool _IMP_input_boolean(void);
void _IMP_output_integer(int32_t);
void _IMP_output_boolean(bool);
void _IMP_output_string(const char *);
void _IMP_halt(void);
void _IMP_newline(void);

void _IMP_divide_by_zero_ex(void) __attribute__((noreturn));

#endif
