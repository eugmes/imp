#ifndef IMPSTD_H
#define IMPSTD_H
#include <stdint.h>
#include <stdbool.h>

int32_t _IMP_input_integer(void);
void _IMP_output_integer(int32_t);
void _IMP_output_boolean(bool);
void _IMP_output_string(const char *);
void _IMP_halt(void);
void _IMP_newline(void);
void _IMP_constraint_error_ex(const char *file_name, int32_t line_no) __attribute__((noreturn));
void _IMP_program_error_ex(const char *file_name, int32_t line_no) __attribute__((noreturn));

#endif
