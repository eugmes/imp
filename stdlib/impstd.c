#include "impstd.h"
#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

int32_t _IMP_input_integer(void)
{
    int32_t val;
    int ret = fscanf(stdin, "%"PRId32, &val);
    if (ret != 1) {
        fprintf(stderr, "IMP ERROR: input failed\n");
        exit(EXIT_FAILURE);
    }

    return val;
}

void _IMP_output_integer(int32_t val)
{
    printf("%"PRId32, val);
}

void _IMP_output_boolean(bool val)
{
    printf("%s", val ? "true" : "false");
}

void _IMP_output_string(const char *s)
{
    printf("%s", s);
}

void _IMP_halt(void)
{
    fprintf(stderr, "IMP HALTED\n");
    exit(0);
}

void _IMP_newline(void)
{
    printf("\n");
}
