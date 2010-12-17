/*
 *
 *  read_lines_c.c
 *  code
 *
 *  Created by Lars Yencken on 2010-12-17.
 *  Copyright 2010 Lars Yencken. All rights reserved.
 *
 */

#include <stdio.h>

const int buf_size = 4096;
int
main(int argc, const char *argv[])
{
    char buffer[buf_size];

    while (fgets(buffer, buf_size, stdin)) {
        fputs(buffer, stdout);
    }

    return 0;
}
