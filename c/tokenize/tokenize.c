//--------------------------------------------------------------------------//
// tokenize.c
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 expandtab:
// Sat 18 Sep 2010 11:11:47 EST
//--------------------------------------------------------------------------//

#include <stdio.h>
#include <string.h>

typedef int bool;

#define true 1
#define false 0

void strip_newline(char* s) {
    int l = strlen(s);
    if (l > 0 && s[l - 1] == '\n') {
        s[l - 1] = '\0';
    }
}

void print_tokens(char* buffer) {
    bool first = true;
    char* next = buffer;
    char* offset = strsep(&next, " ");
    while (next != NULL) {
        if (!first) {
            printf(" ");
        }
        printf("%s", offset);
        offset = strsep(&next, " ");
        first = false;
    }
    if (!first) {
        printf(" ");
    }
    printf("%s", offset);
}

int main(int argc, char *argv[])
{
    const int buf_size = 4096;
    char buffer[buf_size + 1];
    buffer[0] = '\0';
    buffer[buf_size] = '\0';

    while (fgets(buffer, buf_size, stdin) != NULL) {
        print_tokens(buffer);
    }
    return 0;
}

