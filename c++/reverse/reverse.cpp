//--------------------------------------------------------------------------//
// reverse.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 et tw=78:
// Mon Jun  2 08:15:22 2008
//
//--------------------------------------------------------------------------//

#include <iostream>
#include <string.h>

//--------------------------------------------------------------------------//

int main(int argc, char const *argv[])
{
    if (argc != 2) {
        std::cerr << "Usage: reverse somestring" << std::endl;
        exit(1);
    }
    char* r = strdup(argv[1]);
    
    // Reverse r
    const int len = strlen(r);
    char tmp;
    for (int i = 0; i < (len+1)/2; i++) {
        // swap r[i], r[len-i-1]
        tmp = r[i];
        r[i] = r[len - i - 1];
        r[len - i - 1] = tmp;
    }
    
    std::cout << r << std::endl;
    free(r);
    
    return 0;
}