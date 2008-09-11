//--------------------------------------------------------------------------//
// nasty.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 expandtab:
// Thu Sep 11 19:05:46 EST 2008
//--------------------------------------------------------------------------//

#include <iostream>

using namespace std;

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    cout << "Maximizing memory" << endl;
    size_t mem10meg = 10*1024*1024;
    size_t s = mem10meg;
    size_t last_size = s;
    void* ptr = NULL;
    do {
        last_size = s; 
        if (ptr != NULL) {
            free(ptr);
        }
        s += mem10meg;
        ptr = malloc(s);
    } while (ptr != NULL);

    int found_size = last_size / (1024*1024);
    cout << "Reached limit of " << found_size << "Mb" << endl; 
    ptr = malloc(last_size);
    if (ptr == NULL)
    {
        cout << "Error mallocing " << found_size << "Mb" << endl;
        return 0;
    }
    char* c = (char*) ptr;
    while (true) {
        for (int i = 0; i < last_size; i++)
        {
            c[i] = i % 256;
        }
    }
    return 0;
}

