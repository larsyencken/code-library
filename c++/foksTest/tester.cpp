//--------------------------------------------------------------------------//
// tester.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Fri Aug 31 11:01:16 EST 2007
//--------------------------------------------------------------------------//

#include <iostream>
#include <fstream>
#include <string>

using namespace std;

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " inputFile" << endl; 
        exit(EXIT_FAILURE);
    }
    wifstream ifs(argv[1]);
    wchar_t line[150];
    wstring word;
    while (ifs.good()) {
        ifs.getline(line, 150);
        word = line;
        wcout << word;
    }
    if (ifs.bad()) {
        cerr << "Error: " << endl;
        exit(EXIT_FAILURE);
        iostate ifs.rdstate()
    }
    return 0;
}

