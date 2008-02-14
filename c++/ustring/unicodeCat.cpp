//--------------------------------------------------------------------------//
// unicodeCat.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Thu Aug 23 21:22:48 EST 2007
//--------------------------------------------------------------------------//

#include <iostream>
#include <fstream>
#include <vector>
#include <glibmm.h>

using namespace std;
using namespace Glib;

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        cerr << "Usage: unicodeCat inputFile outputFile\n";
        return 1;
    }

    const char* inputFile = argv[1];
    const char* outputFile = argv[2];

    /*
    {
        wofstream ofs("data.ucd");
        ofs.imbue(utf8_locale);
        copy(ucs4_data.begin(), ucs4_data.end(),
                ostream_iterator<ucs4_t,ucs4_t>(ofs));
    }
    */

    vector<ustring> from_file;
    ustring line;
    {
        RefPtr<IOChannel> ifs = IOChannel::create_from_file(inputFile, "r");
        while (ifs->read_line(line) == IO_STATUS_NORMAL) {
            from_file.push_back(line);
        }
        ifs->close();
    }

    {
        RefPtr<IOChannel> ofs = IOChannel::create_from_file(outputFile, "w");
        for (vector<ustring>::iterator i = from_file.begin();
                i != from_file.end(); i++) {
            ofs->write(*i);
        }
        ofs->close();
    }

    return 0;
}
