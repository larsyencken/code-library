//--------------------------------------------------------------------------//
// tokenizer.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 expandtab:
// Sat 18 Sep 2010 09:02:35 EST
//--------------------------------------------------------------------------//

#include <iostream>
#include <boost/tokenizer.hpp>

typedef boost::tokenizer< boost::char_separator<char> > tokenizer;

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    std::string line;
    boost::char_separator<char> sep(" ");
    while (std::cin) {
        getline(std::cin, line);
        tokenizer tokens(line, sep);
        bool first = true;
        for (tokenizer::iterator tok_iter = tokens.begin();
                tok_iter != tokens.end(); ++tok_iter) {
            if (!first) {
                std::cout << " ";
            }
            std::cout << *tok_iter;
            first = false;
        }
        std::cout << std::endl;
    }
    return 0;
}

