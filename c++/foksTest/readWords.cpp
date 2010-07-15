//--------------------------------------------------------------------------//
// readWords.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Thu Aug 30 18:03:50 EST 2007
//--------------------------------------------------------------------------//

#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>

using namespace std;

#include <stdio.h>
#include <string.h>

#include "utf8.h"

//--------------------------------------------------------------------------//

#define MAX_WORD_LEN    50

//--------------------------------------------------------------------------//

void loadWords(const char* inputFile, vector<wstring>& words);
void dumpWords(const vector<wstring>& words, const char* outputFile);

//--------------------------------------------------------------------------//

int main(int argc, char* argv[])
{
    if (argc != 3) {
        cerr << "Usage: readWords inputFile outputFile" << endl;
        exit(EXIT_FAILURE);
    }

    char* inputFile = argv[1];
    char* outputFile = argv[2];

    vector<wstring> words;
    loadWords(inputFile, words);
    dumpWords(words, outputFile);
    return 0;
}

//--------------------------------------------------------------------------//

/**
 * Loads all the words from the input file into a vector, converting from utf8
 * to ucs4 as we go.
 */
void loadWords(const char* inputFile, vector<wstring>& words)
{
    words.clear();
    words.reserve(700000);

    // Read in the entire file.
    FILE* iStream = fopen(inputFile, "r");
    if (iStream == NULL) {
        cerr << "Couldn't open file " << inputFile << endl;
        exit(EXIT_FAILURE);
    }

    {
        char wordBytes[MAX_WORD_LEN*3];
        wstring unicodeWord;
        while (fgets(wordBytes, MAX_WORD_LEN*3, iStream) != NULL) {
            unicodeWord.clear();
            utf8::utf8to32(wordBytes, wordBytes + strlen(wordBytes),
                    back_inserter(unicodeWord));
            words.push_back(unicodeWord);
        }
        cout << "Loaded " << words.size() << " words" << endl;
    }
    fclose(iStream);

    return;
}

//--------------------------------------------------------------------------//

/**
 * Dumps the words to the given filename in utf8 encoding.
 */
void dumpWords(const vector<wstring>& words, const char* outputFile)
{
    ofstream ofs(outputFile);
    string str;
    for (vector<wstring>::const_iterator i = words.begin();
            i != words.end(); i++) {
        str.clear();
        utf8::utf32to8(i->begin(), i->end(), back_inserter(str));
        ofs << str;
    }
    ofs.close();
    return;
}

//--------------------------------------------------------------------------//
