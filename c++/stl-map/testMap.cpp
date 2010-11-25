//--------------------------------------------------------------------------//
// testMap.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Sat Aug 25 12:14:40 EST 2007
//--------------------------------------------------------------------------//

#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <cstdlib>

using namespace std;

typedef map<string, int> freqTable;

void loadFrequencies(freqTable& f, const char* filename);
void testFrequencies(const freqTable& f);

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    freqTable f;
    loadFrequencies(f, "data.txt");
    testFrequencies(f);
    return 0;
}

void loadFrequencies(freqTable& f, const char* filename)
{
    string line;
    ifstream ifs(filename);
    if (!ifs.good()) {
        cerr << "Error: can't open the file " << filename << endl;
        exit(1);
    }

    string key;
    int count;
    while (ifs.peek() != EOF) {
        ifs >> key;
        ifs >> count;
        if (!ifs.eof()) {
            cout << key << " " << count << endl;
            f[key] = count;
        }
    }

    return;
}

void testFrequencies(const freqTable& f)
{
    freqTable::const_iterator cur = f.find(string("cat"));
    cout << "cat: ";
    if (cur != f.end()) {
        cout << (*cur).second << endl;
    } else {
        cout << "not found" << endl;
    }

    cur = f.find(string("apple"));
    cout << "apple: ";
    if (cur != f.end()) {
        cout << (*cur).second << endl;
    } else {
        cout << "not found" << endl;
    }

    return;
}

void split(string s, vector<string>& results, const string& delim)
{
    int cutAt;
    results.clear();

    while ((cutAt = s.find_first_of(delim)) != s.npos) {
        results.push_back(s.substr(0, cutAt));
        s = s.substr(cutAt + 1);
    }

    if (s.length() > 0)
    {
        results.push_back(s);
    }
    return;
}

void split(string s, vector<string>& results)
{
    const string delim = " \t\n";
    return split(s, results, delim);
}
