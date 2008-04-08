#include <set>
#include <vector>
#include <glibmm.h>

using namespace std; 

enum Script { 
    Script__Kanji,
    Script__Hiragana,
    Script__Katakana,
    Script__Ascii,
    Script__DoubleAscii,
    Script__Unknown,
};

/**
 * Returns the script type of the given character.
 */
Script scriptType(wchar_t c);

/**
 * Returns the script type of the first character in the given string. If the
 * string is empty, returns the unknown script.
 */
Script scriptType(const wstring& s);

/**
 * Determines the set of all Scripts used in the given string.
 */
void scriptTypes(const wstring& s, set<Script>& scripts);

typedef pair<Script, wstring> scriptString;

/**
 * Determines the natural boundaries of the string as determined by script
 * changes.
 */
void scriptBoundaries(const wstring& s, vector<scriptString>& results);

/**
 * Returns true if the string contains any characters of the given script,
 * false otherwise.
 */
bool containsScript(const wstring& s, Script script);

/**
 * Returns true if the only script used is the given script, false otherwise.
 */
bool onlyScript(const wstring& s, Script script);
