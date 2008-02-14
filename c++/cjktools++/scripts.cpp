//--------------------------------------------------------------------------//

#include "scripts.hpp"

//--------------------------------------------------------------------------//

const int _interKanaDistance = 96;
 
Script scriptType(wchar_t c)
{
    if (c >= 0x0021 && c <= 0x00ff) {
        return Script__Ascii;
    } else if (c >= 0x3041 && c <= 0x3096) {
        return Script__Hiragana;
    } else if (c >= 0x30a1 && c <= 0x30f6) {
        return Script__Katakana;
    } else if (c >= 0x4e00 && c <= 0x9fa5) {
        return Script__Kanji;
    } else if (c >= 0xff01 && c <= 0xff5f) {
        return Script__DoubleAscii;
    } else {
        return Script__Unknown;
    }
}

//--------------------------------------------------------------------------//

Script scriptType(const wstring& s)
{
    if (s.size() > 0) {
        return scriptType(s[0]);
    } else {
        return Script__Unknown;
    }
}

//--------------------------------------------------------------------------//

void scriptTypes(const wstring& s, set<Script>& scripts)
{
    scripts.clear();
    for (wstring::const_iterator i = s.begin(); i != s.end(); i++)
    {
        scripts.insert(scriptType(*i));
    }
    return;
}

//--------------------------------------------------------------------------//

void scriptBoundaries(const wstring& s, vector<scriptString>& results)
{
    results.clear();
    if (s.size() == 0) {
        return;
    }

    int i = 0, startSeg = 0;
    Script lastScript = scriptType(s[0]);
    Script thisScript;
    i++;

    const int sSize = s.size();
    while (i < sSize) {
        thisScript = scriptType(s[i]);
        if (thisScript != lastScript) {
            results.push_back(make_pair(lastScript, s.substr(startSeg, i)));
            startSeg = i;
            lastScript = thisScript;
        }
        i++;
    }
    results.push_back(make_pair(lastScript, s.substr(startSeg)));
    return;
}

//--------------------------------------------------------------------------//

bool containsScript(const wstring& s, Script script)
{
    for (wstring::const_iterator i = s.begin(); i != s.end(); i++) {
        if (scriptType(*i) == script) {
            return true;
        }
    }
    return false;
}

//--------------------------------------------------------------------------//

bool onlyScript(const wstring& s, Script script)
{
    for (wstring::const_iterator i = s.begin(); i != s.end(); i++) {
        if (scriptType(*i) != script) {
            return false;
        }
    }
    return true;
}

//--------------------------------------------------------------------------//
