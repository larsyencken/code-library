//--------------------------------------------------------------------------//
// kana.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 expandtab:
// Tue Mar 21 19:14:17 EST 2006
//--------------------------------------------------------------------------//

#include "kana.hpp"

#include <set>
#include <stdexcept>

//--------------------------------------------------------------------------//
// TODO:
//  - Docstrings are missing for:
//      toHiragana
//      toKatakana
//      uniqueKanji
//      containsScript
//      scriptType
//      sameKana
//
//  - Test cases are missing for:
//      uniqueKanji
//      sameKana
//--------------------------------------------------------------------------//

//--------------------------------------------------------------------------//

wstring getHiragana()
{
    wstring hiragana;

    for (wchar_t c = _hiraganaStarts; c <= _hiraganaEnds; ++c)
    {
        hiragana.push_back(c);
    }

    return hiragana;
}

const char* getHiragana__doc__ =
    "getHiragana() -> hiragana\n\
    \n\
    Returns a string containing all the hiragana, in order.";

//--------------------------------------------------------------------------//

wstring getKatakana()
{
    wstring katakana;

    for (wchar_t c = _katakanaStarts; c <= _katakanaEnds; ++c)
    {
        katakana.push_back(c);
    }

    return katakana;
}

const char* getKatakana__doc__ =
    "getKatakana() -> katakana\n\
    \n\
    Returns a string containing all katakana in order.";

//--------------------------------------------------------------------------//

wstring toKatakana(const wstring& sentence)
{
    wstring result;

    for (wstring::const_iterator iter = sentence.begin();
            iter != sentence.end(); ++iter)
    {
        // only need to convert hiragana we encounter
        if (*iter >= _hiraganaStarts && *iter <= _hiraganaEnds)
        {
            // here we add the value (ord(ア) - ord(あ))
            result.push_back(*iter + _interKanaDistance);
        }
        else
        {
            result.push_back(*iter);
        }
    }

    return result;
}

const char* toKatakana__doc__ =
    "toKatakana(jString) -> jString\n\
    \n\
    Returns a string identical to the input, but all hiragana have been\n\
    replaced by their matching katakana.";

//---------------------------------------------------------------------------//

wstring toHiragana(const wstring& sentence)
{
    wstring result;

    for (wstring::const_iterator iter = sentence.begin();
            iter != sentence.end(); ++iter)
    {
        // only need to convert katakana we encounter
        if (*iter >= _katakanaStarts && *iter <= _katakanaEnds)
        {
            // here we subtract the value (ord(ア) - ord(あ))
            result.push_back(*iter - _interKanaDistance);
        }
        else
        {
            result.push_back(*iter);
        }
    }

    return result;
}

const char* toHiragana__doc__ =
    "toHiragana(jString) -> jString\n\
    \n\
    Returns a string identical to the input, but all katakana have been\n\
    replaced by their matching hiragana.";

//---------------------------------------------------------------------------//

bool containsScript(Script script, const wstring& sentence)
{
    for (wstring::const_iterator iter = sentence.begin();
            iter != sentence.end(); ++iter)
    {
        wchar_t c = *iter;
        if (scriptType(c) == script)
        {
            return true;
        }
    }

    return false;
}

const char* containsScript__doc__ =
    "containsScript(script, jString) -> bool\n\
    \n\
    Returns True if the given string contains at least one character of\n\
    the desired script, False otherwise.";

//---------------------------------------------------------------------------//

Script scriptType(wchar_t unicodeChar)
{
    if (unicodeChar >= _hiraganaStarts && unicodeChar <= _hiraganaEnds)
    {
        return Script__Hiragana;
    }
    else if (unicodeChar >= _katakanaStarts && unicodeChar <= _katakanaEnds)
    {
        return Script__Katakana;
    }
    else if (unicodeChar >= _kanjiStarts && unicodeChar <= _kanjiEnds)
    {
        return Script__Kanji;
    }
    else if (unicodeChar <= 255)
    {
        return Script__Ascii;
    }
    else if (unicodeChar >= _fullAsciiStarts && unicodeChar <= _fullAsciiEnds)
    {
        return Script__FullAscii;
    }
    else
    {
        return Script__Unknown;
    }
}

const char* scriptType__doc__ =
    "scriptType(unicodeChar) -> script\n\
    \n\
    Determines the type of the given character. If a string is given \n\
    instead of a single character, returns the type of the first char \n\
    in the string instead.";

//--------------------------------------------------------------------------//

Script scriptType(const wstring& sentence)
{
    if (sentence.size() == 0)
    {
        throw range_error("Can't check the script of an empty string");
    }
    return scriptType(sentence[0]);
}

//--------------------------------------------------------------------------//

wstring uniqueKanji(const wstring& sentence)
{
    wstring result;

    // construct a set from the characters in the given sentence
    std::set<wchar_t> uniqueChars(sentence.begin(), sentence.end());

    result.reserve(uniqueChars.size());

    for (std::set<wchar_t>::iterator iter = uniqueChars.begin();
            iter != uniqueChars.end(); ++iter)
    {
        // only output entries which are kanji
        const wchar_t potentialKanji = *iter;
        if (potentialKanji >= _kanjiStarts && potentialKanji <= _kanjiEnds)
        {
            result.push_back(potentialKanji);
        }
    }

    return result;
}

const char* uniqueKanji__doc__ =
    "uniqueKanji(jString) -> set(kanji)\n\
    \n\
    Returns a set containing all the unique kanji found in the given string.";

//---------------------------------------------------------------------------//

bool compareKana(const wstring& lhs, const wstring& rhs)
{
    return toKatakana(lhs) == toKatakana(rhs);
}

const char* compareKana__doc__ =
    "compareKana(stringA, stringB) -> bool\n\
    \n\
    Determines if the two strings containing kana are equivalent, assuming\n\
    that matching hiragana and katakana characters are equivalent.";

//--------------------------------------------------------------------------//

wstring normalizeAscii(const wstring& sentence)
{
    wstring result(sentence);
    wchar_t thisChar;
    const int sentenceSize = result.size();

    for (int i = 0; i < sentenceSize; ++i)
    {
        thisChar = result[i];

        if (thisChar >= _fullAsciiStarts && thisChar <= _fullAsciiEnds) {
            // If it's full width ascii, normalize it.
            result[i] = _normalAsciiStarts + (thisChar - _fullAsciiStarts);
        } else {
            // Check for it in our small table of mapped chars.
            for (int j = 0; j < mappedCharsLen; ++j)
            {
                if (mappedChars[j].first == thisChar)
                {
                    result[i] = mappedChars[j].second;
                    break;
                }
            }
        }

    }

    return result;
}

const char* normalizeAscii__doc__ = 
    "normalizeAscii(mixedString) -> normalizedString\n\
    \n\
    Converts any double width ascii to regular ascii characters.";

//--------------------------------------------------------------------------//

// Declare some function pointers in order to get overloading
Script (*scriptType_char)(wchar_t) = &scriptType;
Script (*scriptType_wstring)(const wstring&) = &scriptType;

BOOST_PYTHON_MODULE(cKana)
{
    def("getHiragana", &getHiragana, getHiragana__doc__);
    def("getKatakana", &getKatakana, getKatakana__doc__);
    def("toHiragana", &toHiragana, toHiragana__doc__);
    def("toKatakana", &toKatakana, toKatakana__doc__);
    def("uniqueKanji", &uniqueKanji, uniqueKanji__doc__);
    def("containsScript", &containsScript, containsScript__doc__);
    def("compareKana", &compareKana, compareKana__doc__);
    def("normalizeAscii", &normalizeAscii, normalizeAscii__doc__);

    def("scriptType", scriptType_char, scriptType__doc__);
    def("scriptType", scriptType_wstring);

    enum_<Script>("Script")
        .value("Hiragana", Script__Hiragana)
        .value("Katakana", Script__Katakana)
        .value("Kanji", Script__Kanji)
        .value("Ascii", Script__Ascii)
        .value("FullAscii", Script__FullAscii)
        .value("Unknown", Script__Unknown)
        ;
}

