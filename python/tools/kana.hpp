//--------------------------------------------------------------------------//
// kana.hpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 expandtab:
// Tue Mar 21 19:14:17 EST 2006
//--------------------------------------------------------------------------//

#include "jptools.hpp"

#include <string>
using namespace std;

//--------------------------------------------------------------------------//
// GLOBALS
//
// Here we store important constants which mark the boundaries of kana
// and kanji in UCS4 encoding (as used by python internals and std::wstring).
//--------------------------------------------------------------------------//

const int _interKanaDistance = 96;      // ord(ア) - ord(あ)

const wchar_t _hiraganaStarts = 0x3041; // ぁ character
const wchar_t _hiraganaEnds = 0x3096;   // hiragana ヶ character
const wchar_t _katakanaStarts = 0x30a1; // ァ character
const wchar_t _katakanaEnds = 0x30f6;   // ヶ character
const wchar_t _kanjiStarts = 0x4e00;    // 一 character
const wchar_t _kanjiEnds = 0x9fa5;      // 龥 character

const wchar_t _normalAsciiStarts = 0x21; // ! character
const wchar_t _fullAsciiStarts = 0xff01; // ！ character
const wchar_t _fullAsciiEnds = 0xff5f; // ｟ character

const pair<wchar_t, wchar_t> mappedChars[] = {
    make_pair(0x3000, 32),
    make_pair(0x3001, 44),
    make_pair(0x3002, 46)
};
const int mappedCharsLen = 3;

//---------------------------------------------------------------------------//
// Script:
//      Define the possible types a mora (defined as a single unicode
//      character) can take on.
//
enum Script {
    Script__Hiragana, Script__Katakana,
    Script__Kanji,
    Script__Ascii,
    Script__FullAscii,
    Script__Unknown
};

//--------------------------------------------------------------------------//
// getHiragana():
//      Return the hiragana alphabet as a single string.
//
wstring getHiragana();

//--------------------------------------------------------------------------//
// getKatakana():
//      Return the katakana alphabet as a single string.
//
wstring getKatakana();

//---------------------------------------------------------------------------//
// toKatakana():
//      Converts all the hiragana in a sentence to katakana.
//
wstring toKatakana(const wstring& sentence);

//---------------------------------------------------------------------------//
// toHiragana():
//      Converts all the katakana in a sentence to hiragana.
//
wstring toHiragana(const wstring& sentence);

//---------------------------------------------------------------------------//
// containsScript():
//      Returns true if there are any kanji in the sentence, false otherwise.
//
bool containsScript(Script sType, const wstring& sentence);

//---------------------------------------------------------------------------//
// uniqueKanji():
//      Returns a string containing only the unique kanji determined.
//
wstring uniqueKanji(const wstring& sentence);

//---------------------------------------------------------------------------//
// scriptType():
//      Determines the type of the given unicode character, classifying it as
//      either Ascii, Unknown, or a known Japanese script. If given a wide
//      string instead of a wide char, checks the type of the first character
//      of the string.
//
Script scriptType(wchar_t unicodeChar);
Script scriptType(const wstring& unicodeChar);

//---------------------------------------------------------------------------//
// sameKana():
//      Determines if the two strings are the same, ignoring differences in
//      script choice for kana (i.e. matching hiragana and katakana characters
//      are considered the same).
//
bool compareKana(const wstring& lhs, const wstring& rhs);

//--------------------------------------------------------------------------//

/**
 * Converts any wide roman characters which are found to their corresponding
 * normal roman characters.
 *
 * @param sentence The string to normalize.
 * @return The same string, with all double-width roman chars converted.  
 */
wstring normalizeAscii(const wstring& sentence);

//--------------------------------------------------------------------------//

