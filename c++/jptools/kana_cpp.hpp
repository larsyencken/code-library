//--------------------------------------------------------------------------//
// kana_cpp.hpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 expandtab:
// Tue Mar 21 19:14:17 EST 2006
//--------------------------------------------------------------------------//

#include <string>
#include <glibmm.h>

using namespace Glib;

typedef std::wstring py_ustring;

//---------------------------------------------------------------------------//

const ustring hiragana("ぁあぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただち>ぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろゎわゐゑをんゔゕゖ");

const py_ustring py_hiragana(hiragana.begin(), hiragana.end());

const ustring katakana("ァアィイゥウェエォオカガキギクグケゲコゴサザシジスズセゼソゾタダチ>ヂッツヅテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレロヮワヰヱヲンヴヵヶ");

//---------------------------------------------------------------------------//

const int _interKanaDistance = katakana[0] - hiragana[0];

const wchar_t _hiraganaStarts = 12353;  // ぁ character
const wchar_t _hiraganaEnds = 12438;    // hiragana ヶ character
const wchar_t _katakanaStarts = 12449;  // ァ character
const wchar_t _katakanaEnds = 12534;    // ヶ character
const wchar_t _kanjiStarts = 19968;     // 一 character
const wchar_t _kanjiEnds = 40869;       // 龥 character

//---------------------------------------------------------------------------//
// toKatakana():
//  Converts all the hiragana in a sentence to katakana.
//
py_ustring toKatakana(const py_ustring& sentence);

//---------------------------------------------------------------------------//
// containsKanji():
//  Returns true if there are any kanji in the sentence, false otherwise.
//
bool containsKanji(const py_ustring& sentence);

//---------------------------------------------------------------------------//
// uniqueKanji():
//  Returns a string containing only the unique kanji determined.
py_ustring uniqueKanji(const py_ustring& sentence);

//---------------------------------------------------------------------------//
// Define the possible types a mora (defined as a single unicode character)
// can take on.
//
enum MoraType {
    MoraType__Hiragana,
    MoraType__Katakana,
    MoraType__Kanji,
    MoraType__Ascii,
    MoraType__Unknown
};

//---------------------------------------------------------------------------//
// moraType():
//  Determines the type of the given unicode character, classifying it as
//  either Ascii, Unknown, or a known Japanese script.
//
MoraType charType(const py_ustring& unicodeChar);

//---------------------------------------------------------------------------//

