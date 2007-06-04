//--------------------------------------------------------------------------//
// kana_cpp.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 expandtab:
// Tue Mar 21 19:14:17 EST 2006
//--------------------------------------------------------------------------//

#include "kana_cpp.hpp"
#include <set>

//---------------------------------------------------------------------------//

py_ustring toKatakana(const py_ustring& sentence)
{
    py_ustring result;
    const int sentenceLen = sentence.length();
    result.reserve(sentenceLen);

    for (int i = 0; i < sentenceLen; ++i)
    {
        // only need to convert hiragana we encounter
        if (sentence[i] >= _hiraganaStarts && sentence[i] <= _hiraganaEnds)
        {
            // the kana sets are in the same order, so adding the difference
            // should suffice
            result.push_back(sentence[i] +_interKanaDistance);
        }
        else
        {
            result.push_back(sentence[i]);
        }
    }

    return result;
}

//---------------------------------------------------------------------------//

bool containsKanji(const py_ustring& sentence)
{
    return false;
}

//---------------------------------------------------------------------------//

MoraType charType(const py_ustring& sentence)
{
    const wchar_t unicodeChar = sentence[0];
    if (unicodeChar >= _hiraganaStarts && unicodeChar <= _hiraganaEnds)
    {
        return MoraType__Hiragana;
    }
    else if (unicodeChar >= _katakanaStarts && unicodeChar <= _katakanaEnds)
    {
        return MoraType__Katakana;
    }
    else if (unicodeChar >= _kanjiStarts && unicodeChar <= _kanjiEnds)
    {
        return MoraType__Kanji;
    }
    else if (unicodeChar <= 255)
    {
        return MoraType__Ascii;
    }
    else
    {
        return MoraType__Unknown;
    }
}

//---------------------------------------------------------------------------//

py_ustring uniqueKanji(const py_ustring& sentence)
{
    py_ustring result;

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

//---------------------------------------------------------------------------//

#include <boost/python/module.hpp>
#include <boost/python/def.hpp>
#include <boost/python/enum.hpp>
using namespace boost::python;

BOOST_PYTHON_MODULE(kana_cpp)
{
    def("uniqueKanji", &uniqueKanji);
    def("toKatakana", &toKatakana);
    def("containsKanji", &containsKanji);
    def("charType", &charType);

    enum_<MoraType>("MoraType")
        .value("Hiragana", MoraType__Hiragana)
        .value("Katakana", MoraType__Katakana)
        .value("Kanji", MoraType__Kanji)
        .value("Ascii", MoraType__Ascii)
        .value("Unknown", MoraType__Unknown)
        ;
}

