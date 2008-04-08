//--------------------------------------------------------------------------//
// readingModel.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Tue Aug 28 22:46:11 EST 2007
//--------------------------------------------------------------------------//

#include "readingModel.hpp"

//--------------------------------------------------------------------------//

ReadingModel::ReadingModel(const Glib::ustring& kanjiDicA,
        const Glib::ustring& kanjidicB)
{
    _loadFile(kanjiDicA);
}

ReadingModel::_loadFile(const Glib::ustring& filename)
{
    try {
    } catch Glib::FileError& e {
    }
}
