//--------------------------------------------------------------------------//
// readingModel.hpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Sat Aug 25 14:24:41 EST 2007
//--------------------------------------------------------------------------//

#include <map>
#include <vector>
#include <set>

#include "common.hpp"

//--------------------------------------------------------------------------//

struct KanjiEntry
{
    Glib::ustring kanji;
    std::vector<ustringPtr> readings;

    bool operator<(const KanjiEntry& rhs)
    {
        return kanji < rhs.kanji; 
    }

    bool operator==(const KanjiEntry& rhs)
    {
        return kanji == rhs.kanji;
    }
};

typedef Glib::RefPtr<KanjiEntry> KanjiEntryPtr;

//--------------------------------------------------------------------------//

class ReadingModel : std::map<Glib::ustring, KanjiEntryPtr>
{
public:
    /**
     * Parses the two kanjidic files and creates the dictionary.
     */
    ReadingModel(const Glib::ustring& kanjiDicPathA,
            const Glib::ustring& kanjidicPathB);

private:
    /**
     * The set of all readings used for all kanji.
     */
    static std::set<ustringPtr> allReadings;
};

//--------------------------------------------------------------------------//
