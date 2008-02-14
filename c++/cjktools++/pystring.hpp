/*
 * Python style string utilities.
 */

#include <vector>
#include <glibmm.h>

namespace cjktools
{

/*
 * split(s, results, delim)
 *  Splits the given string wherever one of the given delimiters is reached.
 *  Returns a vector of substrings.
 */
void split(const Glib::ustring& s, std::vector<std::string>& results,
        const Glib::ustring& delim);

/*
 * split(s, results)
 *  Splits the given string wherever whitespace is found.
 */
void split(const Glib::ustring& s, std::vector<std::string>& results);

}
