//--------------------------------------------------------------------------//
// listLong.hpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 et tw=78:
// Wed Feb  7 12:04:46 2007
//
//--------------------------------------------------------------------------//

#ifndef LISTLONG_HPP
#define LISTLONG_HPP

#include <list>

//--------------------------------------------------------------------------//

/**
 * A list of image ids, for example the results of a query. 
 */
typedef std::list < long int > ListLong;
typedef ListLong::iterator ListLongIterator;

/**
 * A list of lists of image ids, for example the results of clustering.
 */
typedef std::list < ListLong > ListListLong;
typedef ListListLong::iterator ListListLongIterator;

//--------------------------------------------------------------------------//

/**
 * Export definitions for ListLong and ListListLong for Boost Python.
 */
void export_ListLong();

//--------------------------------------------------------------------------//

#endif


