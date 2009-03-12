// move.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Fri Oct  7 10:51:09 EST 2005

#include "move.h"

/*****************************************************************************/

Move::Move()
{
}

/*****************************************************************************/

Move::Move(Point fromPos, Point toPos)
    :
    from(fromPos),
    to(toPos),
    jumpMove(false)
{
}

/*****************************************************************************/

Move::Move(Point fromPos, Point toPos, Point taking)
    :
    from(fromPos),
    to(toPos),
    pieceTaken(taking),
    jumpMove(true)
{
}

/*****************************************************************************/

// Added an output operator for moves.
ostream& operator<<(ostream& stream, const Move& move)
{
    return stream << "m(f:" << move.from << ", t:" << move.to <<
            ", x:" << move.pieceTaken << ")";
}

/*****************************************************************************/
