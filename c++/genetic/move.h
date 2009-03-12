// move.h
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Fri Oct  7 10:48:14 EST 2005

#include "point.h"

/*****************************************************************************/

class Move
{
public:
    Point from;
    Point to;
    bool jumpMove;
    Point pieceTaken;

    // Default do-nothing constructor
    Move();

    // Initialise a new slide move
    Move(Point fromPos, Point toPos);

    // Initialise a new jump move
    Move(Point fromPos, Point toPos, Point takenPos);

};

/*****************************************************************************/

// Added an output operator for moves.
ostream& operator<<(ostream& stream, const Move& move);

/*****************************************************************************/
