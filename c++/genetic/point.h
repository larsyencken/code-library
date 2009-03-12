// point.h
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Fri Oct  7 10:17:29 EST 2005

/*****************************************************************************/

#include <iostream>
#include <fstream>

using namespace std;

/*****************************************************************************/

class Point
{
public:
    int x;
    int y;

    // Constructor setting values to 0,0
    Point();

    // Constructor taking just the x and y values
    Point(int xVal, int yVal);

    // Useful arithmetic operators
    Point operator+(Point p);
    Point operator-(Point p);

    bool isZero() const;
};

/*****************************************************************************/

// Added an output operator for moves.
ostream& operator<<(ostream& stream, const Point& point);

/*****************************************************************************/

