// point.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Fri Oct  7 10:25:26 EST 2005

/*****************************************************************************/

#include "point.h"

/*****************************************************************************/

Point::Point()
    :
    x(0),
    y(0)
{
}

/*****************************************************************************/

Point::Point(int xVal, int yVal)
    :
    x(xVal),
    y(yVal)
{
}

/*****************************************************************************/

Point Point::operator+(Point p)
{
    Point newPoint;

    newPoint.x = x + p.x;
    newPoint.y = y + p.y;

    return newPoint;
}

/*****************************************************************************/

Point Point::operator-(Point p)
{
    Point newPoint;

    newPoint.x = x - p.x;
    newPoint.y = y - p.y;

    return newPoint;
}

/*****************************************************************************/

bool Point::isZero() const
{
    return (x == 0 && y == 0);
}

/*****************************************************************************/

ostream& operator<<(ostream& stream, const Point& point)
{
    return stream << "[" << point.x << "," << point.y << "]";
}

/*****************************************************************************/

