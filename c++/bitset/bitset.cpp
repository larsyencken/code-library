//--------------------------------------------------------------------------//
// bitset.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Wed Sep  5 19:52:33 EST 2007
//--------------------------------------------------------------------------//

#include "bitset.hpp"

//--------------------------------------------------------------------------//

bitset::bitset(int maxVal)
    :
_maxVal(maxVal)
{
    _data = new int[1 + maxVal/BITSPERWORD];
    for (int i = 0; i < maxVal; i++) {
        remove(i);
    }

    _shift = 0;
    for (int i = BITSPERWORD; i != 1; i >>= 1) {
        _shift++;
    }

    {
        _mask = 0;
        for (int i = 0; i < _shift; i++) {
            _mask |= (1 << i);
        }
    }

    return;
}

void bitset::add(int val)
{
    _data[val >> _shift] |= (1<<(val & _mask));
    return;
}

void bitset::remove(int val)
{
    _data[val >> _shift] &= ~(1<<(val & _mask));
    return;
}

bool bitset::test(int val) const
{
    return bool(_data[val >> _shift] & (1<<(val & _mask)));
}
