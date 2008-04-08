//--------------------------------------------------------------------------//
// bitset.hpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Wed Sep  5 19:52:42 EST 2007
//--------------------------------------------------------------------------//

const int BITSPERWORD = 32;

class bitset
{
public:
    /**
     * Build a new bitset.
     */
    bitset(int maxVal);

    /**
     * Add a new number to the set.
     */
    void add(int i);

    /**
     * Check whether the number is already a member of the set.
     */
    bool test(int i) const;

    /**
     * Removes the given number from the set.
     */
    void remove(int i);

private:
    int _maxVal, _shift, _mask;
    int* _data;
};
