// board.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Thu Oct  6 22:08:04 EST 2005

#include "move.h"

#include <vector>

/*****************************************************************************/

using namespace std;

const int g_boardSize = 4;

/*****************************************************************************/

enum Player { Player__White, Player__Black, Player__None };

/*****************************************************************************/

// Returns the opposite player.
Player oppositePlayer(Player player);

/*****************************************************************************/

class Board
{
protected:
    vector< vector<Player> > m_board;

public:
    // *** CONSTRUCTORS *** //

    // Copy an existing board.
    Board(const Board& original);

    // Make a new board from scratch.
    Board();

    // *** ACCESSORS *** //

    // Returns a vector of available moves
    void availableMoves(Player player, vector<Move>& moves) const;

    // Accesses an element
    Player getPoint(Point point) const;

    // Determine if a player has won. Returns Player__Empty if nobody has
    // won.
    Player won() const;

    // Each of these methods iterate over a part of the board, adding +1 for a
    // matching player and -1 for a matching opponent, returning the
    // accumulated score.
    int sumRow(int index, Player player, Player opponent) const;
    int sumColumn(int index, Player player, Player opponent) const;
    int sumUpDiagonal(int index, Player player, Player opponent) const;
    int sumDownDiagonal(int index, Player player, Player opponent) const;

    // *** MANIPULATORS *** //

    // Updates this board with this move.
    void update(const Move& move);

    // Accesses an element
    void setPoint(Point point, Player value);

private:
    // Given a single position on the board, finds all possible moves for
    // that piece and adds them to the vector of moves.
    void _findMoves(Point point, Player player, vector<Move>& slideMoves,
            vector<Move>& jumpMoves) const;

    // Returns true if the point is a valid point on the board.
    bool _validPoint(Point point) const;
};

/*****************************************************************************/

ostream& operator<<(ostream& stream, const Board& point);

/*****************************************************************************/
