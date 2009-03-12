// board.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Thu Oct  6 22:08:04 EST 2005

#include "board.h"
#include <assert.h>

/*****************************************************************************/

Board::Board(const Board& original)
{
    // copy all squares from the original board
    m_board = vector< vector<Player> >(g_boardSize, 
                    vector<Player>(g_boardSize));
    for (int y=0; y < g_boardSize; ++y)
    {
        for (int x=0; x < g_boardSize; ++x)
        {
            m_board[y][x] = original.m_board[y][x];
        }
    }

    return;
};

/*****************************************************************************/

Board::Board()
{
    // initialise alternating squares in white and black
    m_board = vector< vector<Player> >(g_boardSize, 
                    vector<Player>(g_boardSize));
    for (int y=0; y < g_boardSize; ++y)
    {
        for (int x=0; x < g_boardSize; ++x)
        {
            if ((y+x) % 2)
            {
                m_board[y][x] = Player__Black;
            }
            else
            {
                m_board[y][x] = Player__White;
            }
        }
    }

    // need an empty space so that the game can start
    m_board[0][0] = Player__None;

    return;
}

/*****************************************************************************/

void Board::availableMoves(Player player, vector<Move>& moves) const
{
    moves.clear();

    vector<Move> slideMoves;
    vector<Move> jumpMoves;

    for (int y=0; y < g_boardSize; ++y)
    {
        for (int x=0; x < g_boardSize; ++x)
        {
            if (m_board[y][x] == player)
            {
                _findMoves(Point(x, y), player, slideMoves, jumpMoves);
            }
        }
    }

    // append rather than overwrite
    back_insert_iterator< vector<Move> > dest = back_inserter(moves);

    if (jumpMoves.empty())
    {
        // copy all slide moves
        copy(slideMoves.begin(), slideMoves.end(), dest);
    }
    else
    {
        // copy all jump moves
        copy(jumpMoves.begin(), jumpMoves.end(), dest);
    }

    return;
}

/*****************************************************************************/

void Board::_findMoves(Point from, Player player, vector<Move>& slideMoves,
        vector<Move>& jumpMoves) const
{
    Player opponent = oppositePlayer(player);
    Player pointPlayer;

    // find slide moves
    for (int yDiff=-1; yDiff <= 1; ++yDiff)
    {
        for (int xDiff=-1; xDiff <= 1; ++xDiff)
        {
            Point diff(xDiff, yDiff);
            Point to = from + diff;

            // no point looking in the same spot
            if ((! _validPoint(to)) || diff.isZero())
                continue;

            pointPlayer = getPoint(to);

            if (pointPlayer == opponent)
            {
                // we can take them...
                Point taken = to;
                to = to + diff;
                if (! _validPoint(to))
                {
                    continue;
                }

                pointPlayer = getPoint(to);

                if (pointPlayer == Player__None)
                {
                    // ... only if there's an empty space on the other side
                    jumpMoves.push_back(Move(from, to, taken));
                }
            }

            if (pointPlayer == Player__None)
            {
                slideMoves.push_back(Move(from, to));
            }
        }
    }

    return;
}

/*****************************************************************************/

bool Board::_validPoint(Point p) const
{
    return (p.x >= 0 && p.x < g_boardSize && p.y >= 0 && p.y < g_boardSize);
}

/*****************************************************************************/

Player Board::getPoint(Point p) const
{
    return m_board[p.y][p.x];
}

/*****************************************************************************/

void Board::setPoint(Point p, Player value)
{
    m_board[p.y][p.x] = value;
}

/*****************************************************************************/

Player Board::won() const
{
    bool haveWhite = false;
    bool haveBlack = false;

    // loop through the board trying to find one of each case
    for (int y=0; y < g_boardSize; ++y)
    {
        for (int x=0; x < g_boardSize; ++x)
        {
            switch (m_board[y][x])
            {
                case Player__White:
                    haveWhite = true;
                    break;

                case Player__Black:
                    haveBlack = true;
                    break;

                default:
                    break;
            }
        }
    }

    if (!haveWhite)
    {
        return Player__Black;
    }
    else if (!haveBlack)
    {
        return Player__White;
    }

    return Player__None;
}

/*****************************************************************************/

Player oppositePlayer(Player player)
{
    switch (player)
    {
        case Player__White:
            return Player__Black;
    
        case Player__Black:
            return Player__White;
    
        default:
            // the empty board space has no real opponent
            return Player__None;
    }
}

/*****************************************************************************/

int Board::sumRow(int y, Player player, Player opponent) const
{
    assert(y >= 0 && y < g_boardSize);
    Player boardPlayer;

    int score = 0;

    for (int x=0; x < g_boardSize; ++x)
    {
        boardPlayer = m_board[y][x];

        if (boardPlayer == player)
        {
            ++score;
        }
        else if (boardPlayer == opponent)
        {
            --score;
        }
    }

    return score;
}

/*****************************************************************************/

int Board::sumColumn(int x, Player player, Player opponent) const
{
    assert(x >= 0 && x < g_boardSize);
    Player boardPlayer;

    int score = 0;

    for (int y=0; y < g_boardSize; ++y)
    {
        boardPlayer = m_board[y][x];

        if (boardPlayer == player)
        {
            ++score;
        }
        else if (boardPlayer == opponent)
        {
            --score;
        }
    }

    return score;
}

/*****************************************************************************/

int Board::sumUpDiagonal(int index, Player player, Player opponent) const
{
    assert(index >= 0 && index < 2*g_boardSize-1);
    Player boardPlayer;

    int score = 0;

    for (int y=0; y < g_boardSize; ++y)
    {
        for (int x=0; x < g_boardSize; ++x)
        {
            if (x+y == index)
            {
                // we're on the diagonal, so add up the score
                boardPlayer = m_board[y][x];
        
                if (boardPlayer == player)
                {
                    ++score;
                }
                else if (boardPlayer == opponent)
                {
                    --score;
                }
            }
        }
    }

    return score;
}

/*****************************************************************************/

int Board::sumDownDiagonal(int index, Player player, Player opponent) const
{
    assert(index >= 0 && index < 2*g_boardSize-1);
    Player boardPlayer;

    // reduce the index to center on zero
    index -= g_boardSize-1;

    int score = 0;

    for (int y=0; y < g_boardSize; ++y)
    {
        for (int x=0; x < g_boardSize; ++x)
        {
            if (x-y == index)
            {
                // we're on the diagonal, so add up the score
                boardPlayer = m_board[y][x]; 
        
                if (boardPlayer == player)
                {
                    ++score;
                }
                else if (boardPlayer == opponent)
                {
                    --score;
                }
            }
        }
    }

    return score;
}

/*****************************************************************************/

void Board::update(const Move& move)
{
    Player player = getPoint(move.from);

    assert(player != Player__None);

    // remove the piece
    setPoint(move.from, Player__None);

    // and add it to the destination
    setPoint(move.to, player);

    if (move.jumpMove)
    {
        // maybe removing a taken piece
        setPoint(move.pieceTaken, Player__None);
    }

    return;
}

/*****************************************************************************/

ostream& operator<<(ostream& stream, const Board& board)
{
    stream << "+";
    for (int i=0; i < g_boardSize; ++i)
    {
        stream << "---+";
    }
    stream << endl;

    for (int y=g_boardSize-1; y >= 0; --y)
    {
        stream << "|";
        for (int x=0; x < g_boardSize; ++x)
        {
            Player p = board.getPoint(Point(x, y));
            if (p == Player__White)
            {
                stream << " W ";
            }
            else if (p == Player__Black)
            {
                stream << " B ";
            }
            else
            {
                stream << "   ";
            }
            stream << "|";
        }
        stream << endl;

	    stream << "+";
	    for (int x=0; x < g_boardSize; ++x)
	    {
	        stream << "---+";
	    }
	    stream << endl;
    }

    return stream;
}
