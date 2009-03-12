// evaluate.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Mon Oct 10 00:26:09 EST 2005

#include "evaluate.h"
#include <cstdlib>
#include <assert.h>

/*****************************************************************************/

const int g_numWeights = 6*g_boardSize - 2;

const int terribleScore = -10000;

/*****************************************************************************/

Evaluator::Evaluator()
{
    m_weights.resize(g_numWeights);

    // Initialize with random weights
    for (int i=0; i < g_numWeights; ++i)
    {
        m_weights[i] = (double) rand() / (double)(RAND_MAX + 1);
    }

    _normalise();

    // default to a terrible score
    m_score = terribleScore;

    return;
}

/*****************************************************************************/

Evaluator::Evaluator(const Evaluator& original)
{
    m_weights.resize(g_numWeights);
    back_insert_iterator< vector<float> > dest(m_weights);
    copy(original.m_weights.begin(), original.m_weights.end(), dest);
}

/*****************************************************************************/

float Evaluator::evaluate(const Board& board, Player player) const
{
    const Player opponent = oppositePlayer(player);

    float score = 0;

    // weighted sum of rows and columns
    for (int i=0; i < g_boardSize; ++i)
    {
        score += board.sumRow(i, player, opponent)*m_weights[i];
        score += board.sumColumn(i, player, opponent)*m_weights[g_boardSize+i];
    }

    const int numDiags = 2*g_boardSize - 1;
    const int weightOffset = 2*g_boardSize;

    for (int i=0; i < numDiags; ++i)
    {
        score += board.sumUpDiagonal(i, player, opponent)*
                    m_weights[weightOffset+i];
        score += board.sumDownDiagonal(i, player, opponent)*
                    m_weights[weightOffset+numDiags+i];
    }

    return score;
}

/*****************************************************************************/

Move Evaluator::selectMove(const Board& board, Player player) const
{
    vector<Move> moves;
    board.availableMoves(player, moves);

    Board currentBoard(board);
    float currentScore = 0;

    const int numMoves = moves.size();

    assert(numMoves > 0);
    
    Move bestMove = moves[0];
    currentBoard.update(bestMove);
    float bestScore = evaluate(currentBoard, player);

    for (int i=1; i < numMoves; ++i)
    {
        currentBoard = board;
        currentBoard.update(moves[i]);
        currentScore = evaluate(currentBoard, player);

        if (currentScore > bestScore)
        {
            bestScore = currentScore;
            bestMove = moves[i];
        }
    }

    return bestMove;
}

/*****************************************************************************/

void Evaluator::mutate()
{
    // At a random index, modify a weight randomly
    int randomIndex = rand() % g_numWeights;
    m_weights[randomIndex] = (double) rand() / (double)(RAND_MAX + 1);

    _normalise();
    return;
}

/*****************************************************************************/

Evaluator Evaluator::operator+(const Evaluator& rhs)
{
    bool coinToss;

    // Initialise from this evaluator...
    Evaluator newEvaluator(*this);

    // ...then use uniform crossover to selectively replace from the rhs.
    for (int i=0; i < g_numWeights; ++i)
    {
        coinToss = (bool) (rand() % 2);
        if (coinToss)
        {
            // take this weight from the rhs
            m_weights[i] = rhs.m_weights[i];
        }
    }

    return newEvaluator;
}

/*****************************************************************************/

void Evaluator::_normalise()
{
    // Determine the sum of all weights
    float totalSum = 0.0;
    for (int i=0; i < g_numWeights; ++i)
    {
        totalSum += m_weights[i];
    }

    assert(totalSum != 0);

    // Normalise by that factor.
    for (int i=0; i < g_numWeights; ++i)
    {
        m_weights[i] /= totalSum;
    }

    return;
}

/*****************************************************************************/

int Evaluator::operator<(const Evaluator& rhs) const
{
    return m_score < rhs.m_score;
}

/*****************************************************************************/

int Evaluator::operator==(const Evaluator& rhs) const
{
    return m_score == rhs.m_score;
}

/*****************************************************************************/

int scoredComparison(const Evaluator& evaluatorA, const Evaluator& evaluatorB)
{
    int score = 0;

    score += playGame(evaluatorA, evaluatorB);
    score += playGame(evaluatorB, evaluatorA);

    return score;
}

/*****************************************************************************/

int playGame(const Evaluator& evaluatorA, const Evaluator& evaluatorB)
{
    const int maxMoves = 30;

    Move nextMove;
    Board gameBoard;

    //cout << "%---- Starting game ----%\n";
    //cout << gameBoard;

    for (int i=0; i < maxMoves; ++i)
    {
        nextMove = evaluatorA.selectMove(gameBoard, Player__White);
        //cout << nextMove << " -- white\n";
        gameBoard.update(nextMove);
        //cout << gameBoard;
        if (gameBoard.won() == Player__White)
        {
            //cout << "Winner is white!\n";
            return 1;
        }

        nextMove = evaluatorB.selectMove(gameBoard, Player__Black);
        //cout << nextMove << " -- black\n";
        gameBoard.update(nextMove);
        //cout << gameBoard;
        if (gameBoard.won() == Player__Black)
        {
            //cout << "Winner is black!\n";
            return -1;
        }
    }

    return 0;
}

/*****************************************************************************/

void outputSubVector(ostream& stream, int start, int end, 
        const vector<float>& input)
{
    assert(start >= 0 && end <= input.size()); 

    assert((end - start) >= 1);

    stream << input[start];

    for (int i=start+1; i < end; ++i)
    {
        stream << " " << input[i];
    }

    return;
}

/*****************************************************************************/

ostream& operator<<(ostream& stream, const Evaluator& evaluator)
{
    const int numDiags = 2*g_boardSize - 1;
    const int weightOffset = 2*g_boardSize;

    stream << "Evaluator(" << endl;
    stream << "    Rows: ";
    outputSubVector(stream, 0, g_boardSize, evaluator.m_weights);
    stream << "\n    Columns: ";
    outputSubVector(stream, g_boardSize, weightOffset, evaluator.m_weights);
    stream << "\n    UpDiag: ";
    outputSubVector(stream, weightOffset, weightOffset+numDiags, 
            evaluator.m_weights);
    stream << "\n    DownDiag: ";
    outputSubVector(stream, weightOffset+numDiags, g_numWeights,
            evaluator.m_weights);

    stream << "\n)" << endl;

    return stream;
}

/*****************************************************************************/

Evaluator& Evaluator::operator=(const Evaluator& rhs)
{
    assert(rhs.m_weights.size() == g_numWeights);
    m_score = rhs.m_score;
    m_weights = rhs.m_weights;

    return *this;
}

/*****************************************************************************/
