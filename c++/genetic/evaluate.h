// evaluate.h
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Mon Oct 10 00:09:30 EST 2005

/*****************************************************************************/

#include "board.h"

/*****************************************************************************/

class Evaluator
{
public:
    vector<float> m_weights;
    int m_score;
    // *** CONSTRUCTORS *** //

    // Default constructor; zeroes all weights.
    Evaluator();

    // Copy constructor.
    Evaluator(const Evaluator& rhs);

    // *** ACCESSORS *** //

    // Evaluate a board
    float evaluate(const Board& b, Player p) const;

    // Select a move given a board and a player.
    Move selectMove(const Board& b, Player p) const;

    // *** MANIPULATORS *** //

    // Change a weight randomly.
    void mutate();

    // Uniform crossover operator.
    Evaluator operator+(const Evaluator& rhs);

    // Define an ordering of evaluators based on score.
    int operator<(const Evaluator& rhs) const;
    int operator==(const Evaluator& rhs) const;

    // Define a copy operator
    Evaluator& operator=(const Evaluator& rhs); 

private:
    // Scales the weights to sum to 1.
    void _normalise();
};

/*****************************************************************************/

// Compare two evaluators by playing them off against each other.
int scoredComparison(const Evaluator& evaluatorA,
                const Evaluator& evaluatorB);

/*****************************************************************************/

// Compare two evaluators by playing them off against each other.
int playGame(const Evaluator& evaluatorA, const Evaluator& evaluatorB);

/*****************************************************************************/

ostream& operator<<(ostream& stream, const Evaluator& evaluator);

/*****************************************************************************/

