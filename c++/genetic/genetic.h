/*****************************************************************************/
// genetic.h
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Mon Oct 10 11:29:18 EST 2005
/*****************************************************************************/

#include "evaluate.h"

#include <string>

/*****************************************************************************/

const int g_poolSize = 100;

const int g_numComparisons = 10;

/*****************************************************************************/

/*
 * This class handles evaluating and evolving generations of board evaluators.
 */
class Genetic
{
protected:
    vector<Evaluator> m_pool;

public:
    // *** CONSTRUCTORS *** //

    // Makes a new instance, loading from the given filename if available.
    Genetic(string filename);

    // *** ACCESSORS *** //

    // *** MANIPULATORS *** //

    // Trains the pool for the given number of iterations.
    void train(int numIterations);

    // Moves from the current generation to the next, by:
    // - keeping the best two entries as is
    // - removing the bottom end of the population
    // - crossbreeding the remaining entries
    void nextGeneration();

    // Scores the current generation.
    void scoreGeneration();

private:
    // Dumps the current generation to a filename.
    void _dump(string filename) const;

    // Loads a pool from the given filename.
    void _load(string filename);
};

/*****************************************************************************/

