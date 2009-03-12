/*****************************************************************************/
// genetic.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Mon Oct 10 11:43:47 EST 2005
/*****************************************************************************/

#include "genetic.h"
#include <iostream>
#include <assert.h>

using namespace std;

/*****************************************************************************/

const float g_probMutation = 0.3;
const int g_numElites = 2;
const float g_offspringCandidates = 0.3;

/*****************************************************************************/

Genetic::Genetic(string filename)
{
    m_pool.reserve(g_poolSize);

    // if the file exists
    // load in from the filename
    // else load from scratch

    // Create a new pool from scratch
    for (int i=0; i < g_poolSize; ++i)
    {
        m_pool.push_back(Evaluator());
    }

    return;
}

/*****************************************************************************/

void Genetic::train(int numIterations)
{
    cout << "Scoring initial generation\n";

    scoreGeneration();
    for (int i=0; i < numIterations; ++i)
    {
        cout << "Iteration " << i << '\n';
        nextGeneration();
        scoreGeneration();
    }

    cout << "Finished!!!\n";

    return;
}

/*****************************************************************************/

void Genetic::scoreGeneration()
{
    vector<int> backIndex(g_poolSize);

    cout << "--> Zeroing scores\n";
    // firstly zero all scores and create a normal index
    for (int i=0; i < g_poolSize; ++i)
    {
        m_pool[i].m_score = 0;
        backIndex.push_back(i);
    }

    // then shuffle the index
    cout << "--> Shuffling index\n";
    {
        int tmpIndex;
        int r;
        for (int i=0; i < g_poolSize; ++i)
        {
            r = rand() % g_poolSize;
    
            // swap the ith index with the rth index
            tmpIndex = backIndex[i];
            backIndex[i] = backIndex[r];
            backIndex[r] = tmpIndex;
        }
    }

    cout << "--> Playing off players\n";
    {
        const int halfNumPlayoffs = g_numComparisons;
    
        int opponent;
        int playerIndex, opponentIndex;
        int result;
    
        // now playoff each player with the successive ones
        for (int player=0; player < g_poolSize; ++player)
        {
            // start our opponent offset by 1 so that we don't play ourselves
            for (int offset=1; offset <= halfNumPlayoffs; offset++)
            {
                // work out our opponent offset (with wraparound)
                opponent = (player + offset) % g_poolSize;
    
                // change indexes through our table
                playerIndex = backIndex[player];
                opponentIndex = backIndex[opponent];
    
                result = scoredComparison(m_pool[playerIndex], 
                        m_pool[opponentIndex]);
    
                m_pool[player].m_score += result;
                m_pool[opponent].m_score -= result;

                /*
                cout << "Played " << player << "(" << m_pool[player].m_score
                        << ") with " << opponent << "(" << 
                        m_pool[opponent].m_score << "), result: " << result <<
                        endl;
                        */
            }
        }
    }
    
    cout << "--> Sorting players\n";
    {
        //sort(m_pool.begin(), m_pool.end());
        //cout << "--> Reversing sort players\n";
        //reverse(m_pool.begin(), m_pool.end());
    }

    return;
}

/*****************************************************************************/

void Genetic::nextGeneration()
{
    cout << "Seeding new generation\n";

    vector<Evaluator> newGeneration;
    newGeneration.reserve(g_poolSize);

    int nextFree = 0;

    cout << "--> Best elite had score " << m_pool[0].m_score << "\n";

    // firstly add the elites
    while (nextFree < g_numElites)
    {
        newGeneration[nextFree] = m_pool[nextFree];
        ++nextFree;
    }

    const int numWithOffspring = (int) (g_offspringCandidates * g_poolSize);
    assert(numWithOffspring > 0);

    // generate the rest of the pool as offspring and mutations
    while (nextFree < g_poolSize)
    {
        float randProbability = (double) rand() / (double)(RAND_MAX + 1);
        int randA, randB;

        if (randProbability < g_probMutation)
        {
            // mutation occurs!
            // (to one random parent)
            randA = rand() % numWithOffspring;
            newGeneration[nextFree] = m_pool[randA];
            newGeneration[nextFree].mutate();
        }
        else
        {
            // crossover occurs!
            // (between two random parents)
            randA = rand() % numWithOffspring;
            randB = rand() % numWithOffspring;
            newGeneration[nextFree] = m_pool[randA] + m_pool[randB];
        }

        ++nextFree;
    }

    // our new pool is the new generation
    m_pool = newGeneration;

    return;
}

/*****************************************************************************/

void Genetic::_dump(string filename) const
{
    // XXX fill me in
    return;
}

/*****************************************************************************/

void Genetic::_load(string filename)
{
    // XXX fill me in
    return;
}

/*****************************************************************************/

