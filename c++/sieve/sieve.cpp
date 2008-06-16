// 
//  seive.cpp
//  sieve
//  
//  Created by Lars Yencken on 2008-06-16.
//  Copyright 2008-06-16 Lars Yencken. All rights reserved.
// 

#include <iostream>
#include <vector>
#include <math.h>

using namespace std;

bool is_prime(unsigned int n);

int main(int argc, char const *argv[])
{
    unsigned int n;
    while (cin.good())
    {
        cin >> n;
        if (!cin.fail())
        {
            cout << 
                (is_prime(n) ? "true" : "false") << endl;
        }
    }
    return 0;
}

bool is_prime(unsigned int n)
{
    if (n <= 3)
        return true;

    vector<bool> primes;
    primes.resize(n + 1);
    for (int i = 0; i < primes.size(); ++i)
    {
        primes[i] = true;
    }
    int j;
    for (int i = 2; i <= sqrt(n); i++)
    {
        for (j = i; j < n; j += i)
        {
            primes[j] = false;
        }
        if (j == n)
        {
            return false;
        }
    }
    return true;
}