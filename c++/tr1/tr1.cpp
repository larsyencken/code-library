// 
//  tr1.cpp
//  tr1
//  
//  Created by Lars Yencken on 2008-06-20.
//  Copyright 2008-06-20 Lars Yencken. All rights reserved.
// 

#include <iostream>
#include <tr1/unordered_map>
#include <boost/functional/hash.hpp>

typedef std::tr1::unordered_map<long, long, boost::hash<long> > Table;

int main(int argc, char const *argv[])
{
    Table t;
    
    long val;
    int n = 0;
    while (std::cin.good())
    {
        std::cin >> val;
        if (std::cin.fail())
            break;
        n++;
        if (t[val] > 0) {
            std::cout << "Collision: " << t[val] << " " << val << std::endl;
            return 0;
        }
        t[val] = val;
        std::cout << val << std::endl;
    }
    std::cout << "No collisions in " << n << " numbers" << std::endl;
    return 0;
}