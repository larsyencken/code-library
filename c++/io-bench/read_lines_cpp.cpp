/*
 *
 *  read_lines_cpp.cpp
 *  code
 *
 *  Created by Lars Yencken on 2010-12-17.
 *  Copyright 2010 Lars Yencken. All rights reserved.
 *
 */

#include <iostream>
#include <string>

using namespace std;

int
main(int argc, const char *argv[])
{
    ios_base::sync_with_stdio(false);

    string l;
    while (getline(cin, l)) {
        cout << l << endl;
    }
    return 0;
}
