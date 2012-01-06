/*
 *
 *  from_timestamp.cpp
 *
 *  Created by Lars Yencken on 2012-01-06.
 *  Copyright 2012 Lars Yencken. All rights reserved.
 *
 */

#include <iostream>
#include <sstream>
#include <ctime>

using namespace std;

void usage()
{
	cerr << "Usage: from_timestamp <timestamp>" << endl;
	cerr << endl;
	cerr << "Converts a timestamp to a ctime representation." << endl;
}

int main(int argc, const char *argv[])
{
	if (argc > 1)
	{
		usage();
		return 1;
	}

	float f;
	string s;
	while (cin >> s)
	{
		stringstream ss(s);
		if (ss >> f)
		{
			time_t t = (time_t)f;
			cout << ctime(&t);
		}
		else
		{
			cerr << "not a timestamp: " << s << endl;
		}
	}
	return 0;
}
