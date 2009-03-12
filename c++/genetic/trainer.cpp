/*****************************************************************************/
// trainer.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 expandtab:
// Mon Oct 10 15:00:47 EST 2005
/*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#include "genetic.h"

// main():
//

int
main(int argc, char *argv[]) 
{
    Genetic trainer(string("woof"));
    cout << "Initialised, beginning training\n";
    trainer.train(100);
    return 0;
}

