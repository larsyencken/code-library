/*
 *
 *  fork.cpp
 *  code
 *
 *  Created by Lars Yencken on 2010-12-16.
 *  Copyright 2010 NICTA. All rights reserved.
 *
 */

#include <stdio.h>
#include <stdlib.h>

/**
 * From:
 * http://en.wikibooks.org/wiki/Distributed_Systems#Memory_model
 */
int
main()
{
    int i, cpid;
    cpid = fork();
    if (cpid < 0) exit(1);
    else if (cpid == 0) {
        /* this is the child process. */
        for (i=0; i<1000; i++) printf("child: %d\n", i);
        exit(0);
    }
    else {
        /* this is parent process */
        for (i=0; i<1000; i++) printf("parent: %d\n", i);
        waitpid(cpid);
    }
    return 0;
}
