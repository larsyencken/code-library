//--------------------------------------------------------------------------//
// example.c
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 expandtab:
// Thu Nov 13 10:52:21 EST 2008
//
// From: http://en.wikipedia.org/wiki/POSIX_Threads
//--------------------------------------------------------------------------//

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <pthread.h>
 
static void wait_thread(void)
{
    time_t start_time = time(NULL);
 
    while (time(NULL) == start_time)
    {
        // do nothing except chew CPU slices for up to one second.
    }
}
 
static void *thread_func(void *vptr_args)
{
    int i;
 
    for (i = 0; i < 20; i++)
    {
        fputs("  b\n", stderr);
        wait_thread();
    }
 
    return NULL;
}
 
int main(void)
{
    int i;
    pthread_t thread;
 
    if (pthread_create(&thread, NULL, thread_func, NULL) != 0)
    {
        return EXIT_FAILURE;
    }
 
    for (i = 0; i < 20; i++)
    {
        fputs("a\n", stdout);
        wait_thread();
    }
 
    if (pthread_join(thread, NULL) != 0)
    {
        return EXIT_FAILURE;
    }
 
    return EXIT_SUCCESS;
}
