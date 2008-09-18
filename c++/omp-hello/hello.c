//--------------------------------------------------------------------------//
// hello.c
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 expandtab:
// Fri Sep 19 08:20:45 EST 2008
//--------------------------------------------------------------------------//

#include <omp.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    int thread_id, n_threads;
    #pragma omp parallel private(thread_id) num_threads(8)
    {
        thread_id = omp_get_thread_num();
        printf("Hello world from thread %d\n", thread_id);
        #pragma omp barrier
        if (thread_id == 0) {
            n_threads = omp_get_num_threads();
            printf("There are %d threads\n", n_threads);
        }
    }
    return 0;
}

