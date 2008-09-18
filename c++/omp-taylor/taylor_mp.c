/*
 * taylor.c
 *
 * This program calculates the value of e*pi by first calculating e
 * and pi by their taylor expansions and then multiplying them
 * together.
 */

#include <stdio.h>
#include <time.h>

#define num_steps 20000000

int main(int argc, char *argv[])
{
  double start, stop; /* times of beginning and end of procedure */
  double e, pi, factorial, product;

  /* start the timer */
  start = clock();

  /* Now there is no first and seccond, we calculate e and pi */
#pragma omp parallel sections shared(e, pi)
  {
#pragma omp section
    {
      int i;
      printf("e started\n");
      e = 1;
      factorial = 1; /* rather than recalculating the factorial from
                        scratch each iteration we keep it in this varialbe
                        and multiply it by i each iteration. */
      for (i = 1; i<num_steps; i++) {
        factorial *= i;
        e += 1.0/factorial;
      }
      printf("e done: %f\n", e);
    } /* e section */

#pragma omp section 
    {
      int i;
      /* In this thread we calculate pi expansion */
      printf("pi started\n");

      pi = 0;
      for (i = 0; i < num_steps*10; i++) {
        /* we want 1/1 - 1/3 + 1/5 - 1/7 etc.
           therefore we count by fours (0, 4, 8, 12...) and take
             1/(0+1) =  1/1
           - 1/(0+3) = -1/3
             1/(4+1) =  1/5
           - 1/(4+3) = -1/7 and so on */
        pi += 1.0/(i*4.0 + 1.0);
        pi -= 1.0/(i*4.0 + 3.0);
      }
      pi = pi * 4.0;
      printf("pi done: %f\n", pi);
    } /* pi section */
    
  } /* omp sections */
  /* at this point the threads should rejoin */

  product = e * pi;

  stop = clock();

  printf("Reached result %f in %.3f seconds\n", product, (stop-start)/1000000);

  return 0;
}
