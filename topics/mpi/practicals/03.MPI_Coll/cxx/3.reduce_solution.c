/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS take no responsibility for the use of the enclosed      *
 * teaching material.                                           *
 *                                                              *
 * Purpose: Simple reduce                                       *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/


#include <stdio.h>
#include <mpi.h>

int main(int argc, char *argv[]) {
   int rank, input, result;
   MPI_Init(&argc, &argv);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);
   input=rank+1;

   /* reduce the values of the different ranks in input to result of rank 0 
    * with the operation sum (max, logical and)
    */
   MPI_Reduce(&input, &result, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

   if (rank==0)
      printf("Rank 0 says: result is %i\n", result);
   MPI_Finalize();
   return 0;
}
