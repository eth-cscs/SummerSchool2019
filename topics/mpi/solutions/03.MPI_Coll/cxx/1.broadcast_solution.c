/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS take no responsibility for the use of the enclosed      *
 * teaching material.                                           *
 *                                                              *
 * Purpose: Simple broadcast                                    *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/


#include <stdio.h>
#include <mpi.h>

int main(int argc, char *argv[]) {
   int rank, data;
   MPI_Init(&argc, &argv);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);
   if (rank==0){
      printf("enter a value:\n");
      fflush(stdout);
      scanf ("%d",&data);
   }

   /* broadcast the value of data of rank 0 to all ranks */
   MPI_Bcast(&data, 1, MPI_INT, 0, MPI_COMM_WORLD);

   printf("I am rank %i and the value is %i\n", rank, data);
   MPI_Finalize();
   return 0;
}
