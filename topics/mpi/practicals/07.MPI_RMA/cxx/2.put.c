/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS take no responsibility for the use of the enclosed      *
 * teaching material.                                           *
 *                                                              *
 * Purpose: MPI_Put               communication                 *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/* A rank put its rank value to its left neighbour */
/* use MPI_Win_create and MPI_Put with passive synchronization */

int main(int argc, char *argv[])
{

    int rank, left_rank, size, number;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    number = rank;
    left_rank = rank -1;
    if (left_rank < 0)
        left_rank = size -1;

    // create window

    //passive synchronization + put

    // synchronization?

    printf("My rank is %d, my number is: %d",rank, number);
    if (rank-number == 1 || number-rank==size-1) {
        printf(" OK!\n");
    } else {
        printf(" Not OK!\n");
    }

    // cleanup

    MPI_Finalize();
    return 0;
}
