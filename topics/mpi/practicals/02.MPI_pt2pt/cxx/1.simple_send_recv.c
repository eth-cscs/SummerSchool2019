/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS take no responsibility for the use of the enclosed      *
 * teaching material.                                           *
 *                                                              *
 * Purpose: Simple point-to-point communication                 *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/* program that sends a number from one process to another */

int main(int argc, char *argv[])
{

    int rank, size, number;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size != 2) {
        printf("please run this with 2 processors\n");
        MPI_Finalize();
        exit(1);
    }
    if (rank == 0) {
        printf("enter a number:\n");
        fflush(stdout);
        scanf("%d",&number);
    }
    /* send the contents of number from rank 0 to rank 1 using MPI_Send --- MPI_Recv */

    if (rank == 1)
        printf("The communicated number is %i\n", number);
    MPI_Finalize();
    return 0;
}
