/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS take no responsibility for the use of the enclosed      *
 * teaching material.                                           *
 *                                                              *
 * Purpose: Creating a 1-dimensional ring topology              *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/


#include <stdio.h>
#include <mpi.h>

#define max_dims 1


int main (int argc, char *argv[])
{
    int my_rank, size;
    int snd_buf, rcv_buf;
    int right, left;
    int sum, i;

    MPI_Comm    new_comm;
    int  dims[max_dims], periods[max_dims], reorder;

    MPI_Status  status;
    MPI_Request request;

    MPI_Init(&argc, &argv);

    /* Get process info. */
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    /* Set cartesian topology. */


    /* Get nearest neighbour rank. */


    /* Compute global sum. */
    sum = 0;
    snd_buf = my_rank;

    for( i = 0; i < size; i++) {
        MPI_Isend(&snd_buf, 1, MPI_INT, right, 0, new_comm, &request);

        MPI_Recv(&rcv_buf, 1, MPI_INT, left, 0, new_comm, &status);

        MPI_Wait(&request, &status);

        snd_buf = rcv_buf;
        sum += rcv_buf;
    }

    printf ("Rank %i:\tSum = %i\n", my_rank, sum);

    MPI_Finalize();
}
