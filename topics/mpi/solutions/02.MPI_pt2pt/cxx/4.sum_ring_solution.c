/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS take no responsibility for the use of the enclosed      *
 * teaching material.                                           *
 *                                                              *
 * Purpose: Parallel sum using a ping-pong                      *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/


#include <stdio.h>
#include <mpi.h>


int main (int argc, char *argv[])
{
    int my_rank, size;
    int snd_buf, rcv_buf;
    int right, left;
    int sum, i;

    MPI_Status  status;
    MPI_Request request;


    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    MPI_Comm_size(MPI_COMM_WORLD, &size);


    right = (my_rank+1)      % size;
    left  = (my_rank-1+size) % size;
    /* ... this SPMD-style neighbor computation with modulo has the same meaning as:
     * right = my_rank + 1;
     * if (right == size) right = 0;
     * left = my_rank - 1;
     * if (left == -1) left = size-1;
     */

    /* Implement ring addition code
     * do not use if (rank == 0) .. else ..
     * every rank sends initialy its rank number to a neighbor, and then sends what
     * it receives from that neighbor, this is done n times with n = number of processes
     * all ranks will obtain the sum.
     */
    sum = 0;
    snd_buf = my_rank;

    for( i = 0; i < size; i++) {
        MPI_Isend(&snd_buf, 1, MPI_INT, right, 0, MPI_COMM_WORLD, &request);
        MPI_Recv(&rcv_buf, 1, MPI_INT, left, 0, MPI_COMM_WORLD, &status);
        MPI_Wait(&request, &status);
        snd_buf = rcv_buf;
        sum += rcv_buf;
    }
    printf ("Process %i:\tSum = %i\n", my_rank, sum);

    MPI_Finalize();
    return 0;
}
