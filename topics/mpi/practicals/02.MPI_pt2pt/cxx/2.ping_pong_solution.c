/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS take no responsibility for the use of the enclosed      *
 * teaching material.                                           *
 *                                                              *
 * Purpose: A ping-pong                                         *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/

#include <stdio.h>
#include <mpi.h>

#define PING  0 //message tag
#define PONG  1 //message tag
#define SIZE  1024

int main(int argc, char *argv[])
{
    int my_rank;
    float buffer[SIZE];
    MPI_Status status;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    /* Process 0 sends a message (ping) to process 1.
     * After receiving the message, process 1 sends a message (pong) to process 0.
     */
    if (my_rank == 0) {
        MPI_Send(buffer, SIZE, MPI_FLOAT, 1, PING, MPI_COMM_WORLD);
        MPI_Recv(buffer, SIZE, MPI_FLOAT, 1, PONG, MPI_COMM_WORLD, &status);
    }
    if (my_rank == 1) {
        MPI_Recv(buffer, SIZE, MPI_FLOAT, 0, PING, MPI_COMM_WORLD, &status);
        MPI_Send(buffer, SIZE, MPI_FLOAT, 0, PONG, MPI_COMM_WORLD);
    }
    printf("Rank %d says: Ping-pong is completed.\n",my_rank);

    MPI_Finalize();
    return 0;
}
