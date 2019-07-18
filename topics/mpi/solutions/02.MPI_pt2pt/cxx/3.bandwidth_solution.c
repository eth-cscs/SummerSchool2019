/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS, take no responsibility for the use of the enclosed     *
 * teaching material.                                           *
 *                                                              *
 * Purpose: Measuring bandwidth using a ping-pong               *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/

/*
 * NOTE: make a reservation with two nodes:
 * salloc ... -N 2 -n 2 ....
 * start mpi using 2 nodes with one process per node:
 * srun -N 2 -n 2 .......
 * use gnuplot to plot the result:
 * gnuplot bandwidth.gp
 *
 * Advanced: try on only one node, explain the bandwidth values
 * srun -N 1 -n 2 .......
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <mpi.h>

#define PING  0 //message tag
#define PONG  1 //message tag

#define NMESSAGES 100
#define INI_SIZE 1
#define FACT_SIZE 2
#define REFINE_SIZE_MIN (1*1024)
#define REFINE_SIZE_MAX (16*1024)
#define SUM_SIZE (1*1024)
#define MAX_SIZE (1<<29) /* 512 MBytes */

int main(int argc, char *argv[])
{
    int my_rank, k;
    int length_of_message;
    double start, stop, time, transfer_time;
    MPI_Status status;
    char* buffer;
    char output_str[512];
    FILE* f;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    buffer = (char*)malloc(MAX_SIZE*sizeof(char));

    if (my_rank == 0) {
        f = fopen("bandwidth.dat","w");
    }

    length_of_message = INI_SIZE;

    while(length_of_message <= MAX_SIZE) {
        /* Write a loop of NMESSAGES iterations which do a ping pong.
         * Make the size of the message variable and display the bandwidth for each of them.
         * What do you observe? (plot it)
         */
        start = MPI_Wtime();

        for( k = 0; k < NMESSAGES; k++) {
            if (my_rank == 0) {
                MPI_Send(buffer, length_of_message, MPI_BYTE, 1, PING, MPI_COMM_WORLD);
                MPI_Recv(buffer, length_of_message, MPI_BYTE, 1, PONG, MPI_COMM_WORLD, &status);
            } else if (my_rank == 1) {
                MPI_Recv(buffer, length_of_message, MPI_BYTE, 0, PING, MPI_COMM_WORLD, &status);
                MPI_Send(buffer, length_of_message, MPI_BYTE, 0, PONG, MPI_COMM_WORLD);
            }
        }

        stop = MPI_Wtime();
        if (my_rank == 0) {
            time = stop - start;

            transfer_time = time / (2 * NMESSAGES);

            sprintf(output_str, "%i %f %f\n",
                    length_of_message,
                    transfer_time,
                    (length_of_message / transfer_time)/(1024*1024));

            fwrite(output_str, sizeof(char), strlen(output_str), f);
            printf("%s", output_str);

        }
        if (length_of_message >= REFINE_SIZE_MIN && length_of_message < REFINE_SIZE_MAX) {
            length_of_message += SUM_SIZE;
        } else {
            length_of_message *= FACT_SIZE;
        }

    }

    if (my_rank == 0) {
        fclose(f);
    }
    MPI_Finalize();
    free(buffer);
    return 0;
}
