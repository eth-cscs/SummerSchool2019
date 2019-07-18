/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS take no responsibility for the use of the enclosed      *
 * teaching material.                                           *
 *                                                              *
 * Purpose: Exchange ghost cell in 2 directions                 *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/

/* Use only 16 processes for this exercise
 * Send the ghost cell in two directions: top to bottom and bottom to top
 * consider cyclic boundaries, for instance, rank 0 and rank 12 are connected together
 *
 * process decomposition on 4*4 grid
 *
 *  |-----------|
 *  | 0| 1| 2| 3|
 *  |-----------|
 *  | 4| 5| 6| 7|
 *  |-----------|
 *  | 8| 9|10|11|
 *  |-----------|
 *  |12|13|14|15|
 *  |-----------|
 *
 * Each process works on a 10*10 (SUBDOMAIN) block of data
 * the D corresponds to data, g corresponds to "ghost cells"
 * xggggggggggx
 * gDDDDDDDDDDg
 * gDDDDDDDDDDg
 * gDDDDDDDDDDg
 * gDDDDDDDDDDg
 * gDDDDDDDDDDg
 * gDDDDDDDDDDg
 * gDDDDDDDDDDg
 * gDDDDDDDDDDg
 * gDDDDDDDDDDg
 * gDDDDDDDDDDg
 * xggggggggggx
 */

/* Tasks:
 * A. each rank has to find its top and bottom neighbor
 * B. send them the data they need
 *    - top array goes to top neighbor
 *    - bottom array goes to bottom neighbor
 */

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define SUBDOMAIN 10
#define DOMAINSIZE (SUBDOMAIN+2)

int main(int argc, char *argv[])
{
    int rank, size, i, j, rank_bottom, rank_top;
    double data[DOMAINSIZE*DOMAINSIZE];
    MPI_Request request;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size!=16) {
        printf("please run this with 16 processors\n");
        MPI_Finalize();
        exit(1);
    }

    for (i=0; i<DOMAINSIZE*DOMAINSIZE; i++) {
        data[i]=rank;
    }

    rank_bottom=(rank+4)%16;
    rank_top=(rank+16-4)%16;


    //  ghost cell exchange with the neighbouring cells (cyclic) to the bottom and to the top using:
    //  a) MPI_Send, MPI_Irecv, MPI_Wait
    //  b) MPI_Isend, MPI_Recv, MPI_Wait
    //  c) MPI_Sendrecv

    //  to the top

    // a)
    MPI_Irecv(&data[2-1+(DOMAINSIZE-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_bottom, 0, MPI_COMM_WORLD, &request);
    MPI_Send(&data[2-1+(2-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_top, 0, MPI_COMM_WORLD);
    MPI_Wait(&request, &status);

    // b)
    MPI_Isend(&data[2-1+(2-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_top, 0, MPI_COMM_WORLD, &request);
    MPI_Recv(&data[2-1+(DOMAINSIZE-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_bottom, 0, MPI_COMM_WORLD, &status);
    MPI_Wait(&request, &status);

    // c)
    MPI_Sendrecv(&data[2-1+(2-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_top, 0, &data[2-1+(DOMAINSIZE-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_bottom, 0, MPI_COMM_WORLD, &status);

    //  to the bottom
    // a)
    MPI_Irecv(&data[2-1+(1-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_top, 0, MPI_COMM_WORLD, &request);
    MPI_Send(&data[2-1+(DOMAINSIZE-1-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_bottom, 0, MPI_COMM_WORLD);
    MPI_Wait(&request, &status);

    // b)
    MPI_Isend(&data[2-1+(DOMAINSIZE-1-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_bottom, 0, MPI_COMM_WORLD, &request);
    MPI_Recv(&data[2-1+(1-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_top, 0, MPI_COMM_WORLD, &status)    ;
    MPI_Wait(&request, &status);

    // c)
    MPI_Sendrecv(&data[2-1+(DOMAINSIZE-1-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_bottom, 0, &data[2-1+(1-1)*DOMAINSIZE], SUBDOMAIN, MPI_DOUBLE, rank_top, 0, MPI_COMM_WORLD, &status);

    if (rank==9) {
        printf("data of rank 9 after communication\n");
        for (j=0; j<DOMAINSIZE; j++) {
            for (i=0; i<DOMAINSIZE; i++) {
                printf("%.1f ", data[i+j*DOMAINSIZE]);
            }
            printf("\n");
        }
    }

    MPI_Finalize();
    return 0;
}
