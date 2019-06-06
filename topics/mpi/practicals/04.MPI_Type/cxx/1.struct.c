/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS take no responsibility for the use of the enclosed      *
 * teaching material.                                           *
 *                                                              *
 * Purpose: Struct derived datatype                             *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/

#include <stdio.h>
#include <mpi.h>

#define NELEM 25

int main(int argc, char *argv[])
{

    typedef struct {
        float x, y, z;
        float velocity;
        int  n, type;
    } Particle;

    int numtasks, rank, source=0, dest, tag=1, i;
    Particle p[NELEM];
    MPI_Datatype particletype, oldtypes[2];
    MPI_Status stat;
    int blockcounts[2];

    /* MPI_Aint type is used to be consistent with syntax of MPI_Type_extent */
    MPI_Aint offsets[2], extent;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    /* Setup description of the 4 MPI_FLOAT fields x, y, z, velocity */

    /* Setup description of the 2 MPI_INT fields n, type */

    /* Now define structured type and commit it */

    /* setup and send particules */
    if (rank == 0) {
        for (i=0; i<NELEM; i++) {
            p[i].x = i * 1.0;
            p[i].y = i * -1.0;
            p[i].z = i * 1.0;
            p[i].velocity = 0.25;
            p[i].n = i;
            p[i].type = i % 2;
        }
    }

    MPI_Bcast(p, NELEM, particletype, 0, MPI_COMM_WORLD);

    printf("rank= %d   %3.2f %3.2f %3.2f %3.2f %d %d\n", rank, p[3].x, p[3].y, p[3].z, p[3].velocity, p[3].n, p[3].type);

    /* Free type */
    MPI_Finalize();
    return 0;
}
