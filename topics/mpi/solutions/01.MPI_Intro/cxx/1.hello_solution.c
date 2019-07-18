/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the CSCS Summer School.        *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * CSCS take no responsibility for the use of the enclosed      *
 * teaching material.                                           *
 *                                                              *
 * Purpose: a simple MPI-program printing "hello world!"        *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/

/* Write a minimal  MPI program which prints "hello world by each MPI process  */

#include <stdio.h>
/* include mpi header file */
#include <mpi.h>

int main(int argc, char *argv[])
{

    /* initialize MPI */
    MPI_Init(&argc, &argv);

    /* print hello world from each process */
    printf ("Hello world!\n");

    /* finalize MPI */
    MPI_Finalize();
    return 0;
}
