PROGRAM struct

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the CSCS Summer School.        !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! CSCS take no responsibility for the use of the enclosed      !
! teaching material.                                           !
!                                                              !
! Purpose: Struct derived datatype                             !
!                                                              !
! Contents: F-Source                                           !
!==============================================================!

   USE MPI
   IMPLICIT NONE

   INTEGER NELEM
   PARAMETER(NELEM=25)

   TYPE Particle
   SEQUENCE
   REAL*4 x, y, z, velocity
   INTEGER n, type
   END TYPE Particle

   INTEGER numtasks, rank, source, dest, tag, i, ierror
   INTEGER stat(MPI_STATUS_SIZE)
   TYPE (Particle) p(NELEM)
   INTEGER particletype, oldtypes(0:1), blockcounts(0:1), extent
   INTEGER(KIND=MPI_ADDRESS_KIND) offsets(0:1) 
   
   CALL MPI_Init(ierror)
   CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   CALL MPI_Comm_size(MPI_COMM_WORLD, numtasks, ierror)

   ! Setup description of the 4 MPI_FLOAT fields x, y, z, velocity
   offsets(0) = 0
   oldtypes(0) = MPI_REAL
   blockcounts(0) = 4

   ! Setup description of the 2 MPI_INT fields n, type
   ! Need to first identify the offset by getting size of MPI_FLOAT
   CALL MPI_Type_extent(MPI_REAL, extent, ierror)
   offsets(1) = 4 * extent
   oldtypes(1) = MPI_INTEGER
   blockcounts(1) = 2

   ! Now define structured type and commit it
   CALL MPI_Type_create_struct(2, blockcounts, offsets, oldtypes, particletype, ierror)
   CALL MPI_Type_commit(particletype, ierror)

   ! Initialize the particle array and then send it to each task
   IF (rank.EQ.0) THEN
      DO i=0, NELEM-1
         p(i) = Particle(1.0*i, -1.0*i, 1.0*i, 0.25, i, mod(i,2))
      END DO
   ENDIF

   CALL MPI_Bcast(p, NELEM, particletype, 0, MPI_COMM_WORLD, ierror)

   WRITE (*,*) "Rank= ", rank, p(3)

   ! Free the type
   CALL MPI_Type_free(particletype, ierror)
   CALL MPI_Finalize(ierror)

END PROGRAM
