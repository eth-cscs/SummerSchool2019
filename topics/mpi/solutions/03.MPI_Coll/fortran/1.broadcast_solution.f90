PROGRAM broadcast

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the CSCS Summer School.        !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! CSCS take no responsibility for the use of the enclosed      !
! teaching material.                                           !
!                                                              !
! Purpose: a simple broadcast                                  !
!                                                              !
! Contents: F-Source                                           !
!==============================================================!

   USE MPI
   IMPLICIT NONE
   INTEGER rank, data, ierror

   CALL MPI_Init(ierror)
   CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

   IF (rank.EQ.0) THEN
      WRITE (6,*) 'enter a value:'
      CALL FLUSH(6)
      READ (*,*) data
   END IF

   ! broadcast the value of data of rank 0 to all ranks
   CALL MPI_Bcast(data, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)

   WRITE (*,*) "I am rank", rank, "and the value is", data

   CALL MPI_Finalize(ierror)

END PROGRAM
