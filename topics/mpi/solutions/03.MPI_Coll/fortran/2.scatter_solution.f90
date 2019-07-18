PROGRAM scatter

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the CSCS Summer School.        !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! CSCS take no responsibility for the use of the enclosed      !
! teaching material.                                           !
!                                                              !
! Purpose: a simple scatter                                    !
!                                                              !
! Contents: F-Source                                           !
!==============================================================!


   USE MPI
   IMPLICIT NONE
   INTEGER i, rank, size, senddata(20), receivedata, ierror

   CALL MPI_Init(ierror)
   CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)

   IF (size.GT.20) THEN
      IF (rank.EQ.0) THEN
         WRITE (*,*) "do not use more than 20 processors"
         CALL MPI_Finalize(ierror)
      END IF
   END IF
   IF (rank.EQ.0) THEN
      DO i=1, size, 1
         WRITE (6,*) 'enter a value:'
         CALL FLUSH(6)
         READ (*,*) senddata(i)
      END DO
   END IF

   ! scatter the value of senddata of rank 0 to receivedata of all ranks
   CALL MPI_Scatter(senddata, 1, MPI_INTEGER, receivedata, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)

   WRITE (*,*) "I am rank", rank, "and the value is", receivedata

   CALL MPI_Finalize(ierror)

END PROGRAM
