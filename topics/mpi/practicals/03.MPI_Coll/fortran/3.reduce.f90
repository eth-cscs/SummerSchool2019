PROGRAM reduce

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the CSCS Summer School.        !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! CSCS take no responsibility for the use of the enclosed      !
! teaching material.                                           !
!                                                              !
! Purpose: a simple reduce                                     !
!                                                              !
! Contents: F-Source                                           !
!==============================================================!


   USE MPI
   IMPLICIT NONE
   INTEGER rank, input, result, ierror

   CALL MPI_Init(ierror)
   CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

   input=rank+1

   ! reduce the values of the different ranks in input to result of rank 0
   ! with the operation sum (max, logical and)

   IF (rank.EQ.0) THEN
      write (*,*) 'Rank 0 says: result is', result
   END IF

   CALL MPI_Finalize(ierror)

END PROGRAM
