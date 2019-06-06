PROGRAM allreduce

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the CSCS Summer School.        !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! CSCS take no responsibility for the use of the enclosed      !
! teaching material.                                           !
!                                                              !
! Purpose: a simple reduce to all                              !
!                                                              !
! Contents: F-Source                                           !
!==============================================================!

  USE MPI
  IMPLICIT NONE
  INTEGER :: ierror, my_rank
  INTEGER :: sum

  CALL MPI_Init(ierror)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

  ! calculate sum of all ranks

  WRITE(*,*) "Rank", my_rank, ": Sum =", sum

  CALL MPI_Finalize(ierror)

END PROGRAM
