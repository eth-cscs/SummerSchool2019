PROGRAM ping_pong

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the CSCS Summer School.        !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! CSCS take no responsibility for the use of the enclosed      !
! teaching material.                                           !
!                                                              !
! Purpose: A program to try MPI_Ssend and MPI_Recv.            !
!                                                              !
! Contents: F-Source                                           !
!==============================================================!

  USE MPI
  IMPLICIT NONE

  INTEGER length
  PARAMETER (length=1)

  INTEGER status(MPI_STATUS_SIZE)

  REAL buffer(length)

  INTEGER ierror, my_rank

  CALL MPI_INIT(ierror)

  CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierror)

  ! Process 0 sends a message(ping) to process 1.
  ! After receiving the message, process 1 sends a message (pong) to process 0

  WRITE (*,*) 'Ping-ping completed - rank ',my_rank

  CALL MPI_FINALIZE(ierror)

END PROGRAM
