PROGRAM sum_ring

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the CSCS Summer School.        !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! CSCS take no responsibility for the use of the enclosed      !
! teaching material.                                           !
!                                                              !
! Purpose: A program to try MPI_Issend and MPI_Recv.           !
!                                                              !
! Contents: F-Source                                           !
!                                                              !
!==============================================================!

  USE MPI
  IMPLICIT NONE

  INTEGER :: ierror, my_rank, size

  INTEGER :: right, left

  INTEGER :: i, sum

  INTEGER :: snd_buf
  INTEGER :: rcv_buf

  INTEGER :: status(MPI_STATUS_SIZE)

  INTEGER :: request

  CALL MPI_INIT(ierror)

  CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierror)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)

  right = -1 !calculate the rank of the neighbor to your right
  left  = -1 !calculate the rank of the neighbor to your left

  ! below write ring addition code.
  ! do not use IF (RANK.EQ.0) THEN .. ELSE ..
  ! every rank sends initialy its rank number to a neighbor, and then sends what
  ! it receives from that neighbor, this is done n times with n = number of processes
  ! all ranks obtain the sum

  WRITE(*,*) "PE", my_rank, ": Sum =", sum

  CALL MPI_FINALIZE(ierror)

END PROGRAM
