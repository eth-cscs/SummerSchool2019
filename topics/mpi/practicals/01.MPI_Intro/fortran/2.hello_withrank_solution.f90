PROGRAM hello

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the CSCS Summer School.        !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! CSCS take no responsibility for the use of the enclosed      !
! teaching material.                                           !
!                                                              !
! Purpose: A program to try MPI_Comm_size and MPI_Comm_rank.   !
!                                                              !
! Contents: F-Source                                           !
!                                                              !
!==============================================================!

  USE MPI

  IMPLICIT NONE

  ! Don't forget to declare variables

  INTEGER :: ierror, rank, size

  CALL MPI_INIT(ierror)

  ! Get the rank of each process
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)

  ! Get the size of the communicator
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)

  ! Write code such that every process writes its rank and the size of the communicator,
  ! but only process 0 prints "hello world"
  IF (rank .EQ. 0) THEN
      WRITE(*,*) 'Hello world!'
  END IF

  WRITE(*,*) 'I am process', rank, ' out of', size

  CALL MPI_FINALIZE(ierror)

END PROGRAM
