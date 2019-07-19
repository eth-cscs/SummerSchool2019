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
! Purpose: a simple MPI-program printing "hello world!"        !
!                                                              !
! Contents: F-Source                                           !
!==============================================================!

! Write a minimal  MPI program which prints "hello world by each MPI process

! Include header file
  USE MPI

  IMPLICIT NONE
  INTEGER :: ierror

  ! Initialize MPI
  CALL MPI_INIT(ierror)

  ! Print hello world from each process
  WRITE(*,*) 'Hello world!'

  ! Finalize MPI
  CALL MPI_FINALIZE(ierror)

END PROGRAM
