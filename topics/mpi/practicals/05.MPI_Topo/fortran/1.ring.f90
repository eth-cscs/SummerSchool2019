PROGRAM ring

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the CSCS Summer School.        !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! CSCS take no responsibility for the use of the enclosed      !
! teaching material.                                           !
!                                                              !
! Purpose: Creating a 1-dimensional topology.                  !
!                                                              !
! Contents: F-Source                                           !
!==============================================================!

  USE MPI
  IMPLICIT NONE

  INTEGER, PARAMETER :: to_right=201
  INTEGER, PARAMETER :: max_dims=1
  INTEGER :: ierror, my_rank, size
  INTEGER :: right, left
  INTEGER :: i, sum
  INTEGER, ASYNCHRONOUS :: snd_buf
  INTEGER :: rcv_buf
  INTEGER :: status(MPI_STATUS_SIZE)
  INTEGER :: request
  INTEGER(KIND=MPI_ADDRESS_KIND) :: iadummy

  INTEGER :: new_comm
  INTEGER :: dims(max_dims)
  LOGICAL :: reorder, periods(max_dims)


  CALL MPI_Init(ierror)

  CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
  CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)

  ! Set one-dimensional cartesian topology.


  ! Get nearest neighbour ranks.

  ! Compute the sum.
  sum = 0
  snd_buf = my_rank

  DO i = 1, size

     CALL MPI_Isend(snd_buf, 1, MPI_INTEGER, right, to_right, new_comm, request, ierror)

     CALL MPI_Recv(rcv_buf, 1, MPI_INTEGER, left, to_right, new_comm, status, ierror)

     CALL MPI_Wait(request, status, ierror)

     CALL MPI_Get_address(snd_buf, iadummy, ierror)

     snd_buf = rcv_buf
     sum = sum + rcv_buf

  END DO

  WRITE(*,*) "Rank", my_rank, ": Sum =", sum

  CALL MPI_Finalize(ierror)

END PROGRAM
