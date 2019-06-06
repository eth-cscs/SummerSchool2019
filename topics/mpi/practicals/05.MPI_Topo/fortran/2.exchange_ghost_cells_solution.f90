PROGRAM ghost_cell_exchange

! Use only 16 processes for this exercise
! Send the ghost cell in two directions: left<->right and top<->bottom
! ranks are connected in a cyclic manner, for instance, rank 0 and 12 are connected
! process decomposition on 4*4 grid 
!  |-----------|
!  | 0| 4| 8|12|
!  |-----------|
!  | 1| 5| 9|13|
!  |-----------|
!  | 2| 6|10|14|
!  |-----------|
!  | 3| 7|11|15|
!  |-----------|

! Each process works on a 10*10 (SUBDOMAIN) block of data
! the d corresponds to data, g corresponds to "ghost cells"
! and x are empty (not exchanged for now)

!  xggggggggggx
!  gddddddddddg
!  gddddddddddg
!  gddddddddddg
!  gddddddddddg
!  gddddddddddg
!  gddddddddddg
!  gddddddddddg
!  gddddddddddg
!  gddddddddddg
!  gddddddddddg
!  xggggggggggx

  USE MPI
  IMPLICIT NONE

  INTEGER SUBDOMAIN, DOMAINSIZE
  PARAMETER (SUBDOMAIN = 10)
  PARAMETER (DOMAINSIZE = SUBDOMAIN+2)
  INTEGER rank, size, i, j, rank_right, rank_left, ierror
  DOUBLE PRECISION data(DOMAINSIZE,DOMAINSIZE)
  INTEGER request
  INTEGER status(MPI_STATUS_SIZE)
  INTEGER rank_top, rank_bottom
  INTEGER dims(2), periods(2)
  INTEGER comm_cart
  INTEGER data_ghost

  CALL MPI_Init(ierror)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
  CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)

  IF (size.NE.16) THEN
     WRITE (*,*)"please run this with 16 processors"
     CALL MPI_Finalize(ierror);
     STOP
  END IF
  DO i=1, DOMAINSIZE, 1
    DO j=1, DOMAINSIZE, 1
      data(i,j)=rank
    END DO
  END DO

  ! neighbouring ranks with cartesian grid communicator
  ! we do not allow the reordering of ranks here
  dims(1)=4
  dims(2)=4
  periods(1)=1
  periods(2)=1

  ! an alternative solution would be to allow the reordering and to use the new communicator for the communication
  ! then the MPI library has the opportunity to choose the best rank order with respect to performance
  ! CREATE a cartesian communicator (4*4) with periodic boundaries and use it to find your neighboring
  ! ranks in all dimensions.
  CALL MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, 0, comm_cart, ierror)
  CALL MPI_Cart_shift(comm_cart, 0, 1, rank_left, rank_right, ierror)
  CALL MPI_Cart_shift(comm_cart, 1, 1, rank_top, rank_bottom, ierror)

  ! derived datatype, create a datatype to send the rows
  CALL MPI_Type_vector(SUBDOMAIN, 1, DOMAINSIZE, MPI_DOUBLE, data_ghost, ierror)
  CALL MPI_Type_commit(data_ghost, ierror)

  !  ghost cell exchange with the neighbouring cells in all directions
  !  to the left
  CALL MPI_Irecv(data(2,DOMAINSIZE), SUBDOMAIN, MPI_DOUBLE, rank_right, 0, MPI_COMM_WORLD, request, ierror)
  CALL MPI_Send(data(2,2), SUBDOMAIN, MPI_DOUBLE, rank_left, 0, MPI_COMM_WORLD, ierror)
  CALL MPI_Wait(request, status, ierror)

  !  to the right
  CALL MPI_Irecv(data(2,1), SUBDOMAIN, MPI_DOUBLE, rank_left, 0, MPI_COMM_WORLD, request, ierror)
  CALL MPI_Send(data(2,DOMAINSIZE-1), SUBDOMAIN, MPI_DOUBLE, rank_right, 0, MPI_COMM_WORLD, ierror)
  CALL MPI_Wait(request, status, ierror)

  !  to the top
  CALL MPI_Irecv(data(DOMAINSIZE,2), 1, data_ghost, rank_bottom, 0, MPI_COMM_WORLD, request, ierror)
  CALL MPI_Send(data(2,2), 1, data_ghost, rank_top, 0, MPI_COMM_WORLD, ierror)
  CALL MPI_Wait(request, status, ierror)

  !  to the bottom
  CALL MPI_Irecv(data(1,2), 1, data_ghost, rank_top, 0, MPI_COMM_WORLD, request, ierror)
  CALL MPI_Send(data(SUBDOMAIN+1,2), 1, data_ghost, rank_bottom, 0, MPI_COMM_WORLD, ierror)
  CALL MPI_Wait(request, status, ierror)

  IF (rank.EQ.9) THEN
     WRITE (*,*) 'data of rank 9 after communication'
     DO i=1, DOMAINSIZE, 1
        DO j=1, DOMAINSIZE, 1
          WRITE (*,'(F6.1)',advance='no') data(i,j)
        END DO
        WRITE (*,*)
     END DO
  END IF

  CALL MPI_Type_free(data_ghost, ierror)
  CALL MPI_Comm_free(comm_cart, ierror)
  CALL MPI_Finalize(ierror)

END PROGRAM
