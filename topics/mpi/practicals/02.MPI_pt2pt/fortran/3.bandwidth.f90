PROGRAM bandwidth

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the CSCS Summer School.        !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! CSCS take no responsibility for the use of the enclosed      !
! teaching material.                                           !
!                                                              !
! Purpose: Measuring bandwidth using a ping-pong               !
!                                                              !
! Contents: F-Source                                           !
!==============================================================!

! NOTE: make a reservation with two nodes:
! salloc ... -N 2 -n 2 ....
! start mpi using 2 nodes with one process per node:
! srun -N 2 -n 2 .......
! use gnuplot to plot the result:
! module unload ddt
! gnuplot bandwidth.gp
! display bandwisth.png (login with "ssh -Y <machine>")
!
! Advanced: try on only one node, explain the bandwidth values
! srun -N 1 -n 2 .......

  USE MPI
  IMPLICIT NONE

  INTEGER PROCESS_A
  PARAMETER(PROCESS_A=0)

  INTEGER PROCESS_B
  PARAMETER(PROCESS_B=1)

  INTEGER PING
  PARAMETER(PING=17) ! message tag

  INTEGER PONG
  PARAMETER(PONG=23) ! message tag

  INTEGER NMESSAGES
  PARAMETER (NMESSAGES=100)

  INTEGER INI_SIZE
  PARAMETER (INI_SIZE=1)

  INTEGER FACT_SIZE
  PARAMETER (FACT_SIZE=2)

  INTEGER REFINE_SIZE_MIN
  PARAMETER (REFINE_SIZE_MIN=1*1024)

  INTEGER REFINE_SIZE_MAX
  PARAMETER (REFINE_SIZE_MAX=16*1024)

  INTEGER SUM_SIZE
  PARAMETER (SUM_SIZE=1*1024)

  INTEGER MAX_SIZE
  PARAMETER (MAX_SIZE=536870912)

  DOUBLE PRECISION tstart, tstop, transfer_time
  INTEGER status(MPI_STATUS_SIZE)

  CHARACTER, ALLOCATABLE :: buffer(:)

  INTEGER length_of_message
  INTEGER ierror, my_rank, size

  ALLOCATE(buffer(MAX_SIZE))

  OPEN(1,file='bandwidth.dat',status='old')

  CALL MPI_INIT(ierror)

  CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierror)

  length_of_message = INI_SIZE;
  DO WHILE(length_of_message.LE.MAX_SIZE)

     ! Write a loop of NMESSAGES iterations which do a ping pong.
     ! Make the size of the message variable and display the bandwidth for each of them.
     ! What do you observe? (plot it)

     IF (my_rank.EQ.PROCESS_A) THEN
        transfer_time = (tstop - tstart) / (2 * NMESSAGES);
        WRITE(*,*) length_of_message,' ',transfer_time,' ',(length_of_message/transfer_time)/(1024*1024)
        WRITE(1,*) length_of_message,' ',transfer_time,' ',(length_of_message/transfer_time)/(1024*1024)
     END IF

     IF (length_of_message.GE.REFINE_SIZE_MIN .AND. length_of_message.LT.REFINE_SIZE_MAX) THEN
        length_of_message = length_of_message + SUM_SIZE;
     ELSE
        length_of_message = length_of_message * FACT_SIZE;
     END IF

  END DO

  CLOSE(1)
  DEALLOCATE(buffer)

  CALL MPI_FINALIZE(ierror)

END PROGRAM
