program run_mpi_persistent
  use mpi
  use mpi_ad
  implicit none

  integer :: ierr, comm, rank, size
  integer, parameter :: tag = 123
  integer :: req_ad
  real :: sendbuf_ad(2), recvbuf_ad(2)

  call MPI_Init(ierr)
  comm = MPI_COMM_WORLD
  call MPI_Comm_rank(comm, rank, ierr)
  call MPI_Comm_size(comm, size, ierr)
  if (size < 2) then
    print *, 'Need at least 2 ranks for persistent test'
    call MPI_Abort(comm, 1, ierr)
  end if

  sendbuf_ad = 0.0
  recvbuf_ad = 0.0

  if (rank == 0) then
    ! Forward-of-reverse: create persistent pair for reverse flow from rank 1 -> 0
    call mpi_send_init_fwd_rev_ad(recvbuf_ad, 2, MPI_REAL, 1, tag, comm, req_ad, ierr)
  else if (rank == 1) then
    call mpi_recv_init_fwd_rev_ad(sendbuf_ad, 2, MPI_REAL, 0, tag, comm, req_ad, ierr)
    sendbuf_ad = [2.5, -1.5]
  end if

  ! Reverse sweep: start persistent ops
  call mpi_wait_rev_ad(req_ad, ierr)
  call mpi_start_rev_ad(req_ad, ierr)

  if (rank == 0) then
    if (abs(recvbuf_ad(1) - 2.5) > 1.0e-6 .or. abs(recvbuf_ad(2) + 1.5) > 1.0e-6) then
      print *, 'Persistent r4 failed: ', recvbuf_ad
      call MPI_Abort(comm, 2, ierr)
    end if
  end if

  ! Cleanup
  if (rank == 0) then
    call mpi_send_init_rev_ad(recvbuf_ad, 2, MPI_REAL, 1, tag, comm, req_ad, ierr)
  else if (rank == 1) then
    call mpi_recv_init_rev_ad(sendbuf_ad, 2, MPI_REAL, 0, tag, comm, req_ad, ierr)
  end if

  call MPI_Finalize(ierr)
end program run_mpi_persistent

