module mpi_example
  use mpi
  implicit none

contains

  subroutine sum_reduce(x, comm)
    real, intent(inout) :: x
    integer, intent(in) :: comm
    real :: tmp
    integer :: ierr

    call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)
    x = tmp

    return
  end subroutine sum_reduce

  subroutine isend_irecv(x, y, comm)
    real, intent(inout) :: x(2)
    real, intent(out) :: y
    integer, intent(in) :: comm
    integer, parameter :: tag = 0
    integer :: reqr, reqs
    integer :: ierr, rank, size
    integer :: pn, pp

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, size, ierr)

    x(:) = x(:) + rank
    pn = modulo(rank - 1, size)
    pp = modulo(rank + 1, size)
    call MPI_Irecv(x(1), 1, MPI_REAL, pn, tag, comm, reqr, ierr)
    call MPI_Isend(x(2), 1, MPI_REAL, pp, tag, comm, reqs, ierr)
    y = x(2)
    call MPI_Wait(reqs, MPI_STATUS_IGNORE, ierr)
    call MPI_Wait(reqr, MPI_STATUS_IGNORE, ierr)
    y = x(1) + y

    return
  end subroutine isend_irecv

end module mpi_example
