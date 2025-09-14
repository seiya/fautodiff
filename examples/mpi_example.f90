module mpi_example
  use mpi
  implicit none

contains

  subroutine sum_reduce(x, y, comm)
    real, intent(inout) :: x, y
    integer, intent(in) :: comm
    real :: tmp
    integer :: ierr

    call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)
    x = tmp

    call MPI_Allreduce(MPI_IN_PLACE, y, 1, MPI_REAL, MPI_SUM, comm, ierr)

    return
  end subroutine sum_reduce

  subroutine isend_irecv(x, y, comm)
    real, intent(inout) :: x(3)
    real, intent(out) :: y
    integer, intent(in) :: comm
    integer, parameter :: tag = 0
    integer :: reqs(2)
    integer :: ierr, rank, size
    integer :: pn, pp

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, size, ierr)

    reqs(:) = MPI_REQUEST_NULL

    x(:) = x(:) + rank
    pn = rank - 1
    pp = rank + 1

    if (pn >= 0) then
      call MPI_Irecv(x(1), 1, MPI_REAL, pn, tag, comm, reqs(1), ierr)
    end if
    if (pp < size) then
      call MPI_Isend(x(2), 1, MPI_REAL, pp, tag, comm, reqs(2), ierr)
    end if
    y = x(2)
    call MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE, ierr)

    if (pp < size) then
      call MPI_Irecv(x(3), 1, MPI_REAL, pp, tag+1, comm, reqs(1), ierr)
    end if
    if (pn >= 0) then
      call MPI_Isend(x(2), 1, MPI_REAL, pn, tag+1, comm, reqs(2), ierr)
    end if
    call MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE, ierr)

    if (pn >= 0) then
      y = x(1) + y
    end if
    if (pp < size) then
      y = x(3) + y
    end if

    return
  end subroutine isend_irecv

  subroutine isend_irecv_alloc(x, y, comm)
    real, intent(in) :: x
    real, intent(out) :: y
    integer, intent(in) :: comm
    integer, parameter :: tag = 0
    real, allocatable :: z(:)
    integer :: reqs(4)
    integer :: ierr, rank, size
    integer :: pn, pp

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, size, ierr)

    reqs(:) = MPI_REQUEST_NULL

    allocate(z(3))
    z(:) = x + rank
    pn = rank - 1
    pp = rank + 1
    if (pn >= 0) then
      call MPI_Irecv(z(1), 1, MPI_REAL, pn, tag, comm, reqs(1), ierr)
      call MPI_Isend(z(2), 1, MPI_REAL, pn, tag+1, comm, reqs(2), ierr)
    end if
    if (pp < size) then
      call MPI_Irecv(z(3), 1, MPI_REAL, pp, tag+1, comm, reqs(3), ierr)
      call MPI_Isend(z(2), 1, MPI_REAL, pp, tag, comm, reqs(4), ierr)
    end if
    y = x + z(2)
    call MPI_Waitall(4, reqs, MPI_STATUSES_IGNORE, ierr)
    if (pn >= 0) then
      y = z(1) + y
    end if
    if (pp < size) then
      y = z(3) + y
    end if

    if (allocated(z)) then
      deallocate(z)
    end if

    return
  end subroutine isend_irecv_alloc

end module mpi_example