module mpi_example_ad
  use mpi_example
  use mpi
  use mpi_ad
  implicit none

contains

  subroutine sum_reduce_fwd_ad(x, x_ad, comm)
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    integer, intent(in)  :: comm
    real :: tmp_ad
    real :: tmp
    integer :: ierr

    call MPI_Allreduce_fwd_ad(x, x_ad, tmp, tmp_ad, 1, MPI_REAL, MPI_SUM, comm, ierr) ! call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)
    x_ad = tmp_ad ! x = tmp
    x = tmp

    return
  end subroutine sum_reduce_fwd_ad

  subroutine sum_reduce_rev_ad(x_ad, comm)
    real, intent(inout) :: x_ad
    integer, intent(in)  :: comm
    real :: tmp_ad
    integer :: ierr

    tmp_ad = x_ad ! x = tmp
    x_ad = 0.0 ! x = tmp
    call MPI_Allreduce_rev_ad(x_ad, tmp_ad, 1, MPI_REAL, MPI_SUM, comm, ierr) ! call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)

    return
  end subroutine sum_reduce_rev_ad

  subroutine isend_irecv_fwd_ad(x, x_ad, y, y_ad, comm)
    integer, parameter :: tag = 0
    real, intent(inout) :: x(2)
    real, intent(inout) :: x_ad(2)
    real, intent(out) :: y
    real, intent(out) :: y_ad
    integer, intent(in)  :: comm
    integer :: reqr_ad
    integer :: reqs_ad
    integer :: rank
    integer :: ierr
    integer :: size
    integer :: pn
    integer :: pp
    integer :: reqr
    integer :: reqs

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, size, ierr)
    x(:) = x(:) + rank
    pn = modulo(rank - 1, size)
    pp = modulo(rank + 1, size)
    call MPI_Irecv_fwd_ad(x(1), x_ad(1), 1, MPI_REAL, pn, tag, comm, reqr, reqr_ad, ierr) ! call MPI_Irecv(x(1), 1, MPI_REAL, pn, tag, comm, reqr, ierr)
    call MPI_Isend_fwd_ad(x(2), x_ad(2), 1, MPI_REAL, pp, tag, comm, reqs, reqs_ad, ierr) ! call MPI_Isend(x(2), 1, MPI_REAL, pp, tag, comm, reqs, ierr)
    y_ad = x_ad(2) ! y = x(2)
    y = x(2)
    call MPI_Wait_fwd_ad(reqs, reqs_ad, MPI_STATUS_IGNORE, MPI_STATUS_IGNORE, ierr) ! call MPI_Wait(reqs, MPI_STATUS_IGNORE, ierr)
    call MPI_Wait_fwd_ad(reqr, reqr_ad, MPI_STATUS_IGNORE, MPI_STATUS_IGNORE, ierr) ! call MPI_Wait(reqr, MPI_STATUS_IGNORE, ierr)
    y_ad = x_ad(1) + y_ad ! y = x(1) + y
    y = x(1) + y

    return
  end subroutine isend_irecv_fwd_ad

  subroutine isend_irecv_rev_ad(x_ad, y_ad, comm)
    integer, parameter :: tag = 0
    real, intent(inout) :: x_ad(2)
    real, intent(inout) :: y_ad
    integer, intent(in)  :: comm
    integer :: reqr_ad
    integer :: reqs_ad
    integer :: rank
    integer :: ierr
    integer :: size
    integer :: pn
    integer :: pp

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, size, ierr)
    pn = modulo(rank - 1, size)
    pp = modulo(rank + 1, size)
    call MPI_Irecv_fwd_rev_ad(x_ad(1), 1, MPI_REAL, pn, tag, comm, reqr_ad, ierr)
    call MPI_Isend_fwd_rev_ad(x_ad(2), 1, MPI_REAL, pp, tag, comm, reqs_ad, ierr)

    x_ad(1) = y_ad + x_ad(1) ! y = x(1) + y
    call MPI_Wait_rev_ad(reqr_ad, ierr) ! call MPI_Wait(reqr, MPI_STATUS_IGNORE, ierr)
    call MPI_Wait_rev_ad(reqs_ad, ierr) ! call MPI_Wait(reqs, MPI_STATUS_IGNORE, ierr)
    x_ad(2) = y_ad + x_ad(2) ! y = x(2)
    y_ad = 0.0 ! y = x(2)
    call MPI_Isend_rev_ad(x_ad(2), 1, MPI_REAL, pp, tag, comm, reqs_ad, ierr) ! call MPI_Isend(x(2), 1, MPI_REAL, pp, tag, comm, reqs, ierr)
    call MPI_Irecv_rev_ad(x_ad(1), 1, MPI_REAL, pn, tag, comm, reqr_ad, ierr) ! call MPI_Irecv(x(1), 1, MPI_REAL, pn, tag, comm, reqr, ierr)

    return
  end subroutine isend_irecv_rev_ad

end module mpi_example_ad
