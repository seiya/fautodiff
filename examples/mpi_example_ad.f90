module mpi_example_ad
  use mpi_example
  use mpi
  use mpi_ad
  implicit none

contains

  subroutine sum_reduce_fwd_ad(x, x_ad, y, y_ad, comm)
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: y
    real, intent(inout) :: y_ad
    integer, intent(in)  :: comm
    real :: tmp_ad
    real :: tmp
    integer :: ierr

    call MPI_Allreduce_fwd_ad(x, x_ad, tmp, tmp_ad, 1, MPI_REAL, MPI_SUM, comm, ierr)
      ! call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)
    x_ad = tmp_ad ! x = tmp
    x = tmp
    call MPI_Allreduce_fwd_ad(MPI_IN_PLACE, y, y_ad, 1, MPI_REAL, MPI_SUM, comm, ierr)
      ! call MPI_Allreduce(MPI_IN_PLACE, y, 1, MPI_REAL, MPI_SUM, comm, ierr)

    return
  end subroutine sum_reduce_fwd_ad

  subroutine sum_reduce_rev_ad(x, x_ad, y, y_ad, comm)
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: y
    real, intent(inout) :: y_ad
    integer, intent(in)  :: comm
    real :: tmp_ad
    integer :: ierr

    call MPI_Allreduce_rev_ad(MPI_IN_PLACE, y, y_ad, 1, MPI_REAL, MPI_SUM, comm, ierr)
      ! call MPI_Allreduce(MPI_IN_PLACE, y, 1, MPI_REAL, MPI_SUM, comm, ierr)
    tmp_ad = x_ad ! x = tmp
    x_ad = 0.0 ! x = tmp
    call MPI_Allreduce_rev_ad(x, x_ad, tmp_ad, 1, MPI_REAL, MPI_SUM, comm, ierr)
      ! call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)

    return
  end subroutine sum_reduce_rev_ad

  subroutine isend_irecv_fwd_ad(x, x_ad, y, y_ad, comm)
    integer, parameter :: tag = 0
    real, intent(inout) :: x(3)
    real, intent(inout) :: x_ad(3)
    real, intent(out) :: y
    real, intent(out) :: y_ad
    integer, intent(in)  :: comm
    integer :: reqs_ad(2)
    integer :: rank
    integer :: ierr
    integer :: size
    integer :: pn
    integer :: pp
    integer :: reqs(2)

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, size, ierr)
    x(:) = x(:) + rank
    pn = rank - 1
    pp = rank + 1
    reqs_ad(:) = MPI_REQUEST_NULL
    reqs(:) = MPI_REQUEST_NULL
    if (pn >= 0) then
      call MPI_Irecv_fwd_ad(x(1), x_ad(1), 1, MPI_REAL, pn, tag, comm, reqs(1), reqs_ad(1), ierr)
        ! call MPI_Irecv(x(1), 1, MPI_REAL, pn, tag, comm, reqs(1), ierr)
    end if
    if (pp < size) then
      call MPI_Isend_fwd_ad(x(2), x_ad(2), 1, MPI_REAL, pp, tag, comm, reqs(2), reqs_ad(2), ierr)
        ! call MPI_Isend(x(2), 1, MPI_REAL, pp, tag, comm, reqs(2), ierr)
    end if
    y_ad = x_ad(2) ! y = x(2)
    y = x(2)
    call MPI_Waitall_fwd_ad(2, reqs, reqs_ad, MPI_STATUSES_IGNORE, MPI_STATUSES_IGNORE, ierr)
      ! call MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE, ierr)
    reqs_ad(:) = MPI_REQUEST_NULL
    reqs(:) = MPI_REQUEST_NULL
    if (pp < size) then
      call MPI_Irecv_fwd_ad(x(3), x_ad(3), 1, MPI_REAL, pp, tag + 1, comm, reqs(1), reqs_ad(1), ierr)
        ! call MPI_Irecv(x(3), 1, MPI_REAL, pp, tag+1, comm, reqs(1), ierr)
    end if
    if (pn >= 0) then
      call MPI_Isend_fwd_ad(x(2), x_ad(2), 1, MPI_REAL, pn, tag + 1, comm, reqs(2), reqs_ad(2), ierr)
        ! call MPI_Isend(x(2), 1, MPI_REAL, pn, tag+1, comm, reqs(2), ierr)
    end if
    call MPI_Waitall_fwd_ad(2, reqs, reqs_ad, MPI_STATUSES_IGNORE, MPI_STATUSES_IGNORE, ierr)
      ! call MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE, ierr)
    if (pn >= 0) then
      y_ad = x_ad(1) + y_ad ! y = x(1) + y
      y = x(1) + y
    end if
    if (pp < size) then
      y_ad = x_ad(3) + y_ad ! y = x(3) + y
      y = x(3) + y
    end if

    return
  end subroutine isend_irecv_fwd_ad

  subroutine isend_irecv_rev_ad(x_ad, y_ad, comm)
    integer, parameter :: tag = 0
    real, intent(inout) :: x_ad(3)
    real, intent(inout) :: y_ad
    integer, intent(in)  :: comm
    integer :: reqs_ad(2)
    integer :: rank
    integer :: ierr
    integer :: size
    integer :: pn
    integer :: pp
    integer :: reqs_ad_save_380_ad(2)

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, size, ierr)
    pn = rank - 1
    pp = rank + 1
    reqs_ad(:) = MPI_REQUEST_NULL
    if (pn >= 0) then
      call MPI_Irecv_fwd_rev_ad(x_ad(1), 1, MPI_REAL, pn, tag, comm, reqs_ad(1), ierr)
    end if
    if (pp < size) then
      call MPI_Isend_fwd_rev_ad(x_ad(2), 1, MPI_REAL, pp, tag, comm, reqs_ad(2), ierr)
    end if
    reqs_ad_save_380_ad(:) = reqs_ad(:)
    reqs_ad(:) = MPI_REQUEST_NULL
    if (pp < size) then
      call MPI_Irecv_fwd_rev_ad(x_ad(3), 1, MPI_REAL, pp, tag + 1, comm, reqs_ad(1), ierr)
    end if
    if (pn >= 0) then
      call MPI_Isend_fwd_rev_ad(x_ad(2), 1, MPI_REAL, pn, tag + 1, comm, reqs_ad(2), ierr)
    end if

    if (pp < size) then
      x_ad(3) = y_ad + x_ad(3) ! y = x(3) + y
    end if
    if (pn >= 0) then
      x_ad(1) = y_ad + x_ad(1) ! y = x(1) + y
    end if
    call MPI_Waitall_rev_ad(2, reqs_ad, ierr) ! call MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE, ierr)
    if (pn >= 0) then
      call MPI_Isend_rev_ad(x_ad(2), 1, MPI_REAL, pn, tag + 1, comm, reqs_ad(2), ierr)
        ! call MPI_Isend(x(2), 1, MPI_REAL, pn, tag+1, comm, reqs(2), ierr)
    end if
    if (pp < size) then
      call MPI_Irecv_rev_ad(x_ad(3), 1, MPI_REAL, pp, tag + 1, comm, reqs_ad(1), ierr)
        ! call MPI_Irecv(x(3), 1, MPI_REAL, pp, tag+1, comm, reqs(1), ierr)
    end if
    reqs_ad(:) = reqs_ad_save_380_ad(:)
    call MPI_Waitall_rev_ad(2, reqs_ad, ierr) ! call MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE, ierr)
    x_ad(2) = y_ad + x_ad(2) ! y = x(2)
    y_ad = 0.0 ! y = x(2)
    if (pp < size) then
      call MPI_Isend_rev_ad(x_ad(2), 1, MPI_REAL, pp, tag, comm, reqs_ad(2), ierr)
        ! call MPI_Isend(x(2), 1, MPI_REAL, pp, tag, comm, reqs(2), ierr)
    end if
    if (pn >= 0) then
      call MPI_Irecv_rev_ad(x_ad(1), 1, MPI_REAL, pn, tag, comm, reqs_ad(1), ierr)
        ! call MPI_Irecv(x(1), 1, MPI_REAL, pn, tag, comm, reqs(1), ierr)
    end if

    return
  end subroutine isend_irecv_rev_ad

  subroutine isend_irecv_alloc_fwd_ad(x, x_ad, y, y_ad, comm)
    integer, parameter :: tag = 0
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad
    integer, intent(in)  :: comm
    integer :: reqs_ad(4)
    real, allocatable :: z_ad(:)
    integer :: rank
    integer :: ierr
    integer :: size
    integer :: reqs(4)
    real, allocatable :: z(:)
    integer :: pn
    integer :: pp

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, size, ierr)
    reqs_ad(:) = MPI_REQUEST_NULL
    reqs(:) = MPI_REQUEST_NULL
    allocate(z(3))
    allocate(z_ad(3))
    z_ad(:) = x_ad ! z(:) = x + rank
    z(:) = x + rank
    pn = rank - 1
    pp = rank + 1
    if (pn >= 0) then
      call MPI_Irecv_fwd_ad(z(1), z_ad(1), 1, MPI_REAL, pn, tag, comm, reqs(1), reqs_ad(1), ierr)
        ! call MPI_Irecv(z(1), 1, MPI_REAL, pn, tag, comm, reqs(1), ierr)
      call MPI_Isend_fwd_ad(z(2), z_ad(2), 1, MPI_REAL, pn, tag + 1, comm, reqs(2), reqs_ad(2), ierr)
        ! call MPI_Isend(z(2), 1, MPI_REAL, pn, tag+1, comm, reqs(2), ierr)
    end if
    if (pp < size) then
      call MPI_Irecv_fwd_ad(z(3), z_ad(3), 1, MPI_REAL, pp, tag + 1, comm, reqs(3), reqs_ad(3), ierr)
        ! call MPI_Irecv(z(3), 1, MPI_REAL, pp, tag+1, comm, reqs(3), ierr)
      call MPI_Isend_fwd_ad(z(2), z_ad(2), 1, MPI_REAL, pp, tag, comm, reqs(4), reqs_ad(4), ierr)
        ! call MPI_Isend(z(2), 1, MPI_REAL, pp, tag, comm, reqs(4), ierr)
    end if
    y_ad = x_ad + z_ad(2) ! y = x + z(2)
    y = x + z(2)
    call MPI_Waitall_fwd_ad(4, reqs, reqs_ad, MPI_STATUSES_IGNORE, MPI_STATUSES_IGNORE, ierr)
      ! call MPI_Waitall(4, reqs, MPI_STATUSES_IGNORE, ierr)
    if (pn >= 0) then
      y_ad = z_ad(1) + y_ad ! y = z(1) + y
      y = z(1) + y
    end if
    if (pp < size) then
      y_ad = z_ad(3) + y_ad ! y = z(3) + y
      y = z(3) + y
    end if
    if (allocated(z)) then
      if (allocated(z_ad)) then
        deallocate(z_ad)
      end if
      if (allocated(z)) then
        deallocate(z)
      end if
    end if

    return
  end subroutine isend_irecv_alloc_fwd_ad

  subroutine isend_irecv_alloc_rev_ad(x_ad, y_ad, comm)
    integer, parameter :: tag = 0
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad
    integer, intent(in)  :: comm
    integer :: reqs_ad(4)
    real, allocatable :: z_ad(:)
    integer :: rank
    integer :: ierr
    integer :: size
    integer :: pn
    integer :: pp

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, size, ierr)
    reqs_ad(:) = MPI_REQUEST_NULL
    allocate(z_ad(3))
    z_ad = 0.0
    pn = rank - 1
    pp = rank + 1
    if (pn >= 0) then
      call MPI_Irecv_fwd_rev_ad(z_ad(1), 1, MPI_REAL, pn, tag, comm, reqs_ad(1), ierr)
      call MPI_Isend_fwd_rev_ad(z_ad(2), 1, MPI_REAL, pn, tag + 1, comm, reqs_ad(2), ierr)
    end if
    if (pp < size) then
      call MPI_Irecv_fwd_rev_ad(z_ad(3), 1, MPI_REAL, pp, tag + 1, comm, reqs_ad(3), ierr)
      call MPI_Isend_fwd_rev_ad(z_ad(2), 1, MPI_REAL, pp, tag, comm, reqs_ad(4), ierr)
    end if

    if (pp < size) then
      z_ad(3) = y_ad ! y = z(3) + y
    end if
    if (pn >= 0) then
      z_ad(1) = y_ad ! y = z(1) + y
    end if
    call MPI_Waitall_rev_ad(4, reqs_ad, ierr) ! call MPI_Waitall(4, reqs, MPI_STATUSES_IGNORE, ierr)
    x_ad = y_ad + x_ad ! y = x + z(2)
    z_ad(2) = y_ad ! y = x + z(2)
    y_ad = 0.0 ! y = x + z(2)
    if (pp < size) then
      call MPI_Isend_rev_ad(z_ad(2), 1, MPI_REAL, pp, tag, comm, reqs_ad(4), ierr)
        ! call MPI_Isend(z(2), 1, MPI_REAL, pp, tag, comm, reqs(4), ierr)
      call MPI_Irecv_rev_ad(z_ad(3), 1, MPI_REAL, pp, tag + 1, comm, reqs_ad(3), ierr)
        ! call MPI_Irecv(z(3), 1, MPI_REAL, pp, tag+1, comm, reqs(3), ierr)
    end if
    if (pn >= 0) then
      call MPI_Isend_rev_ad(z_ad(2), 1, MPI_REAL, pn, tag + 1, comm, reqs_ad(2), ierr)
        ! call MPI_Isend(z(2), 1, MPI_REAL, pn, tag+1, comm, reqs(2), ierr)
      call MPI_Irecv_rev_ad(z_ad(1), 1, MPI_REAL, pn, tag, comm, reqs_ad(1), ierr)
        ! call MPI_Irecv(z(1), 1, MPI_REAL, pn, tag, comm, reqs(1), ierr)
    end if
    x_ad = sum(z_ad(:)) + x_ad ! z(:) = x + rank
    if (allocated(z_ad)) then
      deallocate(z_ad)
    end if

    return
  end subroutine isend_irecv_alloc_rev_ad

end module mpi_example_ad
