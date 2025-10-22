module mpi_sub_use_ad
  use mpi_ad
  implicit none


contains

  subroutine foo(x, comm)
    use mpi
    real, intent(inout) :: x
    integer, intent(in)  :: comm
    real :: tmp
    integer :: ierr

    call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)
    x = tmp

    return
  end subroutine foo

  subroutine foo_fwd_ad(x, x_ad, comm)
    use mpi
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    integer, intent(in)  :: comm
    real :: tmp_ad
    real :: tmp
    integer :: ierr

    call MPI_Allreduce_fwd_ad(x, x_ad, tmp, tmp_ad, 1, MPI_REAL, MPI_SUM, comm, ierr)
      ! call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)
    x_ad = tmp_ad ! x = tmp
    x = tmp

    return
  end subroutine foo_fwd_ad

  subroutine foo_rev_ad(x, x_ad, comm)
    use mpi
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    integer, intent(in)  :: comm
    real :: tmp_ad
    integer :: ierr

    tmp_ad = x_ad ! x = tmp
    x_ad = 0.0 ! x = tmp
    call MPI_Allreduce_rev_ad(x, x_ad, tmp_ad, 1, MPI_REAL, MPI_SUM, comm, ierr)
      ! call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)

    return
  end subroutine foo_rev_ad

end module mpi_sub_use_ad
