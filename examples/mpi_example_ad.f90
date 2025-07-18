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

    call mpi_allreduce_fwd_ad(x, x_ad, tmp, tmp_ad, 1, MPI_REAL, MPI_SUM, comm, ierr) ! call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)
    x_ad = tmp_ad ! x = tmp

    return
  end subroutine sum_reduce_fwd_ad

  subroutine sum_reduce_rev_ad(x, x_ad, comm)
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    integer, intent(in)  :: comm

    x_ad = 0.0 ! x = tmp

    return
  end subroutine sum_reduce_rev_ad

end module mpi_example_ad
