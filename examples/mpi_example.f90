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

end module mpi_example
