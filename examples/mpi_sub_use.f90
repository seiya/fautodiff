module mpi_sub_use
  implicit none
contains
  subroutine foo(x, comm)
    use mpi
    real, intent(inout) :: x
    integer, intent(in) :: comm
    real :: tmp
    integer :: ierr
    call MPI_Allreduce(x, tmp, 1, MPI_REAL, MPI_SUM, comm, ierr)
    x = tmp
    return
  end subroutine foo
end module mpi_sub_use
