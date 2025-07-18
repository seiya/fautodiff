program run_mpi_example
  use mpi
  use mpi_example
  use mpi_example_ad
  implicit none
  real, parameter :: tol = 1.0e-4
  integer :: ierr

  call MPI_Init(ierr)

  call test_sum_reduce

  call MPI_Finalize(ierr)

contains

  subroutine test_sum_reduce
    real :: x, x_ad
    real :: x_eps, fd, eps
    real :: inner1, inner2
    integer :: comm

    eps = 1.0e-3
    comm = MPI_COMM_WORLD
    x = 1.0
    call sum_reduce(x, comm)
    x_eps = 1.0 + eps
    call sum_reduce(x_eps, comm)
    fd = (x_eps - x) / eps
    x = 1.0
    x_ad = 1.0
    call sum_reduce_fwd_ad(x, x_ad, comm)
    if (abs((x_ad - fd) / fd) > tol) then
      print *, 'test_sum_reduce_fwd failed', x_ad, fd
      error stop 1
    end if

    inner1 = x_ad**2
    call sum_reduce_rev_ad(x, x_ad, comm)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
      print *, 'test_sum_reduce_rev failed', inner1, inner2
      error stop 1
    end if

  end subroutine test_sum_reduce

end program run_mpi_example
