program run_mpi_sub_use
  use mpi
  use mpi_sub_use_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_foo = 1

  integer :: length, status
  character(:), allocatable :: arg
  integer :: i_test
  integer :: ierr
  integer :: comm, rank, size

  i_test = I_all
  if (command_argument_count() > 0) then
     call get_command_argument(1, length=length, status=status)
     if (status == 0) then
        allocate(character(len=length) :: arg)
        call get_command_argument(1, arg, status=status)
        if (status == 0) then
           select case(arg)
           case ("foo")
              i_test = I_foo
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  call MPI_Init(ierr)
  comm = MPI_COMM_WORLD
  call MPI_Comm_rank(comm, rank, ierr)
  call MPI_Comm_size(comm, size, ierr)

  if (i_test == I_foo .or. i_test == I_all) then
     call test_foo(comm, rank)
  end if

  call MPI_Finalize(ierr)

  stop
contains

  subroutine test_foo(comm, rank)
    integer, intent(in) :: comm, rank
    real, parameter :: eps = 1.0e-3
    real :: x, x_base, x_eps
    real :: x_ad
    real :: fd
    real :: inner1_local, inner1_global
    real :: inner2_local, inner2_global
    integer :: ierr

    x = 1.0 + real(rank)
    call foo(x, comm)
    x_base = x

    x = 1.0 + real(rank) + eps
    call foo(x, comm)
    x_eps = x
    fd = (x_eps - x_base) / eps

    x = 1.0 + real(rank)
    x_ad = 1.0
    call foo_fwd_ad(x, x_ad, comm)
    if (abs((x_ad - fd) / max(1.0e-12, abs(fd))) > tol) then
       print *, 'test_mpi_sub_use_fwd failed on rank', rank, x_ad, fd
       call MPI_Abort(comm, -1, ierr)
    end if

    inner1_local = x_ad**2
    call MPI_Reduce(inner1_local, inner1_global, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)

    inner2_local = x_ad
    x = 1.0 + real(rank)
    call foo_rev_ad(x, inner2_local, comm)
    call MPI_Reduce(inner2_local, inner2_global, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)

    if (rank == 0) then
       if (abs((inner2_global - inner1_global) / max(1.0e-12, abs(inner1_global))) > tol) then
          print *, 'test_mpi_sub_use_rev failed', inner1_global, inner2_global
          call MPI_Abort(comm, -1, ierr)
       end if
    end if

    return
  end subroutine test_foo

end program run_mpi_sub_use
