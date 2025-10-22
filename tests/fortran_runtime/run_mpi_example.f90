program run_mpi_example
  use mpi
  use mpi_example_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_sum_reduce = 1
  integer, parameter :: I_isend_irecv = 2
  integer, parameter :: I_isend_irecv_alloc = 3

  integer :: length, status
  character(:), allocatable :: arg
  integer :: i_test

  ! MPI
  integer :: comm, rank
  integer :: ierr

  i_test = I_all ! default
    if (command_argument_count() > 0) then
     call get_command_argument(1, length=length, status=status)
     if (status == 0) then
        allocate(character(len=length) :: arg)
        call get_command_argument(1, arg, status=status)
        if (status == 0) then
           select case(arg)
           case ("sum_reduce")
              i_test = I_sum_reduce
           case ("isend_irecv")
              i_test = I_isend_irecv
           case ("isend_irecv_alloc")
              i_test = I_isend_irecv_alloc
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

  if (i_test == I_sum_reduce .or. i_test == I_all) then
     call test_sum_reduce
  end if
  if (i_test == I_isend_irecv .or. i_test == I_all) then
    call test_isend_irecv
  end if
  if (i_test == I_isend_irecv_alloc .or. i_test == I_all) then
    call test_isend_irecv_alloc
  end if

  call MPI_Finalize(ierr)

contains

  subroutine test_sum_reduce
    real :: x, x_ad
    real :: y, y_ad
    real :: x_eps, y_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 1.0
    y = 1.0
    call sum_reduce(x, y, comm)
    x_eps = 1.0 + eps
    y_eps = 1.0
    call sum_reduce(x_eps, y_eps, comm)
    fd = (x_eps - x) / eps
    x = 1.0
    y = 1.0
    x_ad = 1.0
    y_ad = 0.0
    call sum_reduce_fwd_ad(x, x_ad, y, y_ad, comm)
    if (abs((x_ad - fd) / fd) > tol) then
      print *, 'test_sum_reduce_fwd failed', x_ad, fd
      error stop 1
    end if

    inner1 = x_ad**2 + y_ad**2
    if (rank == 0) then
      call MPI_reduce(MPI_IN_PLACE, inner1, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    else
      call MPI_reduce(inner1, inner1, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    end if
    call sum_reduce_rev_ad(x, x_ad, y, y_ad, comm)
    inner2 = x_ad
    if (rank == 0) then
      call MPI_reduce(MPI_IN_PLACE, inner2, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    else
      call MPI_reduce(inner2, inner2, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    end if
    if (rank == 0) then
      if (abs((inner2 - inner1) / inner1) > tol) then
        print *, 'test_sum_reduce_rev failed', inner1, inner2
        call MPI_abort(comm, -1, ierr)
      end if
    end if

  end subroutine test_sum_reduce

  subroutine test_isend_irecv
    real :: x(3), x_ad(3)
    real :: y, y_ad
    real :: x_eps(3), y_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x(:) = 1.0
    call isend_irecv(x, y, comm)
    x_eps(:) = 1.0 + eps
    call isend_irecv(x_eps, y_eps, comm)
    fd = (y_eps - y) / eps
    x(:) = 1.0
    x_ad(:) = 1.0
    call isend_irecv_fwd_ad(x, x_ad, y, y_ad, comm)
    if (abs((y_ad - fd) / fd) > tol) then
      print *, 'test_isend_irecv_fwd failed', y_ad, fd
      error stop 1
    end if

    inner1 = y_ad**2 + x_ad(1)**2 + x_ad(2)**2 + x_ad(3)**2
    if (rank == 0) then
      call MPI_reduce(MPI_IN_PLACE, inner1, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    else
      call MPI_reduce(inner1, inner1, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    end if
    call isend_irecv_rev_ad(x_ad, y_ad, comm)
    inner2 = sum(x_ad)
    if (rank == 0) then
      call MPI_reduce(MPI_IN_PLACE, inner2, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    else
      call MPI_reduce(inner2, inner2, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    end if
    if (rank == 0) then
      if (abs((inner2 - inner1) / inner1) > tol) then
        print *, 'test_isend_irecv failed', inner1, inner2
        call MPI_abort(comm, -1, ierr)
      end if
    end if

  end subroutine test_isend_irecv

  subroutine test_isend_irecv_alloc
    real :: x, x_ad
    real :: y, y_ad
    real :: x_eps, y_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 1.0
    call isend_irecv_alloc(x, y, comm)
    x_eps = 1.0 + eps
    call isend_irecv_alloc(x_eps, y_eps, comm)
    fd = (y_eps - y) / eps

    x = 1.0
    x_ad = 1.0
    call isend_irecv_alloc_fwd_ad(x, x_ad, y, y_ad, comm)
    if (abs((y_ad - fd) / fd) > tol) then
      print *, 'test_isend_irecv_alloc_fwd failed', y_ad, fd
      error stop 1
    end if

    inner1 = y_ad**2 + x_ad**2
    if (rank == 0) then
      call MPI_reduce(MPI_IN_PLACE, inner1, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    else
      call MPI_reduce(inner1, inner1, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    end if
    call isend_irecv_alloc_rev_ad(x_ad, y_ad, comm)
    inner2 = x_ad
    if (rank == 0) then
      call MPI_reduce(MPI_IN_PLACE, inner2, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    else
      call MPI_reduce(inner2, inner2, 1, MPI_REAL, MPI_SUM, 0, comm, ierr)
    end if
    if (rank == 0) then
      if (abs((inner2 - inner1) / max(inner1, 1.0e-12)) > tol) then
        print *, 'test_isend_irecv_alloc_rev failed', inner1, inner2
        call MPI_abort(comm, -1, ierr)
      end if
    end if

  end subroutine test_isend_irecv_alloc

end program run_mpi_example
