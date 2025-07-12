program run_directives
  use directives
  use directives_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_add_const = 1
  integer, parameter :: I_worker = 2

  integer :: length, status
  character(:), allocatable :: arg
  integer :: i_test

  i_test = I_all
  if (command_argument_count() > 0) then
     call get_command_argument(1, length=length, status=status)
     if (status == 0) then
        allocate(character(len=length) :: arg)
        call get_command_argument(1, arg, status=status)
        if (status == 0) then
           select case(arg)
           case ("add_const")
              i_test = I_add_const
           case ("worker")
              i_test = I_worker
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_add_const .or. i_test == I_all) then
     call test_add_const
  end if
  if (i_test == I_worker .or. i_test == I_all) then
     call test_worker
  end if

  stop
contains

  subroutine test_add_const
    real :: x, y, z
    real :: x_ad, y_ad
    real :: y_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 2.0
    z = 3.0
    call add_const(x, y, z)
    call add_const(x + eps, y_eps, z)
    fd = (y_eps - y) / eps
    x_ad = 1.0
    call add_const_fwd_ad(x, x_ad, y, y_ad, z)
    if (abs(y_ad - fd) > tol) then
       print *, 'test_add_const_fwd failed', y_ad, fd
       error stop 1
    end if

    inner1 = y_ad**2
    call add_const_rev_ad(x, x_ad, y_ad, z)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_add_const_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_add_const

  subroutine test_worker
    real :: x, z
    real :: x_ad, z_ad
    real :: z_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 2.0
    call worker(x, z)
    call worker(x + eps, z_eps)
    fd = (z_eps - z) / eps
    x_ad = 1.0
    call worker_fwd_ad(x, x_ad, z, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_worker_fwd failed', z_ad, fd
       error stop 1
    end if

    inner1 = z_ad**2
    call worker_rev_ad(x, x_ad, z_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_worker_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_worker

end program run_directives
