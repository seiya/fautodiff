program run_block_construct
  use block_construct_ad
  implicit none
  real, parameter :: tol = 1.0e-3

  integer, parameter :: I_all = 0
  integer, parameter :: I_compute_module = 1
  integer, parameter :: I_use_block = 2

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
           case ("compute_module")
              i_test = I_compute_module
           case ("use_block")
              i_test = I_use_block
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_compute_module .or. i_test == I_all) then
     call test_compute_module
  end if
  if (i_test == I_use_block .or. i_test == I_all) then
     call test_use_block
  end if

  stop
contains

  subroutine test_compute_module
    real, parameter :: eps = 1.0e-3
    real :: x, x_ad
    real :: z_base, z_eps, fd, grad
    real :: inner1, inner2

    z = 0.0
    x = 2.0
    call compute_module(x)
    z_base = z
    z = 0.0
    call compute_module(x + eps)
    z_eps = z
    fd = (z_eps - z_base) / eps

    z = 0.0
    z_ad = 0.0
    x = 2.0
    x_ad = 1.0
    call compute_module_fwd_ad(x, x_ad)
    grad = z_ad
    if (abs((grad - fd) / max(abs(fd), tol)) > tol) then
       print *, 'test_compute_module_fwd failed', grad, fd
       error stop 1
    end if

    inner1 = grad**2
    x_ad = 0.0
    z_ad = grad
    call compute_module_rev_ad(x_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / max(abs(inner1), tol)) > tol) then
       print *, 'test_compute_module_rev failed', inner1, inner2
       error stop 1
    end if

    z = 0.0
    z_ad = 0.0

    return
  end subroutine test_compute_module

  subroutine test_use_block
    real, parameter :: eps = 1.0e-3
    real :: x, y, y_eps
    real :: x_ad, y_ad
    real :: fd, inner1, inner2

    x = 1.2
    call use_block(x, y)
    call use_block(x + eps, y_eps)
    fd = (y_eps - y) / eps

    x = 1.2
    x_ad = 1.0
    call use_block_fwd_ad(x, x_ad, y, y_ad)
    if (abs((y_ad - fd) / max(abs(fd), tol)) > tol) then
       print *, 'test_use_block_fwd failed', y_ad, fd
       error stop 1
    end if

    inner1 = y_ad**2
    x_ad = 0.0
    call use_block_rev_ad(x_ad, y_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / max(abs(inner1), tol)) > tol) then
       print *, 'test_use_block_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_use_block

end program run_block_construct
