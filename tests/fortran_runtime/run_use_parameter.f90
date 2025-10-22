program run_use_parameter
  use support_mod_ad
  use use_parameter_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_scale_and_shift = 1

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
           case ("scale_and_shift")
              i_test = I_scale_and_shift
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_scale_and_shift .or. i_test == I_all) then
     call test_scale_and_shift
  end if

  stop
contains

  subroutine test_scale_and_shift
    real, parameter :: eps = 1.0e-3
    real :: x, y, x_out, y_out
    real :: x_eps, y_eps
    real :: x_ad, y_ad
    real :: fd_y, inner1, inner2

    x = 1.0
    call scale_and_shift(x, y)
    x_out = x
    y_out = y

    x_eps = 1.0 + eps
    call scale_and_shift(x_eps, y_eps)
    fd_y = (y_eps - y_out) / eps

    x = 1.0
    x_ad = 1.0
    call scale_and_shift_fwd_ad(x, x_ad, y, y_ad)
    if (abs((y_ad - fd_y) / fd_y) > tol) then
       print *, 'test_scale_and_shift_fwd failed', y_ad, fd_y
       error stop 1
    end if

    inner1 = y_ad**2
    x_ad = 0.0
    call scale_and_shift_rev_ad(x_ad, y_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_scale_and_shift_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_scale_and_shift

end program run_use_parameter
