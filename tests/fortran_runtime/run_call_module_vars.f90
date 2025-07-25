program run_call_module_vars
  use module_vars
  use module_vars_ad
  use call_module_vars
  use call_module_vars_ad
  implicit none
  real, parameter :: tol = 3.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_call_inc_and_use = 1

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
           case ("call_inc_and_use")
              i_test = I_call_inc_and_use
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_call_inc_and_use .or. i_test == I_all) then
     call test_call_inc_and_use
  end if

  stop
contains

  subroutine test_call_inc_and_use
    real :: x, y
    real :: x_ad, y_ad
    real :: y_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    a = 3.0
    a_ad = 0.0
    x = 2.0
    call call_inc_and_use(x, y)
    a = 3.0
    call call_inc_and_use(x + eps, y_eps)
    fd = (y_eps - y) / eps
    a = 3.0
    x_ad = 1.0
    call call_inc_and_use_fwd_ad(x, x_ad, y, y_ad)
    if (abs((y_ad - fd) / fd) > tol) then
       print *, 'test_call_inc_and_use_fwd failed', y_ad, fd
       error stop 1
    end if

    inner1 = y_ad**2 + a_ad**2
    a = 3.0
    x_ad = 0.0
    call call_inc_and_use_rev_ad(x, x_ad, y_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_call_inc_and_use_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_call_inc_and_use

end program run_call_module_vars
