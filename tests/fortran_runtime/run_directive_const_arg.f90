program run_directive_const_arg
  use directive_const_arg
  use directive_const_arg_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_add_const_fwd = 1

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
           case ("add_const_fwd")
              i_test = I_add_const_fwd
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_add_const_fwd .or. i_test == I_all) then
     call test_add_const_fwd
  end if

  stop
contains

  subroutine test_add_const_fwd
    real :: x, y, y_eps, y_ad, fd, eps

    eps = 1.0e-6
    x = 2.0
    call add_const(x, y, 3.0)
    call add_const(x + eps, y_eps, 3.0)
    fd = (y_eps - y) / eps
    call add_const_fwd_ad(x, 1.0, y_ad, 3.0)
    if (abs(y_ad - fd) > tol) then
       print *, 'test_add_const_fwd failed', y_ad, fd
       error stop 1
    end if
    return
  end subroutine test_add_const_fwd

end program run_directive_const_arg
