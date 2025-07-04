program run_directive_const_arg
  use directive_const_arg
  use directive_const_arg_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_add_const = 1

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

  stop
contains

  subroutine test_add_const
    real :: x, y, z
    real :: x_ad, y_ad
    real :: y_eps, fd, eps

    eps = 1.0e-6
    x = 2.0
    z = 3.0
    call add_const(x, y, z)
    call add_const(x + eps, y_eps, z)
    fd = (y_eps - y) / eps
    x_ad = 1.0
    call add_const_fwd_ad(x, x_ad, y_ad, z)
    if (abs(y_ad - fd) > tol) then
       print *, 'test_add_const_fwd failed', y_ad, fd
       error stop 1
    end if

    return
  end subroutine test_add_const

end program run_directive_const_arg
