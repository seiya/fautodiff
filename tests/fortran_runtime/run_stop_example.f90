program run_stop_example
  use stop_example
  use stop_example_ad
  implicit none
  real, parameter :: tol = 3.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_stop_sub = 1

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
           case ("stop_sub")
              i_test = I_stop_sub
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_stop_sub .or. i_test == I_all) then
     call test_stop_sub
  end if

  stop
contains

  subroutine test_stop_sub
    real :: x, y
    real :: x_ad, y_ad
    real :: y_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 1.0
    call stop_sub(x, y)
    call stop_sub(x + eps, y_eps)
    fd = (y_eps - y) / eps
    x_ad = 1.0
    call stop_sub_fwd_ad(x, x_ad, y, y_ad)
    if (abs((y_ad - fd) / fd) > tol) then
       print *, 'test_stop_sub_fwd failed', y_ad, fd
       error stop 1
    end if

    inner1 = y_ad**2
    x_ad = 0.0
    call stop_sub_rev_ad(x, x_ad, y_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_stop_sub_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_stop_sub

end program run_stop_example
