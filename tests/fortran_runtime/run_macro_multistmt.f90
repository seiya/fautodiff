program run_macro_multistmt
  use macro_multistmt_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_foo = 1

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

  if (i_test == I_foo .or. i_test == I_all) then
     call test_foo
  end if

  stop
contains

  subroutine test_foo
    real :: x, y
    real :: y_ad
    real :: y_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 1.0
    y = 3.0
    call foo(x, y)
    x = 1.0
    y_eps = 3.0 + eps
    call foo(x, y_eps)
    fd = (y_eps - y) / eps

    x = 1.0
    y = 3.0
    y_ad = 1.0
    call foo_fwd_ad(x, y, y_ad)
    if (abs((y_ad - fd) / fd) > tol) then
       print *, 'test_foo_fwd failed', y_ad, fd
       error stop 1
    end if

    inner1 = y_ad**2
    call foo_rev_ad(y_ad)
    inner2 = y_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_foo_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_foo

end program run_macro_multistmt
