program run_keyword_args
  use keyword_args_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_inc = 1
  integer, parameter :: I_do_inc = 2

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
           case ("inc")
              i_test = I_inc
           case ("do_inc")
              i_test = I_do_inc
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_inc .or. i_test == I_all) then
     call test_inc
  end if
  if (i_test == I_do_inc .or. i_test == I_all) then
     call test_do_inc
  end if

  stop
contains

  subroutine test_inc
    real, parameter :: eps = 1.0e-3
    real :: a, b, a_eps
    real :: a_ad, b_ad
    real :: fd, inner1, inner2
    real :: a_seed, b_seed, a_bar, b_bar

    a = 1.5
    b = 0.5
    call inc(a, b)
    a_eps = 1.5 + eps
    b = 0.5 + eps
    call inc(a_eps, b)
    fd = (a_eps - a) / eps

    a = 1.5
    b = 0.5
    a_seed = 1.0
    b_seed = 1.0
    a_ad = a_seed
    b_ad = b_seed
    call inc_fwd_ad(a, a_ad, b, b_ad)
    if (abs((a_ad - fd) / max(abs(fd), tol)) > tol) then
       print *, 'test_inc_fwd failed', a_ad, fd
       error stop 1
    end if

    inner1 = a_ad**2
    a_bar = a_ad
    a_ad = a_bar
    b_bar = 0.0
    call inc_rev_ad(a_ad, b_bar)
    inner2 = a_seed * a_ad + b_seed * b_bar
    if (abs((inner2 - inner1) / max(abs(inner1), tol)) > tol) then
       print *, 'test_inc_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_inc

  subroutine test_do_inc
    real, parameter :: eps = 1.0e-3
    real :: x, y, x_eps
    real :: x_ad, y_ad
    real :: fd, inner1, inner2
    real :: x_seed, y_seed, x_bar, y_bar

    x = 1.0
    y = 0.2
    call do_inc(x, y)
    x_eps = 1.0 + eps
    y = 0.2 + eps
    call do_inc(x_eps, y)
    fd = (x_eps - x) / eps

    x = 1.0
    y = 0.2
    x_seed = 1.0
    y_seed = 1.0
    x_ad = x_seed
    y_ad = y_seed
    call do_inc_fwd_ad(x, x_ad, y, y_ad)
    if (abs((x_ad - fd) / max(abs(fd), tol)) > tol) then
       print *, 'test_do_inc_fwd failed', x_ad, fd
       error stop 1
    end if

    inner1 = x_ad**2
    x_bar = x_ad
    x_ad = x_bar
    y_bar = 0.0
    call do_inc_rev_ad(x_ad, y_bar)
    inner2 = x_seed * x_ad + y_seed * y_bar
    if (abs((inner2 - inner1) / max(abs(inner1), tol)) > tol) then
       print *, 'test_do_inc_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_do_inc

end program run_keyword_args
