program run_control_flow
  use control_flow
  use control_flow_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_if_example = 1
  integer, parameter :: I_do_example = 2
  integer, parameter :: I_select_example = 3
  integer, parameter :: I_do_while_example = 4

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
           case ("if_example")
              i_test = I_if_example
           case ("do_example")
              i_test = I_do_example
           case ("select_example")
              i_test = I_select_example
           case ("do_while_example")
              i_test = I_do_while_example
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_if_example .or. i_test == I_all) then
     call test_if_example
  end if
  if (i_test == I_do_example .or. i_test == I_all) then
     call test_do_example
  end if
  if (i_test == I_select_example .or. i_test == I_all) then
     call test_select_example
  end if
  if (i_test == I_do_while_example .or. i_test == I_all) then
     call test_do_while_example
  end if

  stop
contains

  subroutine test_if_example
    real :: x, y, z
    real :: x_ad, y_ad, z_ad
    real :: y_eps, z_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 1.0
    y = 2.0
    call if_example(x, y, z)
    y_eps = y + eps
    call if_example(x + eps, y_eps, z_eps)
    fd = (z_eps - z) / eps
    x_ad = 1.0
    y_ad = 1.0
    call if_example_fwd_ad(x, x_ad, y, y_ad, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_if_example_fwd failed', z_ad, fd
       error stop 1
    end if

    inner1 = z_ad**2
    x_ad = 0.0
    y_ad = 0.0
    call if_example_rev_ad(x, x_ad, y, y_ad, z_ad)
    inner2 = x_ad + y_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_if_example_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_if_example

  subroutine test_do_example
    integer :: n
    real :: x, sum
    real :: x_ad, sum_ad
    real :: sum_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    n = 3
    x = 2.0
    call do_example(n, x, sum)
    call do_example(n, x + eps, sum_eps)
    fd = (sum_eps - sum) / eps
    x_ad = 1.0
    call do_example_fwd_ad(n, x, x_ad, sum_ad)
    if (abs((sum_ad - fd) / fd) > tol) then
       print *, 'test_do_example_fwd failed', sum_ad, fd
       error stop 1
    end if

    inner1 = sum_ad**2
    x_ad = 0.0
    call do_example_rev_ad(n, x, x_ad, sum_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_do_example_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_do_example

  subroutine test_select_example
    integer :: i
    real :: x, z, z_eps, z_ad, fd, eps
    real :: x_ad
    real :: inner1, inner2

    eps = 1.0e-3

    ! Case 1
    i = 1
    x = 2.0
    call select_example(i, x, z)
    call select_example(i, x + eps, z_eps)
    fd = (z_eps - z) / eps
    call select_example_fwd_ad(i, x, 1.0, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_select_example_fwd failed case1', z_ad, fd
       error stop 1
    end if
    inner1 = z_ad**2
    call select_example_rev_ad(i, x, x_ad, z_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_select_example_rev failed case1', inner1, inner2
       error stop 1
    end if

    ! Default case
    i = 4
    call select_example(i, x, z)
    call select_example(i, x + eps, z_eps)
    fd = (z_eps - z) / eps
    call select_example_fwd_ad(i, x, 1.0, z_ad)
    if (abs(z_ad - fd) > tol) then
       print *, 'test_select_example_fwd failed default', z_ad, fd
       error stop 1
    end if
    inner1 = z_ad**2
    call select_example_rev_ad(i, x, x_ad, z_ad)
    inner2 = x_ad
    if (abs(inner2 - inner1) > tol) then
       print *, 'test_select_example_rev failed default', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_select_example

  subroutine test_do_while_example
    real :: x, limit, eps
    integer :: count, count_eps
    real :: fd
    real :: x_ad, limit_ad
    real :: inner1, inner2

    eps = 1.0e-3
    x = 0.5
    limit = 1.0
    call do_while_example(x, limit, count)
    call do_while_example(x + eps, limit + eps, count_eps)
    fd = real(count_eps - count) / eps
    if (abs(fd) > tol) then
       print *, 'test_do_while_example_fwd failed', fd
       error stop 1
    end if

    inner1 = 0.0
    x_ad = 0.0
    limit_ad = 0.0
    call do_while_example_rev_ad(x, x_ad, limit, limit_ad)
    inner2 = x_ad + limit_ad
    if (abs(inner2 - inner1) > tol) then
       print *, 'test_do_while_example_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_do_while_example

end program run_control_flow
