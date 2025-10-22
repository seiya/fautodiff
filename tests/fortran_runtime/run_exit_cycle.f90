program run_exit_cycle
  use exit_cycle_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_do_exit_cycle = 1
  integer, parameter :: I_while_exit_cycle = 2
  integer, parameter :: I_exit_cycle_with_labels = 3

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
           case ("do_exit_cycle")
              i_test = I_do_exit_cycle
           case ("while_exit_cycle")
              i_test = I_while_exit_cycle
           case ("exit_cycle_with_labels")
              i_test = I_exit_cycle_with_labels
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_do_exit_cycle .or. i_test == I_all) then
     call test_do_exit_cycle
  end if
  if (i_test == I_while_exit_cycle .or. i_test == I_all) then
     call test_while_exit_cycle
  end if
  if (i_test == I_exit_cycle_with_labels .or. i_test == I_all) then
     call test_exit_cycle_with_labels
  end if

  stop
contains

  subroutine test_do_exit_cycle
    real, parameter :: tol = 4e-3
    integer :: n
    real :: x, res, res_eps
    real :: x_ad, res_ad
    real :: fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    n = 5
    x = 2.0
    call do_exit_cycle(n, x, res)
    call do_exit_cycle(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    call do_exit_cycle_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_do_exit_cycle_fwd failed', res_ad, fd
       error stop 1
    end if

    inner1 = res_ad**2
    call do_exit_cycle_rev_ad(n, x, x_ad, res_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
      print *, 'test_do_exit_cycle_rev failed', inner1, inner2
      error stop 1
    end if

    return
  end subroutine test_do_exit_cycle

  subroutine test_while_exit_cycle
    real, parameter :: tol = 4e-3
    integer :: n
    real :: x, res, res_eps
    real :: x_ad, res_ad
    real :: fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    n = 5
    x = 2.0
    call while_exit_cycle(n, x, res)
    call while_exit_cycle(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    call while_exit_cycle_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_while_exit_cycle_fwd failed', res_ad, fd
       error stop 1
    end if

    inner1 = res_ad**2
    call while_exit_cycle_rev_ad(n, x, x_ad, res_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
      print *, 'test_while_exit_cycle_rev failed', inner1, inner2
      error stop 1
    end if

    return
  end subroutine test_while_exit_cycle

  subroutine test_exit_cycle_with_labels
    real, parameter :: tol = 4e-4
    integer :: n
    real :: x, res, res_eps
    real :: x_ad, res_ad
    real :: fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    n = 4
    x = 2.0
    call exit_cycle_with_labels(n, x, res)
    call exit_cycle_with_labels(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    call exit_cycle_with_labels_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_exit_cycle_with_labels_fwd failed', res_ad, fd
       error stop 1
    end if

    inner1 = res_ad**2
    call exit_cycle_with_labels_rev_ad(n, x, x_ad, res_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
      print *, 'test_exit_cycle_with_labels_rev failed', inner1, inner2
      error stop 1
    end if

    return
  end subroutine test_exit_cycle_with_labels

end program run_exit_cycle
