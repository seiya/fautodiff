program run_cross_mod
  use cross_mod_b_ad
  use cross_mod_a_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_call_inc = 1
  integer, parameter :: I_incval = 2
  integer, parameter :: I_call_inc_kw = 3
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
           case ("call_inc")
              i_test = I_call_inc
           case ("incval")
              i_test = I_incval
           case ("call_inc_kw")
              i_test = I_call_inc_kw
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_call_inc .or. i_test == I_all) then
     call test_call_inc
  end if
  if (i_test == I_call_inc_kw .or. i_test == I_all) then
     call test_call_inc_kw
  end if
  if (i_test == I_incval .or. i_test == I_all) then
     call test_incval
  end if

  stop
contains

  subroutine test_call_inc
    real :: x
    real :: x_ad
    real :: x_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 1.0
    call call_inc(x)
    x_eps = 1.0 + eps
    call call_inc(x_eps)
    fd = (x_eps - x) / eps
    x = 1.0
    x_ad = 1.0
    call call_inc_fwd_ad(x, x_ad)
    if (abs((x_ad - fd) / fd) > tol) then
       print *, 'test_call_inc_fwd failed', x_ad, fd
       error stop 1
    end if

    inner1 = x_ad**2
    x = 1.0
    call call_inc_rev_ad(x_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_call_inc_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_call_inc

  subroutine test_call_inc_kw
    real :: x
    real :: x_ad
    real :: x_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 1.0
    call call_inc_kw(x)
    x_eps = 1.0 + eps
    call call_inc_kw(x_eps)
    fd = (x_eps - x) / eps
    x = 1.0
    x_ad = 1.0
    call call_inc_kw_fwd_ad(x, x_ad)
    if (abs((x_ad - fd) / fd) > tol) then
       print *, 'test_call_inc_kw_fwd failed', x_ad, fd
       error stop 1
    end if

    inner1 = x_ad**2
    x = 1.0
    call call_inc_kw_rev_ad(x_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_call_inc_kw_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_call_inc_kw

  subroutine test_incval
    real :: a, a_ad
    real :: a_eps, fd, eps
    real :: inner1, inner2
    real :: inc_ad

    eps = 1.0e-3
    a = 1.0
    call incval(a, 1.0)
    a_eps = 1.0 + eps
    call incval(a_eps, 1.0)
    fd = (a_eps - a) / eps
    a = 1.0
    a_ad = 1.0
    call incval_fwd_ad(a, a_ad, 1.0, 0.0)
    if (abs((a_ad - fd) / fd) > tol) then
       print *, 'test_incval_fwd failed', a_ad, fd
       error stop 1
    end if

    inner1 = a_ad**2
    a = 1.0
    inc_ad = 0.0
    call incval_rev_ad(a_ad, inc_ad)
    inner2 = a_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_incval_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_incval

end program run_cross_mod
