program run_cross_mod
  use cross_mod_b
  use cross_mod_b_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_call_inc_fwd = 1
  integer, parameter :: I_call_inc_rev = 2
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
           case ("call_inc_fwd")
              i_test = I_call_inc_fwd
           case ("call_inc_rev")
              i_test = I_call_inc_rev
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_call_inc_fwd) then
     call test_call_inc_fwd
  end if
  if (i_test == I_call_inc_rev .or. i_test == I_all) then
     call test_call_inc_rev
  end if

  stop
contains

  subroutine test_call_inc_fwd
    real :: x, x_eps, x_ad, fd, eps

    eps = 1.0e-6
    x = 1.0
    call call_inc(x)
    x_eps = 1.0 + eps
    call call_inc(x_eps)
    fd = (x_eps - x) / eps
    x = 1.0
    x_ad = 1.0
    call call_inc_fwd_ad(x, x_ad)
    if (abs(x_ad - fd) > tol) then
       print *, 'test_call_inc_fwd failed', x_ad, fd
       error stop 1
    end if
    return
  end subroutine test_call_inc_fwd

  subroutine test_call_inc_rev
    real :: x
    real :: x_ad
    real :: exp_x, exp_x_ad

    x = 1.0
    call call_inc(x)

    x_ad = 1.0
    call call_inc_rev_ad(x, x_ad)

    exp_x = 2.0
    exp_x_ad = 1.0

    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol) then
       print *, 'test_call_inc failed', x, x_ad
       error stop 1
    end if
    return
  end subroutine test_call_inc_rev

end program run_cross_mod
