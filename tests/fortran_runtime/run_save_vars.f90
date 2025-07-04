program run_save_vars
  use save_vars
  use save_vars_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_simple = 1

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
           case ("simple")
              i_test = I_simple
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_simple .or. i_test == I_all) then
     call test_simple
  end if

  stop
contains

  subroutine test_simple
  real, parameter :: tol = 1.0e-3
    real :: x, y, z
    real :: x_ad, y_ad, z_ad
    real :: z_eps, fd, eps
    real :: exp_z, exp_x, exp_y

    eps = 1.0e-3
    x = 2.0
    y = 3.0
    call simple(x, y, z)
    call simple(x + eps, y + eps, z_eps)
    fd = (z_eps - z) / eps
    x_ad = 1.0
    y_ad = 1.0
    call simple_fwd_ad(x, x_ad, y, y_ad, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_simple_fwd failed', z_ad, fd
       error stop 1
    end if

    x = 2.0
    y = 3.0
    call simple(x, y, z)
    x_ad = 0.0
    y_ad = 0.0
    z_ad = 1.0
    call simple_rev_ad(x, x_ad, y, y_ad, z_ad)
    exp_z = 2.0*x**3 + 2.0*x**2 + 2.0*x + (1.0 + y)
    exp_x = 6.0*x**2 + 4.0*x + 2.0
    exp_y = 1.0
    if (abs(z - exp_z) > tol .or. abs(x_ad - exp_x) > tol .or. &
        abs(y_ad - exp_y) > tol) then
       print *, 'test_simple failed', z, x_ad, y_ad
       error stop 1
    end if

    return
  end subroutine test_simple

end program run_save_vars
