program run_store_vars
  use store_vars
  use store_vars_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_do_with_recurrent_scalar_fwd = 1
  integer, parameter :: I_do_with_recurrent_scalar_rev = 2
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
           case ("do_with_recurrent_scalar_fwd")
              i_test = I_do_with_recurrent_scalar_fwd
           case ("do_with_recurrent_scalar_rev")
              i_test = I_do_with_recurrent_scalar_rev
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_do_with_recurrent_scalar_fwd .or. i_test == I_all) then
     call test_do_with_recurrent_scalar_fwd
  end if
  if (i_test == I_do_with_recurrent_scalar_rev .or. i_test == I_all) then
     call test_do_with_recurrent_scalar_rev
  end if

  stop
contains

  subroutine test_do_with_recurrent_scalar_fwd
    integer, parameter :: n = 3
    real, parameter :: tol = 3e-4
    real :: x(n), z(n)
    real :: x_ad(n), z_ad(n)
    real :: z_eps(n), fd(n), eps

    eps = 1.0e-3
    x = (/2.0, 3.0, 4.0/)
    call do_with_recurrent_scalar(n, x, z)
    call do_with_recurrent_scalar(n, x + eps, z_eps)
    fd(:) = (z_eps(:) - z(:)) / eps
    x_ad(:) = 1.0
    call do_with_recurrent_scalar_fwd_ad(n, x, x_ad, z_ad)
    if (maxval(abs((z_ad(:) - fd(:)) / fd(:))) > tol) then
       print *, 'test_do_with_recurrent_scalar_fwd failed'
       print *, maxval(abs((z_ad(:) - fd(:)) / fd(:)))
       print *, z_ad(:)
       print *, fd(:)
       error stop 1
    end if
    return
  end subroutine test_do_with_recurrent_scalar_fwd

  subroutine test_do_with_recurrent_scalar_rev
    integer, parameter :: n = 3
    real :: x(n), z(n)
    real :: x_ad(n), z_ad(n)
    real :: exp_z, exp_x1, exp_x2, exp_x3

    x = (/2.0, 3.0, 4.0/)
    call do_with_recurrent_scalar(n, x, z)

    x_ad = 0.0
    z_ad = 0.0
    z_ad(n) = 1.0
    call do_with_recurrent_scalar_rev_ad(n, x, x_ad, z_ad)

    exp_z = x(1) * x(2) * x(3)
    exp_x1 = x(2) * x(3)
    exp_x2 = x(1) * x(3)
    exp_x3 = x(1) * x(2)

    if (abs(z(n) - exp_z) > tol .or. abs(x_ad(1) - exp_x1) > tol .or. &
        abs(x_ad(2) - exp_x2) > tol .or. abs(x_ad(3) - exp_x3) > tol) then
       print *, 'test_do_with_recurrent_scalar failed', z(n), x_ad
       error stop 1
    end if
    return
  end subroutine test_do_with_recurrent_scalar_rev

end program run_store_vars
