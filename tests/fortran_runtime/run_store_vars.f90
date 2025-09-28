program run_store_vars
  use store_vars
  use store_vars_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_do_with_recurrent_scalar = 1
  integer, parameter :: I_do_while = 2
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
           case ("do_with_recurrent_scalar")
              i_test = I_do_with_recurrent_scalar
           case ("do_while")
              i_test = I_do_while
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_do_with_recurrent_scalar .or. i_test == I_all) then
     call test_do_with_recurrent_scalar
  end if
  if (i_test == I_do_while .or. i_test == I_all) then
     call test_do_while
  end if

  stop
contains

  subroutine test_do_with_recurrent_scalar
    integer, parameter :: n = 3
    real, parameter :: tol = 2e-3
    real :: x(n), z(n)
    real :: x_ad(n), z_ad(n)
    real :: z_eps(n), fd(n), eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = (/2.0, 3.0, 4.0/)
    call do_with_recurrent_scalar(n, x, z)
    call do_with_recurrent_scalar(n, x + eps, z_eps)
    fd(:) = (z_eps(:) - z(:)) / eps
    x_ad(:) = 1.0
    call do_with_recurrent_scalar_fwd_ad(n, x, x_ad, z, z_ad)
    if (maxval(abs((z_ad(:) - fd(:)) / fd(:))) > tol) then
       print *, 'test_do_with_recurrent_scalar_fwd failed'
       print *, maxval(abs((z_ad(:) - fd(:)) / fd(:)))
       print *, z_ad(:)
       print *, fd(:)
       error stop 1
    end if

    inner1 = sum(z_ad(:)**2)
    x_ad = 0.0
    call do_with_recurrent_scalar_rev_ad(n, x, x_ad, z_ad)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_do_with_recurrent_scalar_rev failed', inner1, inner2
       error stop 1
    end if

    return
end subroutine test_do_with_recurrent_scalar

  subroutine test_do_while
    real, parameter :: eps = 1.0e-3
    real, parameter :: tol_loc = 3e-3
    real :: x
    real :: y, z, y_eps, z_eps
    real :: x_ad, y_ad, z_ad
    real :: fd_y, fd_z
    real :: inner1, inner2

    x = 0.8
    call do_while(x, y, z)
    call do_while(x + eps, y_eps, z_eps)
    fd_y = (y_eps - y) / eps
    fd_z = (z_eps - z) / eps

    x = 0.8
    x_ad = 1.0
    call do_while_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    if (abs((y_ad - fd_y) / max(tol_loc, abs(fd_y))) > tol_loc .or. &
        abs((z_ad - fd_z) / max(tol_loc, abs(fd_z))) > tol_loc) then
       print *, 'test_do_while_fwd failed'
       print *, y_ad, fd_y, z_ad, fd_z
       error stop 1
    end if

    inner1 = y_ad**2 + z_ad**2
    x_ad = 0.0
    call do_while_rev_ad(x, x_ad, y_ad, z_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / max(tol_loc, abs(inner1))) > tol_loc) then
       print *, 'test_do_while_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_do_while

end program run_store_vars
