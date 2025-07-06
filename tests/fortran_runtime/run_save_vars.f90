program run_save_vars
  use save_vars
  use save_vars_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_simple = 1
  integer, parameter :: I_if_example = 2
  integer, parameter :: I_array_private = 3
  integer, parameter :: I_array = 4
  integer, parameter :: I_local_array = 5
  integer, parameter :: I_stencil_array = 6

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
           case ("if_example")
              i_test = I_if_example
           case ("array_private")
              i_test = I_array_private
           case ("array")
              i_test = I_array
           case ("local_array")
              i_test = I_local_array
           case ("stencil_array")
              i_test = I_stencil_array
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
  if (i_test == I_if_example .or. i_test == I_all) then
     call test_if_example
  end if
  if (i_test == I_array_private .or. i_test == I_all) then
     call test_array_private
  end if
  if (i_test == I_array .or. i_test == I_all) then
     call test_array
  end if
  if (i_test == I_local_array .or. i_test == I_all) then
     call test_local_array
  end if
  if (i_test == I_stencil_array .or. i_test == I_all) then
     call test_stencil_array
  end if

  stop
contains

  subroutine test_simple
  real, parameter :: tol = 1.0e-3
    real :: x, y, z
    real :: x_ad, y_ad, z_ad
    real :: z_eps, fd, eps
    real :: inner1, inner2

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

    inner1 = z_ad**2
    x_ad = 0.0
    y_ad = 0.0
    call simple_rev_ad(x, x_ad, y, y_ad, z_ad)
    inner2 = x_ad + y_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_simple_rev failed', inner1, inner2

       error stop 1
    end if

    return
  end subroutine test_simple

  subroutine test_if_example
    real, parameter :: tol = 8e-4
    real :: x, y, z, z_eps
    real :: x_ad, y_ad, z_ad
    real :: fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 1.0
    y = 2.0
    call if_example(x, y, z)
    call if_example(x + eps, y + eps, z_eps)
    fd = (z_eps - z) / eps
    x_ad = 1.0
    y_ad = 1.0
    call if_example_fwd_ad(x, x_ad, y, y_ad, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_if_example_fwd failed', z_ad, fd
       error stop 1
    end if

    inner1 = z_ad**2
    call if_example_rev_ad(x, x_ad, y, y_ad, z_ad)
    inner2 = x_ad + y_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_if_example_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_if_example

  subroutine test_array_private
    real, parameter :: tol = 4e-3
    integer, parameter :: n = 2, m = 2
    real :: x(n,m), y(n,m), z(n,m), z_eps(n,m), z_ad(n,m)
    real :: x_ad(n,m), y_ad(n,m), fd(n,m)
    real :: eps, inner1, inner2

    eps = 1.0e-3
    x = 1.0
    y = 2.0
    call do_with_array_private(n, m, x, y, z)
    call do_with_array_private(n, m, x + eps, y + eps, z_eps)
    fd(:,:) = (z_eps(:,:) - z(:,:)) / eps
    x_ad = 1.0
    y_ad = 1.0
    call do_with_array_private_fwd_ad(n, m, x, x_ad, y, y_ad, z_ad)
    if (any(abs((z_ad(:,:) - fd(:,:)) / fd(:,:)) > tol)) then
       print *, 'test_array_private_fwd failed'
       print *, maxval(abs((z_ad(:,:) - fd(:,:)) / fd(:,:)))
       error stop 1
    end if

    inner1 = sum(z_ad(:,:)**2)
    call do_with_array_private_rev_ad(n, m, x, x_ad, y, y_ad, z_ad)
    inner2 = sum(x_ad(:,:)) + sum(y_ad(:,:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_array_private_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_array_private

  subroutine test_array
    real, parameter :: tol = 7e-4
    integer, parameter :: n = 2, m = 2
    real :: x(n,m), y(n,m), z(n,m), z_eps(n,m), z_ad(n,m)
    real :: x_ad(n,m), y_ad(n,m), fd(n,m)
    real :: eps, inner1, inner2

    eps = 1.0e-3
    x = 1.0
    y = 2.0
    call do_with_array(n, m, x, y, z)
    call do_with_array(n, m, x + eps, y + eps, z_eps)
    fd(:,:) = (z_eps(:,:) - z(:,:)) / eps
    x_ad = 1.0
    y_ad = 1.0
    call do_with_array_fwd_ad(n, m, x, x_ad, y, y_ad, z_ad)
    if (maxval(abs((z_ad(:,:) - fd(:,:)) / fd(:,:))) > tol) then
       print *, 'test_array_fwd failed'
       print *, maxval(abs((z_ad(:,:) - fd(:,:)) / fd(:,:)))
       error stop 1
    end if

    inner1 = sum(z_ad(:,:)**2)
    call do_with_array_rev_ad(n, m, x, x_ad, y, y_ad, z_ad)
    inner2 = sum(x_ad(:,:)) + sum(y_ad(:,:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_array_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_array

  subroutine test_local_array
    real, parameter :: tol = 1.5e-3
    integer, parameter :: n = 2, m = 2
    real :: x(n,m), y(n,m), z(n,m), z_eps(n,m), z_ad(n,m)
    real :: x_ad(n,m), y_ad(n,m), fd(n,m)
    real :: eps, inner1, inner2

    eps = 1.0e-3
    x = 1.0
    y = 2.0
    call do_with_local_array(n, m, x, y, z)
    call do_with_local_array(n, m, x + eps, y + eps, z_eps)
    fd(:,:) = (z_eps(:,:) - z(:,:)) / eps
    x_ad = 1.0
    y_ad = 1.0
    call do_with_local_array_fwd_ad(n, m, x, x_ad, y, y_ad, z_ad)
    if (maxval(abs((z_ad(:,:) - fd(:,:)) / fd(:,:))) > tol) then
       print *, 'test_local_array_fwd failed'
       print *, maxval(abs((z_ad(:,:) - fd(:,:)) / fd(:,:)))
       error stop 1
    end if

    inner1 = sum(z_ad(:,:)**2)
    call do_with_local_array_rev_ad(n, m, x, x_ad, y, y_ad, z_ad)
    inner2 = sum(x_ad(:,:)) + sum(y_ad(:,:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_local_array_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_local_array

  subroutine test_stencil_array
    real, parameter :: tol = 2.0e-3
    integer, parameter :: n = 3
    real :: x(n), x_base(n), x_eps(n)
    real :: x_ad(n), fd(n), eps
    real :: inner1, inner2

    eps = 1.0e-3
    x_base = (/1.0, 2.0, 3.0/)
    x = x_base
    call do_with_stencil_array(n, x)
    x_eps = x_base + eps
    call do_with_stencil_array(n, x_eps)
    fd(:) = (x_eps(:) - x(:)) / eps
    x = x_base
    x_ad = 1.0
    call do_with_stencil_array_fwd_ad(n, x, x_ad)
    if (maxval(abs((x_ad(:) - fd(:)) / fd(:))) > tol) then
       print *, 'test_stencil_array_fwd failed'
       print *, maxval(abs((x_ad(:) - fd(:)) / fd(:)))
       error stop 1
    end if

    inner1 = sum(x_ad(:)**2)
    x = x_base
    call do_with_stencil_array_rev_ad(n, x, x_ad)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_stencil_array_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_stencil_array

end program run_save_vars
