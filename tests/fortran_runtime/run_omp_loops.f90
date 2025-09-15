program run_omp_loops
  use omp_loops
  use omp_loops_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_sum_loop = 1
  integer, parameter :: I_stencil_loop = 2
  integer, parameter :: I_omp_ws_if = 3

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
           case ("sum_loop")
              i_test = I_sum_loop
           case ("stencil_loop")
              i_test = I_stencil_loop
           case ("omp_ws_if")
              i_test = I_omp_ws_if
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_sum_loop .or. i_test == I_all) then
     call test_sum_loop
  end if
  if (i_test == I_stencil_loop .or. i_test == I_all) then
     call test_stencil_loop
  end if
  if (i_test == I_omp_ws_if .or. i_test == I_all) then
     call test_omp_ws_if
  end if

  stop
contains

  subroutine test_sum_loop
    integer, parameter :: n = 3
    real :: x(n), y(n), s
    real :: x_ad(n), y_ad(n), s_ad
    real :: y_eps(n), s_eps, fd_y(n), fd_s, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = (/1.0, 2.0, 3.0/)
    call sum_loop(n, x, y, s)
    call sum_loop(n, x + eps, y_eps, s_eps)
    fd_y(:) = (y_eps(:) - y(:)) / eps
    fd_s = (s_eps - s) / eps
    x_ad(:) = 1.0
    call sum_loop_fwd_ad(n, x, x_ad, y, y_ad, s, s_ad)
    if (any(abs((y_ad(:) - fd_y(:)) / fd_y(:)) > tol) .or. abs((s_ad - fd_s) / fd_s) > tol) then
       print *, 'test_sum_loop_fwd failed'
       error stop 1
    end if

    inner1 = sum(y_ad(:)**2) + s_ad**2
    x_ad(:) = 0.0
    call sum_loop_rev_ad(n, x_ad, y_ad, s_ad)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol) then
      print *, 'test_sum_loop_rev failed', inner1, inner2
      error stop 1
    end if

    return
  end subroutine test_sum_loop

  subroutine test_stencil_loop
    real, parameter :: tol_stencil = 2.0e-4
    integer, parameter :: n = 3
    real :: x(n), y(n)
    real :: x_ad(n), y_ad(n)
    real :: y_eps(n), fd_y(n), eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = (/1.0, 2.0, 3.0/)
    call stencil_loop(n, x, y)
    call stencil_loop(n, x + eps, y_eps)
    fd_y(:) = (y_eps(:) - y(:)) / eps
    x_ad(:) = 1.0
    call stencil_loop_fwd_ad(n, x, x_ad, y, y_ad)
    if (any(abs((y_ad(:) - fd_y(:)) / fd_y(:)) > tol_stencil)) then
       print *, 'test_stencil_loop_fwd failed'
       error stop 1
    end if

    inner1 = sum(y_ad(:)**2)
    x_ad(:) = 0.0
    call stencil_loop_rev_ad(n, x_ad, y_ad)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol_stencil) then
       print *, 'test_stencil_loop_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_stencil_loop

  subroutine test_omp_ws_if
    integer, parameter :: n = 3
    real, parameter :: tol_ws = 5.0e-4
    real :: x(n), y(n)
    real :: x_ad(n), y_ad(n)
    real :: y_eps(n), fd_y(n), eps
    real :: inner1, inner2
    logical :: f

    eps = 1.0e-3
    x = (/1.0, 2.0, 3.0/)

    ! Case 1: f = .false. => y = x
    f = .false.
    call omp_ws_if(x, y, f)
    call omp_ws_if(x + eps, y_eps, f)
    fd_y(:) = (y_eps(:) - y(:)) / eps
    x_ad(:) = 1.0
    call omp_ws_if_fwd_ad(x, x_ad, y, y_ad, f)
    if (any(abs((y_ad(:) - fd_y(:)) / max(1.0e-12, fd_y(:))) > tol_ws)) then
       print *, 'test_omp_ws_if_fwd (f=false) failed'
       error stop 1
    end if

    inner1 = sum(y_ad(:)**2)
    x_ad(:) = 0.0
    call omp_ws_if_rev_ad(x, x_ad, y_ad, f)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / max(1.0e-12, inner1)) > tol_ws) then
      print *, 'test_omp_ws_if_rev (f=false) failed', inner1, inner2
      error stop 1
    end if

    ! Case 2: f = .true. => y = x + x**2
    f = .true.
    call omp_ws_if(x, y, f)
    call omp_ws_if(x + eps, y_eps, f)
    fd_y(:) = (y_eps(:) - y(:)) / eps
    x_ad(:) = 1.0
    call omp_ws_if_fwd_ad(x, x_ad, y, y_ad, f)
    if (any(abs((y_ad(:) - fd_y(:)) / max(1.0e-12, fd_y(:))) > tol_ws)) then
       print *, 'test_omp_ws_if_fwd (f=true) failed'
       error stop 1
    end if

    inner1 = sum(y_ad(:)**2)
    x_ad(:) = 0.0
    call omp_ws_if_rev_ad(x, x_ad, y_ad, f)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / max(1.0e-12, inner1)) > tol_ws) then
      print *, 'test_omp_ws_if_rev (f=true) failed', inner1, inner2
      error stop 1
    end if

    return
  end subroutine test_omp_ws_if

end program run_omp_loops
