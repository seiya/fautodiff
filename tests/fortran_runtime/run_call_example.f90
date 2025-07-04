program run_call_example
  use call_example
  use call_example_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_call_subroutine = 1
  integer, parameter :: I_call_fucntion = 2
  integer, parameter :: I_arg_operation = 3
  integer, parameter :: I_arg_function = 4

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
           case ("call_subroutine")
              i_test = I_call_subroutine
           case ("call_fucntion")
              i_test = I_call_fucntion
           case ("arg_operation")
              i_test = I_arg_operation
           case ("arg_function")
              i_test = I_arg_function
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_call_subroutine .or. i_test == I_all) then
     call test_call_subroutine
  end if
  if (i_test == I_call_fucntion .or. i_test == I_all) then
     call test_call_fucntion
  end if
  if (i_test == I_arg_operation .or. i_test == I_all) then
     call test_arg_operation
  end if
  if (i_test == I_arg_function .or. i_test == I_all) then
     call test_arg_function
  end if

  stop
contains

  subroutine test_call_subroutine
    real :: x, y
    real :: x_ad, y_ad
    real :: fd, eps, x_eps, y_eps
    real :: exp_x, exp_x_ad, exp_y_ad

    eps = 1.0e-3
    x = 1.0
    y = 2.0
    call call_subroutine(x, y)
    x_eps = 1.0 + eps
    y_eps = 2.0 + eps
    call call_subroutine(x_eps, y_eps)
    fd = (x_eps - x) / eps
    x_ad = 1.0
    y_ad = 1.0
    call call_subroutine_fwd_ad(x, x_ad, y, y_ad)
    if (abs((x_ad - fd) / fd) > tol) then
       print *, 'test_call_subroutine_fwd failed', x_ad, fd
       error stop 1
    end if

    x = 1.0
    y = 2.0
    call call_subroutine(x, y)
    x_ad = 1.0
    y_ad = 0.0
    call call_subroutine_rev_ad(x, x_ad, y, y_ad)
    exp_x = 4.0
    exp_x_ad = 2.0
    exp_y_ad = 1.0
    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol .or. &
        abs(y_ad - exp_y_ad) > tol) then
       print *, 'test_call_subroutine failed', x, x_ad, y_ad
       error stop 1
    end if

    return
  end subroutine test_call_subroutine

  subroutine test_call_fucntion
    real :: x, y
    real :: x_ad, y_ad
    real :: x_eps, res_fd, eps
    real :: exp_x, exp_x_ad, exp_y_ad

    eps = 1.0e-3
    y = 3.0
    call call_fucntion(x, y)
    x_eps = 0.0
    call call_fucntion(x_eps, y + eps)
    res_fd = (x_eps - x) / eps
    y_ad = 1.0
    call call_fucntion_fwd_ad(x_ad, y, y_ad)
    if (abs((x_ad - res_fd) / res_fd) > tol) then
       print *, 'test_call_fucntion_fwd failed', x_ad, res_fd
       error stop 1
    end if

    y = 3.0
    call call_fucntion(x, y)
    x_ad = 1.0
    y_ad = 0.0
    call call_fucntion_rev_ad(x_ad, y, y_ad)
    exp_x = y**2
    exp_x_ad = 0.0
    exp_y_ad = 2.0 * y
    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol .or. &
        abs(y_ad - exp_y_ad) > tol) then
       print *, 'test_call_fucntion failed', x, x_ad, y_ad
       error stop 1
    end if

    return
  end subroutine test_call_fucntion

  subroutine test_arg_operation
    real :: x, y
    real :: x_ad, y_ad
    real :: x_eps, y_eps, fd, eps
    real :: exp_x, exp_x_ad, exp_y_ad

    eps = 1.0e-3
    x = 1.0
    y = 2.0
    call arg_operation(x, y)
    x_eps = 1.0 + eps
    y_eps = 2.0 + eps
    call arg_operation(x_eps, y_eps)
    fd = (x_eps - x) / eps
    x = 1.0
    y = 2.0
    x_ad = 1.0
    call arg_operation_fwd_ad(x, x_ad, y, 1.0)
    if (abs((x_ad - fd) / fd) > tol) then
       print *, 'test_arg_operation_fwd failed', x_ad, fd
       error stop 1
    end if

    x = 1.0
    y = 2.0
    call arg_operation(x, y)
    x_ad = 1.0
    y_ad = 0.0
    call arg_operation_rev_ad(x, x_ad, y, y_ad)
    exp_x = 6.0
    exp_x_ad = 2.0
    exp_y_ad = 2.0
    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol .or. &
        abs(y_ad - exp_y_ad) > tol) then
       print *, 'test_arg_operation failed', x, x_ad, y_ad
       error stop 1
    end if

    return
  end subroutine test_arg_operation

  subroutine test_arg_function
    real :: x, y
    real :: x_ad, y_ad
    real :: x_eps, y_eps, fd, eps
    real :: exp_x, exp_x_ad, exp_y_ad

    eps = 1.0e-3
    x = 1.0
    y = 2.0
    call arg_function(x, y)
    x_eps = 1.0 + eps
    y_eps = 2.0 + eps
    call arg_function(x_eps, y_eps)
    fd = (x_eps - x) / eps
    x = 1.0
    y = 2.0
    x_ad = 1.0
    call arg_function_fwd_ad(x, x_ad, y, 1.0)
    if (abs((x_ad - fd) / fd) > tol) then
       print *, 'test_arg_function_fwd failed', x_ad, fd
       error stop 1
    end if

    x = 1.0
    y = 2.0
    call arg_function(x, y)
    x_ad = 1.0
    y_ad = 0.0
    call arg_function_rev_ad(x, x_ad, y, y_ad)
    exp_x = 6.0
    exp_x_ad = 2.0
    exp_y_ad = 4.0
    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol .or. &
        abs(y_ad - exp_y_ad) > tol) then
       print *, 'test_arg_function failed', x, x_ad, y_ad
       error stop 1
    end if
    return
  end subroutine test_arg_function

end program run_call_example
