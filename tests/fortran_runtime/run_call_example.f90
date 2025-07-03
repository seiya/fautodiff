program run_call_example
  use call_example
  use call_example_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_call_subroutine_rev = 1
  integer, parameter :: I_call_fucntion_rev = 2
  integer, parameter :: I_arg_operation_rev = 3
  integer, parameter :: I_arg_function_rev = 4
  integer, parameter :: I_call_subroutine_fwd = 5
  integer, parameter :: I_call_fucntion_fwd = 6
  integer, parameter :: I_arg_operation_fwd = 7
  integer, parameter :: I_arg_function_fwd = 8

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
           case ("call_subroutine_rev")
              i_test = I_call_subroutine_rev
           case ("call_fucntion_rev")
              i_test = I_call_fucntion_rev
           case ("arg_operation_rev")
              i_test = I_arg_operation_rev
           case ("arg_function_rev")
              i_test = I_arg_function_rev
           case ("call_subroutine_fwd")
              i_test = I_call_subroutine_fwd
           case ("call_fucntion_fwd")
              i_test = I_call_fucntion_fwd
           case ("arg_operation_fwd")
              i_test = I_arg_operation_fwd
           case ("arg_function_fwd")
              i_test = I_arg_function_fwd
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_call_subroutine_rev .or. i_test == I_all) then
     call test_call_subroutine_rev
  end if
  if (i_test == I_call_fucntion_rev .or. i_test == I_all) then
     call test_call_fucntion_rev
  end if
  if (i_test == I_arg_operation_rev .or. i_test == I_all) then
     call test_arg_operation_rev
  end if
  if (i_test == I_arg_function_rev .or. i_test == I_all) then
     call test_arg_function_rev
  end if
  if (i_test == I_call_subroutine_fwd) then
     call test_call_subroutine_fwd
  end if
  if (i_test == I_call_fucntion_fwd) then
     call test_call_fucntion_fwd
  end if
  if (i_test == I_arg_operation_fwd) then
     call test_arg_operation_fwd
  end if
  if (i_test == I_arg_function_fwd) then
     call test_arg_function_fwd
  end if

  stop
contains

  subroutine test_call_subroutine_rev
    real :: x, y
    real :: x_ad, y_ad
    real :: exp_x, exp_x_ad, exp_y_ad

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
  end subroutine test_call_subroutine_rev

  subroutine test_call_fucntion_rev
    real :: x, y
    real :: x_ad, y_ad
    real :: exp_x, exp_x_ad, exp_y_ad

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
  end subroutine test_call_fucntion_rev

  subroutine test_arg_operation_rev
    real :: x, y
    real :: x_ad, y_ad
    real :: exp_x, exp_x_ad, exp_y_ad

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
  end subroutine test_arg_operation_rev

  subroutine test_arg_function_rev
    real :: x, y
    real :: x_ad, y_ad
    real :: exp_x, exp_x_ad, exp_y_ad

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
  end subroutine test_arg_function_rev

  subroutine test_call_subroutine_fwd
    real :: x, y, x_ad, fd, eps, x_base, x_eps

    eps = 1.0e-6
    x = 1.0
    y = 2.0
    call call_subroutine(x, y)
    x_base = x
    x = 1.0 + eps
    y = 2.0 + eps
    call call_subroutine(x, y)
    x_eps = x
    fd = (x_eps - x_base) / eps
    x = 1.0
    y = 2.0
    x_ad = 1.0
    call call_subroutine_fwd_ad(x, x_ad, y, 1.0)
    if (abs(x_ad - fd) > tol) then
       print *, 'test_call_subroutine_fwd failed', x_ad, fd
       error stop 1
    end if
    return
  end subroutine test_call_subroutine_fwd

  subroutine test_call_fucntion_fwd
    real :: x, y, x_eps, res_fd, x_ad, eps

    eps = 1.0e-6
    y = 3.0
    call call_fucntion(x, y)
    x_eps = 0.0
    call call_fucntion(x_eps, y + eps)
    res_fd = (x_eps - x) / eps
    call call_fucntion_fwd_ad(x_ad, y, 1.0)
    if (abs(x_ad - res_fd) > tol) then
       print *, 'test_call_fucntion_fwd failed', x_ad, res_fd
       error stop 1
    end if
    return
  end subroutine test_call_fucntion_fwd

  subroutine test_arg_operation_fwd
    real :: x, y, x_base, x_eps, x_ad, fd, eps

    eps = 1.0e-6
    x = 1.0
    y = 2.0
    call arg_operation(x, y)
    x_base = x
    x = 1.0 + eps
    y = 2.0 + eps
    call arg_operation(x, y)
    x_eps = x
    fd = (x_eps - x_base) / eps
    x = 1.0
    y = 2.0
    x_ad = 1.0
    call arg_operation_fwd_ad(x, x_ad, y, 1.0)
    if (abs(x_ad - fd) > tol) then
       print *, 'test_arg_operation_fwd failed', x_ad, fd
       error stop 1
    end if
    return
  end subroutine test_arg_operation_fwd

  subroutine test_arg_function_fwd
    real :: x, y, x_base, x_eps, x_ad, fd, eps

    eps = 1.0e-6
    x = 1.0
    y = 2.0
    call arg_function(x, y)
    x_base = x
    x = 1.0 + eps
    y = 2.0 + eps
    call arg_function(x, y)
    x_eps = x
    fd = (x_eps - x_base) / eps
    x = 1.0
    y = 2.0
    x_ad = 1.0
    call arg_function_fwd_ad(x, x_ad, y, 1.0)
    if (abs(x_ad - fd) > tol) then
       print *, 'test_arg_function_fwd failed', x_ad, fd
       error stop 1
    end if
    return
  end subroutine test_arg_function_fwd

end program run_call_example
