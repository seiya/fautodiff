program run_allocate_vars
  use allocate_vars
  use allocate_vars_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_allocate_and_sum = 1
  integer, parameter :: I_allocate_in_if = 2
  integer, parameter :: I_allocate_in_loop = 3
  integer, parameter :: I_save_alloc = 4
  integer, parameter :: I_module_vars = 5

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
           case ("allocate_and_sum")
              i_test = I_allocate_and_sum
            case ("allocate_in_if")
               i_test = I_allocate_in_if
            case ("allocate_in_loop")
               i_test = I_allocate_in_loop
            case ("save_alloc")
               i_test = I_save_alloc
            case ("module_vars")
               i_test = I_module_vars
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_allocate_and_sum .or. i_test == I_all) then
     call test_allocate_and_sum
  end if
  if (i_test == I_allocate_in_if .or. i_test == I_all) then
     call test_allocate_in_if
  end if
  if (i_test == I_allocate_in_loop .or. i_test == I_all) then
     call test_allocate_in_loop
  end if
  if (i_test == I_save_alloc .or. i_test == I_all) then
     call test_save_alloc
  end if
  if (i_test == I_module_vars .or. i_test == I_all) then
     call test_module_vars
  end if

  stop
contains

  subroutine test_allocate_and_sum
    real, parameter :: tol = 3.0e-4
    integer, parameter :: n = 5
    real :: x, res
    real :: x_ad, res_ad
    real :: res_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 2.0
    call allocate_and_sum(n, x, res)
    call allocate_and_sum(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    call allocate_and_sum_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_allocate_and_sum_fwd failed', res_ad, fd
       error stop 1
    end if

    inner1 = res_ad**2
    call allocate_and_sum_rev_ad(n, x, x_ad, res_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_allocate_and_sum_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_allocate_and_sum

  subroutine test_allocate_in_if
    real, parameter :: tol = 3.0e-4
    integer, parameter :: n = 5
    real :: x, res
    real :: x_ad, res_ad
    real :: res_eps, fd, eps

    eps = 1.0e-3
    x = 2.0
    call allocate_in_if(n, x, res)
    call allocate_in_if(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    call allocate_in_if_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_allocate_in_if_fwd failed', res_ad, fd
       error stop 1
    end if

    return
  end subroutine test_allocate_in_if

  subroutine test_allocate_in_loop
    real, parameter :: tol = 3.0e-4
    integer, parameter :: n = 5
    real :: x, res
    real :: x_ad, res_ad
    real :: res_eps, fd, eps

    eps = 1.0e-3
    x = 2.0
    call allocate_in_loop(n, x, res)
    call allocate_in_loop(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    call allocate_in_loop_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_allocate_in_loop_fwd failed', res_ad, fd
       error stop 1
    end if

    return
  end subroutine test_allocate_in_loop

  subroutine test_save_alloc
    real, parameter :: tol = 3.0e-4
    integer, parameter :: n = 3
    real :: x(n), y, z
    real :: x_ad(n), y_ad, z_ad
    real :: z_eps, fd, eps
    real :: inner1, inner2
    integer :: i

    eps = 1.0e-3
    x = (/1.0, 2.0, 3.0/)
    y = 2.0
    call save_alloc(n, x, y, z)
    y = 2.0 + eps
    call save_alloc(n, x, y, z_eps)
    fd = (z_eps - z) / eps
    x_ad(:) = 0.0
    y_ad = 1.0
    call save_alloc_fwd_ad(n, x, x_ad, y, y_ad, z, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_save_alloc_fwd failed', z_ad, fd
       error stop 1
    end if

    inner1 = z_ad**2
    x_ad(:) = 0.0
    y_ad = 0.0
    call save_alloc_rev_ad(n, x, x_ad, y, y_ad, z_ad)
    inner2 = y_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_save_alloc_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_save_alloc

  subroutine test_module_vars
    integer, parameter :: n = 1
    real :: x
    real :: x_ad
    real :: x_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 2.0
    call module_vars_init(n, x)
    call module_vars_main(n, x)
    call module_vars_finalize(n, x)
    x_eps = 2.0 + eps
    call module_vars_init(n, x_eps)
    call module_vars_main(n, x_eps)
    call module_vars_finalize(n, x_eps)
    fd = (x_eps - x) / eps
    x = 2.0
    x_ad = 1.0
    call module_vars_init(n, x)
    call module_vars_init_fwd_ad(n, x, x_ad)
    call module_vars_main_fwd_ad(n, x, x_ad)
    call module_vars_finalize_fwd_ad(n, x, x_ad)
    if (abs((x_ad - fd) / fd) > tol) then
       print *, 'test_module_vars_fwd failed', x_ad, fd
       error stop 1
    end if

    inner1 = x_ad**2
    call module_vars_finalize_rev_ad(n, x_ad)
    call module_vars_main_rev_ad(n, x_ad)
    call module_vars_init_rev_ad(n, x_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_module_vars_rev faile', inner1, inner2
       error stop 1
    end if

    call module_vars_finalize(n, x)

    return
  end subroutine test_module_vars

end program run_allocate_vars
