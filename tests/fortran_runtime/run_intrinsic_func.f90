program run_intrinsic_func
  use intrinsic_func
  use intrinsic_func_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_casting = 1
  integer, parameter :: I_math = 2
  integer, parameter :: I_non_diff = 3
  integer, parameter :: I_special = 4

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
           case ("casting")
              i_test = I_casting
           case ("math")
              i_test = I_math
           case ("non_diff")
              i_test = I_non_diff
           case ("special")
              i_test = I_special
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_casting .or. i_test == I_all) then
     call test_casting
  end if
  if (i_test == I_math .or. i_test == I_all) then
     call test_math
  end if
  if (i_test == I_non_diff .or. i_test == I_all) then
     call test_non_diff
  end if
  if (i_test == I_special .or. i_test == I_all) then
     call test_special
  end if

  stop
contains

  subroutine test_casting
    integer :: i, n
    real :: r
    real :: r_ad
    real ::  r_eps, fd, eps
    double precision :: d, d_eps, d_ad
    character(len=1) :: c
    double precision :: exp_d
    real :: exp_r

    eps = 1.0e-3
    i = 3
    r = 4.5
    c = 'A'
    call casting_intrinsics(i, r, d, c, n)
    call casting_intrinsics(i, r + eps, d_eps, c, n)
    fd = (d_eps - d) / eps
    r_ad = 1.0
    call casting_intrinsics_fwd_ad(i, r, r_ad, d_ad, c)
    if (abs((d_ad - fd) / fd) > tol) then
       print *, 'test_casting_fwd failed', d_ad, fd
       error stop 1
    end if

    i = 3
    r = 4.5
    c = 'A'
    call casting_intrinsics(i, r, d, c, n)
    r_ad = 0.0
    d_ad = 1.0d0
    call casting_intrinsics_rev_ad(i, r, r_ad, d_ad, c)
    exp_d = dble(r) + dble(int(r))
    exp_r = 1.0
    if (abs(d - exp_d) > tol .or. abs(r_ad - exp_r) > tol) then
       print *, 'test_casting failed', d, r_ad
       error stop 1
    end if

    return
  end subroutine test_casting

  subroutine test_math
    real, parameter :: tol = 2e-4
    real :: x, y, z
    real :: x_ad, y_ad, z_ad
    real :: eps, z_eps, fd, y_eps

    eps = 1.0e-3
    x = 0.5
    y = 2.0
    call math_intrinsics(x, y, z)
    y_eps = y + eps
    call math_intrinsics(x + eps, y_eps, z_eps)
    fd = (z_eps - z) / eps
    x_ad = 1.0
    y_ad = 1.0
    call math_intrinsics_fwd_ad(x, x_ad, y, y_ad, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_math_fwd failed', z_ad, fd
       error stop 1
    end if

    return
  end subroutine test_math

  subroutine test_non_diff
    character(len=4) :: str
    real :: arr(2)
    real :: arr_ad(2)
    real :: y_ad

    str = 'abcd'
    arr = (/1.0, 2.0/)
    arr_ad = 0.0
    call non_differentiable_intrinsics_fwd_ad(str, arr, arr_ad, 1.0, 1.0, y_ad)
    if (y_ad /= 0.0) then
       print *, 'test_non_diff_fwd failed', y_ad
       error stop 1
    end if

    return
  end subroutine test_non_diff

  subroutine test_special
    real, parameter :: tol2 = 1.0e-6
    real :: mat_in(2,2)
    real :: mat_in_ad(2,2)
    real :: mat_out_ad(2,2)
    real :: exp(2,2)

    mat_in_ad = 1.0
    mat_in = reshape((/1.0,2.0,3.0,4.0/), (/2,2/))
    call special_intrinsics_fwd_ad(mat_in, mat_in_ad, mat_out_ad)
    exp = transpose(mat_in_ad)
    exp = cshift(exp, -1, 2)
    if (any(abs(mat_out_ad - exp) > tol2)) then
       print *, 'test_special_fwd failed'
       error stop 1
    end if

    return
  end subroutine test_special

end program run_intrinsic_func
