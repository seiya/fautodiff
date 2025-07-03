program run_arrays
  use array
  use array_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_elementwise_add_rev = 1
  integer, parameter :: I_dot_product_rev = 2
  integer, parameter :: I_multidimension_rev = 3
  integer, parameter :: I_elementwise_add_fwd = 4
  integer, parameter :: I_dot_product_fwd = 5
  integer, parameter :: I_multidimension_fwd = 6

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
           case ("elementwise_add_rev")
              i_test = I_elementwise_add_rev
           case ("dot_product_rev")
              i_test = I_dot_product_rev
           case ("multidimension_rev")
              i_test = I_multidimension_rev
           case ("elementwise_add_fwd")
              i_test = I_elementwise_add_fwd
           case ("dot_product_fwd")
              i_test = I_dot_product_fwd
           case ("multidimension_fwd")
              i_test = I_multidimension_fwd
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_elementwise_add_rev .or. i_test == I_all) then
     call test_elementwise_add_rev
  end if
  if (i_test == I_dot_product_rev .or. i_test == I_all) then
     call test_dot_product_rev
  end if
  if (i_test == I_multidimension_rev .or. i_test == I_all) then
     call test_multidimension_rev
  end if
  if (i_test == I_elementwise_add_fwd) then
     call test_elementwise_add_fwd
  end if
  if (i_test == I_dot_product_fwd) then
     call test_dot_product_fwd
  end if
  if (i_test == I_multidimension_fwd) then
     call test_multidimension_fwd
  end if

  stop
contains

  subroutine test_elementwise_add_rev
    integer, parameter :: n = 3
    real :: a(n), b(n), c(n)
    real :: a_ad(n), b_ad(n), c_ad(n)
    real :: exp_c, exp_a, exp_b

    a = (/1.0, 2.0, 3.0/)
    b = (/4.0, 5.0, 6.0/)
    call elementwise_add(n, a, b, c)

    a_ad = 0.0
    b_ad = 0.0
    c_ad = 1.0
    call elementwise_add_rev_ad(n, a, a_ad, b, b_ad, c_ad)

    exp_c = a(1) + 2.0 * b(1)
    exp_a = 1.0
    exp_b = 2.0

    if (abs(c(1) - exp_c) > tol .or. abs(a_ad(1) - exp_a) > tol .or. &
        abs(b_ad(1) - exp_b) > tol) then
       print *, 'test_elementwise_add failed', c(1), a_ad(1), b_ad(1)
       error stop 1
    end if
    return
  end subroutine test_elementwise_add_rev

  subroutine test_dot_product_rev
    integer, parameter :: n = 3
    real :: a(n), b(n), res
    real :: a_ad(n), b_ad(n), res_ad
    real :: exp_res, exp_a, exp_b

    a = (/1.0, 2.0, 3.0/)
    b = (/4.0, 5.0, 6.0/)
    res = dot_product(n, a, b)

    a_ad = 0.0
    b_ad = 0.0
    res_ad = 1.0
    call dot_product_rev_ad(n, a, a_ad, b, b_ad, res_ad)

    exp_res = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
    exp_a = b(1)
    exp_b = a(1)

    if (abs(res - exp_res) > tol .or. abs(a_ad(1) - exp_a) > tol .or. &
        abs(b_ad(1) - exp_b) > tol) then
       print *, 'test_dot_product failed', res, a_ad(1), b_ad(1)
       error stop 1
    end if
    return
  end subroutine test_dot_product_rev

  subroutine test_multidimension_rev
    integer, parameter :: n = 2, m = 2
    real :: a(n,m), b(n,m), d(n,m)
    real :: a_ad(n,m), b_ad(n,m), d_ad(n,m)
    real :: c, c_ad
    real :: exp_d, exp_a, exp_b, exp_c

    a = reshape((/1.0, 2.0, 3.0, 4.0/), (/n, m/))
    b = reshape((/5.0, 6.0, 7.0, 8.0/), (/n, m/))
    c = 1.5
    call multidimension(n, m, a, b, c, d)

    a_ad = 0.0
    b_ad = 0.0
    d_ad = 0.0
    d_ad(1,1) = 1.0
    c_ad = 0.0
    call multidimension_rev_ad(n, m, a, a_ad, b, b_ad, c, c_ad, d_ad)

    exp_d = a(1,1) + b(1,1) * c
    exp_a = 1.0
    exp_b = c
    exp_c = b(1,1)

    if (abs(d(1,1) - exp_d) > tol .or. abs(a_ad(1,1) - exp_a) > tol .or. &
        abs(b_ad(1,1) - exp_b) > tol .or. abs(c_ad - exp_c) > tol) then
       print *, 'test_multidimension failed', d(1,1), a_ad(1,1), b_ad(1,1), c_ad
       error stop 1
    end if
    return
  end subroutine test_multidimension_rev

  subroutine test_elementwise_add_fwd
    integer, parameter :: n = 3
    real :: a(n), b(n), c(n), c_eps(n), c_ad(n), fd, eps

    eps = 1.0e-6
    a = (/1.0, 2.0, 3.0/)
    b = (/4.0, 5.0, 6.0/)
    call elementwise_add(n, a, b, c)
    call elementwise_add(n, a + eps, b + eps, c_eps)
    fd = (c_eps(1) - c(1)) / eps
    call elementwise_add_fwd_ad(n, a, 1.0, b, 1.0, c_ad)
    if (abs(c_ad(1) - fd) > tol) then
       print *, 'test_elementwise_add_fwd failed', c_ad(1), fd
       error stop 1
    end if
    return
  end subroutine test_elementwise_add_fwd

  subroutine test_dot_product_fwd
    integer, parameter :: n = 3
    real :: a(n), b(n), res, res_eps, res_ad, fd, eps

    eps = 1.0e-6
    a = (/1.0, 2.0, 3.0/)
    b = (/4.0, 5.0, 6.0/)
    res = dot_product(n, a, b)
    res_eps = dot_product(n, a + eps, b + eps)
    fd = (res_eps - res) / eps
    call dot_product_fwd_ad(n, a, 1.0, b, 1.0, res_ad)
    if (abs(res_ad - fd) > tol) then
       print *, 'test_dot_product_fwd failed', res_ad, fd
       error stop 1
    end if
    return
  end subroutine test_dot_product_fwd

  subroutine test_multidimension_fwd
    integer, parameter :: n = 2, m = 2
    real :: a(n,m), b(n,m), d(n,m), d_eps(n,m)
    real :: a_ad(n,m), b_ad(n,m), d_ad(n,m)
    real :: c, c_eps, c_ad, fd, eps

    eps = 1.0e-6
    a = reshape((/1.0, 2.0, 3.0, 4.0/), (/n, m/))
    b = reshape((/5.0, 6.0, 7.0, 8.0/), (/n, m/))
    c = 1.5
    call multidimension(n, m, a, b, c, d)
    call multidimension(n, m, a + eps, b + eps, c + eps, d_eps)
    fd = (d_eps(1,1) - d(1,1)) / eps
    call multidimension_fwd_ad(n, m, a, 1.0, b, 1.0, c, 1.0, d_ad)
    if (abs(d_ad(1,1) - fd) > tol) then
       print *, 'test_multidimension_fwd failed', d_ad(1,1), fd
       error stop 1
    end if
    return
  end subroutine test_multidimension_fwd

end program run_arrays
