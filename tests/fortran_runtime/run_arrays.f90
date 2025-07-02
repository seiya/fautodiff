program run_arrays
  use array
  use array_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_elementwise_add = 1
  integer, parameter :: I_dot_product = 2
  integer, parameter :: I_multidimension = 3

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
           case ("elementwise_add")
              i_test = I_elementwise_add
           case ("dot_product")
              i_test = I_dot_product
           case ("multidimension")
              i_test = I_multidimension
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_elementwise_add .or. i_test == I_all) then
     call test_elementwise_add
  end if
  if (i_test == I_dot_product .or. i_test == I_all) then
     call test_dot_product
  end if
  if (i_test == I_multidimension .or. i_test == I_all) then
     call test_multidimension
  end if

  stop
contains

  subroutine test_elementwise_add
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
  end subroutine test_elementwise_add

  subroutine test_dot_product
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
  end subroutine test_dot_product

  subroutine test_multidimension
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
  end subroutine test_multidimension

end program run_arrays
