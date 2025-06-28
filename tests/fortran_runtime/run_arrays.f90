program run_arrays
  use array
  use array_ad
  implicit none

  integer, parameter :: I_all = 0
  integer, parameter :: I_elementwise_add = 1
  integer, parameter :: I_dot_product = 2

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

  stop
contains

  subroutine test_elementwise_add
    integer, parameter :: n = 3
    real :: a(n), b(n), c(n)
    real :: a_ad(n), b_ad(n), c_ad(n)

    a = (/1.0, 2.0, 3.0/)
    b = (/4.0, 5.0, 6.0/)
    call elementwise_add(n, a, b, c)

    a_ad = 0.0
    b_ad = 0.0
    c_ad = 1.0
    call elementwise_add_ad(n, a, a_ad, b, b_ad, c_ad)

    print *, c(1), a_ad(1), b_ad(1)
    return
  end subroutine test_elementwise_add

  subroutine test_dot_product
    integer, parameter :: n = 3
    real :: a(n), b(n), res
    real :: a_ad(n), b_ad(n), res_ad

    a = (/1.0, 2.0, 3.0/)
    b = (/4.0, 5.0, 6.0/)
    res = dot_product(n, a, b)

    a_ad = 0.0
    b_ad = 0.0
    res_ad = 1.0
    call dot_product_ad(n, a, a_ad, b, b_ad, res_ad)

    print *, res, a_ad(1), b_ad(1)
    return
  end subroutine test_dot_product

end program run_arrays
