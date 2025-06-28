program run_simple_math
  use simple_math
  use simple_math_ad
  implicit none

  integer, parameter :: I_all              = 0
  integer, parameter :: I_add_numbers      = 1
  integer, parameter :: I_multiply_numbers = 2
  integer, parameter :: I_subtract_numbers = 3
  integer, parameter :: I_divide_numbers   = 4
  integer, parameter :: I_power_numbers    = 5

  integer :: length, status
  character(:), allocatable :: arg
  integer :: i_test

  i_test = I_all ! default
  if (command_argument_count() > 0) then
     call get_command_argument(1, length=length, status=status)
     if (status == 0) then
        allocate(character(len=length) :: arg)
        call get_command_argument(1, arg, status=status)
        if (status == 0) then
           select case(arg)
           case ("add_numbers")
              i_test = I_add_numbers
           case ("multiply_numbers")
              i_test = I_multiply_numbers
           case ("subtract_numbers")
              i_test = I_subtract_numbers
           case ("divide_numbers")
              i_test = I_divide_numbers
           case ("power_numbers")
              i_test = I_power_numbers
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if
           
  if (i_test == I_add_numbers .or. i_test == I_all) then
     call test_add_numbers
  end if
  if (i_test == I_multiply_numbers .or. i_test == I_all) then
     call test_multiply_numbers
  end if
  if (i_test == I_subtract_numbers .or. i_test == I_all) then
     call test_subtract_numbers
  end if
  if (i_test == I_divide_numbers .or. i_test == I_all) then
     call test_divide_numbers
  end if
  if (i_test == I_power_numbers .or. i_test == I_all) then
     call test_power_numbers
  end if

  stop

contains

  subroutine test_add_numbers
    real :: a, b, c
    real :: a_ad, b_ad, c_ad

    a = 2.0
    b = 3.0
    c = add_numbers(a, b)

    a_ad = 0.0
    b_ad = 0.0
    c_ad = 1.0
    call add_numbers_ad(a, a_ad, b, b_ad, c_ad)

    print *, c, a_ad, b_ad

    return
  end subroutine test_add_numbers

  subroutine test_multiply_numbers
    real :: a, b, c
    real :: a_ad, b_ad, c_ad

    a = 2.0
    b = 3.0
    call multiply_numbers(a, b, c)

    a_ad = 0.0
    b_ad = 0.0
    c_ad = 1.0
    call multiply_numbers_ad(a, a_ad, b, b_ad, c_ad)

    print *, c, a_ad, b_ad

    return
  end subroutine test_multiply_numbers

  subroutine test_subtract_numbers
    real :: a, b, c
    real :: a_ad, b_ad, c_ad

    a = 2.0
    b = 3.0
    c = subtract_numbers(a, b)

    a_ad = 0.0
    b_ad = 0.0
    c_ad = 1.0
    call subtract_numbers_ad(a, a_ad, b, b_ad, c_ad)

    print *, c, a_ad, b_ad

    return
  end subroutine test_subtract_numbers

  subroutine test_divide_numbers
    real :: a, b, c
    real :: a_ad, b_ad, c_ad

    a = 2.0
    b = 3.0
    call divide_numbers(a, b, c)

    a_ad = 0.0
    b_ad = 0.0
    c_ad = 1.0
    call divide_numbers_ad(a, a_ad, b, b_ad, c_ad)

    print *, c, a_ad, b_ad

    return
  end subroutine test_divide_numbers

  subroutine test_power_numbers
    real :: a, b, c
    real :: a_ad, b_ad, c_ad

    a = 2.0
    b = 3.0
    c = power_numbers(a, b)

    a_ad = 0.0
    b_ad = 0.0
    c_ad = 1.0
    call power_numbers_ad(a, a_ad, b, b_ad, c_ad)

    print *, c, a_ad, b_ad

    return
  end subroutine test_power_numbers

end program run_simple_math
