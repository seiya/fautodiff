program run_control_flow
  use control_flow
  use control_flow_ad
  implicit none

  integer, parameter :: I_all = 0
  integer, parameter :: I_if_example = 1
  integer, parameter :: I_do_example = 2

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
           case ("if_example")
              i_test = I_if_example
           case ("do_example")
              i_test = I_do_example
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_if_example .or. i_test == I_all) then
     call test_if_example
  end if
  if (i_test == I_do_example .or. i_test == I_all) then
     call test_do_example
  end if

  stop
contains

  subroutine test_if_example
    real :: x, y, z
    real :: x_ad, y_ad, z_ad

    x = 1.0
    y = 2.0
    call if_example(x, y, z)

    x_ad = 0.0
    y_ad = 0.0
    z_ad = 1.0
    call if_example_ad(x, x_ad, y, y_ad, z_ad)

    print *, z, x_ad
    return
  end subroutine test_if_example

  subroutine test_do_example
    integer :: n
    real :: x, sum
    real :: x_ad, sum_ad

    n = 3
    x = 2.0
    call do_example(n, x, sum)

    x_ad = 0.0
    sum_ad = 1.0
    call do_example_ad(n, x, x_ad, sum_ad)

    print *, sum, x_ad
    return
  end subroutine test_do_example

end program run_control_flow
