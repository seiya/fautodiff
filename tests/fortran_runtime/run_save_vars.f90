program run_save_vars
  use save_vars
  use save_vars_ad
  implicit none

  integer, parameter :: I_all = 0
  integer, parameter :: I_simple = 1

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
           case ("simple")
              i_test = I_simple
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_simple .or. i_test == I_all) then
     call test_simple
  end if

  stop
contains

  subroutine test_simple
    real :: x, y, z
    real :: x_ad, y_ad, z_ad

    x = 2.0
    y = 3.0
    call simple(x, y, z)

    x_ad = 0.0
    y_ad = 0.0
    z_ad = 1.0
    call simple_ad(x, x_ad, y, y_ad, z_ad)

    print *, z, x_ad, y_ad
    return
  end subroutine test_simple

end program run_save_vars
