program run_same_file_modules
  use var_mod_ad
  use use_mod_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_add_b = 1

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
           case ("add_b")
              i_test = I_add_b
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_add_b .or. i_test == I_all) then
     call test_add_b
  end if

  stop
contains

  subroutine test_add_b
    real, parameter :: eps = 1.0e-3
    real :: x, y, y_eps
    real :: x_ad, y_ad
    real :: fd, inner1, inner2

    b = 0.75
    x = 1.5
    call add_b(x, y)
    x = 1.5 + eps
    call add_b(x, y_eps)
    fd = (y_eps - y) / eps

    b = 0.75
    x = 1.5
    x_ad = 1.0
    call add_b_fwd_ad(x, x_ad, y, y_ad)
    if (abs((y_ad - fd) / fd) > tol) then
       print *, 'test_add_b_fwd failed', y_ad, fd
       error stop 1
    end if

    inner1 = y_ad**2
    x_ad = 0.0
    call add_b_rev_ad(x_ad, y_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_add_b_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_add_b

end program run_same_file_modules
