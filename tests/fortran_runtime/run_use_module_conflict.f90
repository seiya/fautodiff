program run_use_module_conflict
  use use_module_conflict
  use use_module_conflict_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_add_with_mod = 1

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
           case ("add_with_mod")
              i_test = I_add_with_mod
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_add_with_mod .or. i_test == I_all) then
     call test_add_with_mod
  end if

  stop
contains
  subroutine test_add_with_mod
    real(8) :: x, y
    real(8) :: x_ad, y_ad
    real(8) :: y_eps, fd, eps
    real(8) :: inner1, inner2

    eps = 1.0e-6_8
    x = 2.0_8
    call add_with_mod(x, y)
    call add_with_mod(x + eps, y_eps)
    fd = (y_eps - y) / eps
    x_ad = 1.0_8
    call add_with_mod_fwd_ad(x, x_ad, y, y_ad)
    if (abs((y_ad - fd) / fd) > tol) then
       print *, 'test_add_with_mod_fwd failed', y_ad, fd
       error stop 1
    end if

    inner1 = y_ad**2
    x_ad = 0.0_8
    call add_with_mod_rev_ad(x_ad, y_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_add_with_mod_rev failed', inner1, inner2
       error stop 1
    end if
  end subroutine test_add_with_mod
end program run_use_module_conflict
