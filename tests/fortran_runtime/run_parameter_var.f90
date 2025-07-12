program run_parameter_var
  use parameter_var
  use parameter_var_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_compute_area = 1

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
           case ("compute_area")
              i_test = I_compute_area
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_compute_area .or. i_test == I_all) then
     call test_compute_area
  end if

  stop
contains

  subroutine test_compute_area
    real, parameter :: tol = 2e-4
    real :: r, area
    real :: r_ad, area_ad
    real :: area_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    r = 2.0
    call compute_area(r, area)
    call compute_area(r + eps, area_eps)
    fd = (area_eps - area) / eps
    r_ad = 1.0
    call compute_area_fwd_ad(r, r_ad, area, area_ad)
    if (abs((area_ad - fd) / fd) > tol) then
       print *, 'test_compute_area_fwd failed', area_ad, fd
       error stop 1
    end if

    inner1 = area_ad**2
    call compute_area_rev_ad(r, r_ad, area_ad)
    inner2 = r_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_compute_area_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_compute_area

end program run_parameter_var
