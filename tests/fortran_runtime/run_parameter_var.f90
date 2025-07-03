program run_parameter_var
  use parameter_var
  use parameter_var_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_compute_area_fwd = 1

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
           case ("compute_area_fwd")
              i_test = I_compute_area_fwd
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_compute_area_fwd .or. i_test == I_all) then
     call test_compute_area_fwd
  end if

  stop
contains

  subroutine test_compute_area_fwd
    real :: r, area, area_eps, area_ad, fd, eps

    eps = 1.0e-6
    r = 2.0
    call compute_area(r, area)
    call compute_area(r + eps, area_eps)
    fd = (area_eps - area) / eps
    call compute_area_fwd_ad(r, 1.0, area_ad)
    if (abs(area_ad - fd) > tol) then
       print *, 'test_compute_area_fwd failed', area_ad, fd
       error stop 1
    end if
    return
  end subroutine test_compute_area_fwd

end program run_parameter_var
