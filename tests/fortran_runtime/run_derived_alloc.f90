program run_derived_alloc
  use derived_alloc
  use derived_alloc_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_example = 1

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
           case ("derived_alloc_example")
              i_test = I_example
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_example .or. i_test == I_all) then
     call test_derived_alloc_example
  end if

  stop
contains

  subroutine test_derived_alloc_example
    integer, parameter :: n = 5
    real :: x, res, res_eps
    real :: x_ad, res_ad
    real :: fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 2.0
    call derived_alloc_example(n, x, res)
    call derived_alloc_example(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    call derived_alloc_example_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_derived_alloc_example_fwd failed', res_ad, fd
       error stop 1
    end if

    inner1 = res_ad**2
    call derived_alloc_example_rev_ad(n, x, x_ad, res_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_derived_alloc_example_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_derived_alloc_example

end program run_derived_alloc
