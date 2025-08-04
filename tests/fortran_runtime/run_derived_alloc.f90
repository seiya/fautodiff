program run_derived_alloc
  use derived_alloc
  use derived_alloc_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_run = 1

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
           case ("derived_alloc")
              i_test = I_run
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_run .or. i_test == I_all) then
     call test_derived_alloc
  end if

  stop
contains

  subroutine test_derived_alloc
    integer, parameter :: n = 2
    integer, parameter :: m = 1
    real, parameter :: tol = 2e-4
    real :: x, res, res_eps
    real :: x_ad, res_ad
    real :: fd, eps
    real :: inner1, inner2
    integer :: j

    eps = 1.0e-3
    x = 2.0
    call derived_alloc_init(n, m)
    call derived_alloc_run_fwd_rev_ad()
    call derived_alloc_run(n, m, x, res)
    call derived_alloc_finalize(m)
    call derived_alloc_init(n, m)
    call derived_alloc_run(n, m, x + eps, res_eps)
    call derived_alloc_finalize(m)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    call derived_alloc_init(n, m)
    call derived_alloc_init_fwd_ad(n, m)
    call derived_alloc_run_fwd_ad(n, m, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
      print *, 'test_derived_alloc_run_fwd failed', res_ad, fd
      error stop 1
    end if

    inner1 = res_ad**2
    call derived_alloc_finalize_rev_ad(m)
    x_ad = 0.0
    call derived_alloc_run_rev_ad(n, m, x, x_ad, res_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
      print *, 'test_derived_alloc_run_rev failed', inner1, inner2
      error stop 1
    end if

    call derived_alloc_finalize_fwd_ad(m)
    call derived_alloc_init_rev_ad(n, m)
    call derived_alloc_finalize(m)

    return
  end subroutine test_derived_alloc

end program run_derived_alloc
