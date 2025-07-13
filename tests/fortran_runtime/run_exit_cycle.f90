program run_exit_cycle
  use exit_cycle
  use exit_cycle_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_loop_exit_cycle = 1

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
           case ("loop_exit_cycle")
              i_test = I_loop_exit_cycle
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_loop_exit_cycle .or. i_test == I_all) then
     call test_loop_exit_cycle
  end if

  stop
contains

  subroutine test_loop_exit_cycle
    integer :: n
    real :: x, res, res_eps
    real :: x_ad, res_ad
    real :: fd, eps

    eps = 1.0e-3
    n = 6
    x = 1.0
    call loop_exit_cycle(n, x, res)
    call loop_exit_cycle(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    call loop_exit_cycle_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_loop_exit_cycle_fwd failed', res_ad, fd
       error stop 1
    end if



    return
  end subroutine test_loop_exit_cycle

end program run_exit_cycle
