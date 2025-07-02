program run_intrinsic_func
  use intrinsic_func
  use intrinsic_func_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_casting = 1

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
           case ("casting")
              i_test = I_casting
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_casting .or. i_test == I_all) then
     call test_casting
  end if

  stop
contains

  subroutine test_casting
    integer :: i, n
    real :: r, r_ad
    double precision :: d, d_ad
    character(len=1) :: c
    double precision :: exp_d
    real :: exp_r

    i = 3
    r = 4.5
    c = 'A'
    call casting_intrinsics(i, r, d, c, n)

    r_ad = 0.0
    d_ad = 1.0d0
    call casting_intrinsics_rev_ad(i, r, r_ad, d_ad, c)

    exp_d = dble(r) + dble(int(r))
    exp_r = 1.0

    if (abs(d - exp_d) > tol .or. abs(r_ad - exp_r) > tol) then
       print *, 'test_casting failed', d, r_ad
       error stop 1
    end if
    return
  end subroutine test_casting

end program run_intrinsic_func
