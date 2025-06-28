program run_intrinsic_func
  use intrinsic_func
  use intrinsic_func_ad
  implicit none

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

    i = 3
    r = 4.5
    c = 'A'
    call casting_intrinsics(i, r, d, c, n)

    r_ad = 0.0
    d_ad = 1.0d0
    call casting_intrinsics_ad(i, r, r_ad, d_ad, c)

    print *, d, r_ad
    return
  end subroutine test_casting

end program run_intrinsic_func
