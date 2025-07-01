program run_real_kind
  use real_kind
  use real_kind_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_scale_8 = 1
  integer, parameter :: I_scale_rp = 2
  integer, parameter :: I_scale_dp = 3

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
           case("scale_8")
              i_test = I_scale_8
           case("scale_rp")
              i_test = I_scale_rp
           case("scale_dp")
              i_test = I_scale_dp
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_scale_8 .or. i_test == I_all) then
     call test_scale_8
  end if
  if (i_test == I_scale_rp .or. i_test == I_all) then
     call test_scale_rp
  end if
  if (i_test == I_scale_dp .or. i_test == I_all) then
     call test_scale_dp
  end if

  stop
contains

  subroutine test_scale_8
    real(8) :: x, x_ad
    real(8) :: exp_x, exp_x_ad

    x = 2.0_8
    call scale_8(x)

    x_ad = 1.0_8
    call scale_8_ad(x, x_ad)

    exp_x = 4.0_8
    exp_x_ad = 2.0_8

    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol) then
       print *, 'test_scale_8 failed', x, x_ad
       error stop 1
    end if
    return
  end subroutine test_scale_8

  subroutine test_scale_rp
    real(RP) :: x, x_ad
    real(RP) :: exp_x, exp_x_ad

    x = 2.0_RP
    call scale_rp(x)

    x_ad = 1.0_RP
    call scale_rp_ad(x, x_ad)

    exp_x = 4.0_RP
    exp_x_ad = 2.0_RP

    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol) then
       print *, 'test_scale_rp failed', x, x_ad
       error stop 1
    end if
    return
  end subroutine test_scale_rp

  subroutine test_scale_dp
    double precision :: x, x_ad
    double precision :: exp_x, exp_x_ad

    x = 2.0d0
    call scale_dp(x)

    x_ad = 1.0d0
    call scale_dp_ad(x, x_ad)

    exp_x = 4.0d0
    exp_x_ad = 2.0d0

    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol) then
       print *, 'test_scale_dp failed', x, x_ad
       error stop 1
    end if
    return
  end subroutine test_scale_dp

end program run_real_kind
