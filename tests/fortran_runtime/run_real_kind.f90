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
    real(8) :: x
    real(8) :: x_ad
    real(8) :: x_eps, fd, eps
    real(8) :: inner1, inner2

    eps = 1.0e-6_8
    x = 2.0_8
    call scale_8(x)
    x_eps = 2.0_8 + eps
    call scale_8(x_eps)
    fd = (x_eps - x) / eps
    x = 2.0_8
    x_ad = 1.0_8
    call scale_8_fwd_ad(x, x_ad)
    if (abs(x_ad - fd) > tol) then
       print *, 'test_scale_8_fwd failed', x_ad, fd
       error stop 1
    end if

    inner1 = x_ad**2
    x = 2.0_8
    call scale_8(x)
    call scale_8_rev_ad(x_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_scale_8 failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_scale_8

  subroutine test_scale_rp
    real(RP) :: x
    real(RP) :: x_ad
    real(RP) :: x_eps, fd, eps
    real(RP) :: inner1, inner2

    eps = 1.0e-6_RP
    x = 2.0_RP
    call scale_rp(x)
    x_eps = 2.0_RP + eps
    call scale_rp(x_eps)
    fd = (x_eps - x) / eps
    x = 2.0_RP
    x_ad = 1.0_RP
    call scale_rp_fwd_ad(x, x_ad)
    if (abs(x_ad - fd) > tol) then
       print *, 'test_scale_rp_fwd failed', x_ad, fd
       error stop 1
    end if

    inner1 = x_ad**2
    x = 2.0_RP
    call scale_rp(x)
    call scale_rp_rev_ad(x_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_scale_rp failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_scale_rp

  subroutine test_scale_dp
    double precision :: x
    double precision :: x_ad
    double precision :: x_eps, fd, eps
    double precision :: inner1, inner2

    eps = 1.0d-6
    x = 2.0d0
    call scale_dp(x)
    x_eps = 2.0d0 + eps
    call scale_dp(x_eps)
    fd = (x_eps - x) / eps
    x = 2.0d0
    x_ad = 1.0d0
    call scale_dp_fwd_ad(x, x_ad)
    if (abs(x_ad - fd) > tol) then
       print *, 'test_scale_dp_fwd failed', x_ad, fd
       error stop 1
    end if

    inner1 = x_ad**2
    x = 2.0d0
    call scale_dp(x)
    call scale_dp_rev_ad(x_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_scale_dp failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_scale_dp

end program run_real_kind
