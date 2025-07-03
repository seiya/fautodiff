program run_real_kind
  use real_kind
  use real_kind_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  integer, parameter :: I_all = 0
  integer, parameter :: I_scale_8_rev = 1
  integer, parameter :: I_scale_rp_rev = 2
  integer, parameter :: I_scale_dp_rev = 3
  integer, parameter :: I_scale_8_fwd = 4
  integer, parameter :: I_scale_rp_fwd = 5
  integer, parameter :: I_scale_dp_fwd = 6

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
           case("scale_8_rev")
              i_test = I_scale_8_rev
           case("scale_rp_rev")
              i_test = I_scale_rp_rev
           case("scale_dp_rev")
              i_test = I_scale_dp_rev
           case("scale_8_fwd")
              i_test = I_scale_8_fwd
           case("scale_rp_fwd")
              i_test = I_scale_rp_fwd
           case("scale_dp_fwd")
              i_test = I_scale_dp_fwd
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_scale_8_rev .or. i_test == I_all) then
     call test_scale_8_rev
  end if
  if (i_test == I_scale_rp_rev .or. i_test == I_all) then
     call test_scale_rp_rev
  end if
  if (i_test == I_scale_dp_rev .or. i_test == I_all) then
     call test_scale_dp_rev
  end if
  if (i_test == I_scale_8_fwd) then
     call test_scale_8_fwd
  end if
  if (i_test == I_scale_rp_fwd) then
     call test_scale_rp_fwd
  end if
  if (i_test == I_scale_dp_fwd) then
     call test_scale_dp_fwd
  end if

  stop
contains

  subroutine test_scale_8_rev
    real(8) :: x, x_ad
    real(8) :: exp_x, exp_x_ad

    x = 2.0_8
    call scale_8(x)

    x_ad = 1.0_8
    call scale_8_rev_ad(x, x_ad)

    exp_x = 4.0_8
    exp_x_ad = 2.0_8

    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol) then
       print *, 'test_scale_8 failed', x, x_ad
       error stop 1
    end if
    return
  end subroutine test_scale_8_rev

  subroutine test_scale_rp_rev
    real(RP) :: x, x_ad
    real(RP) :: exp_x, exp_x_ad

    x = 2.0_RP
    call scale_rp(x)

    x_ad = 1.0_RP
    call scale_rp_rev_ad(x, x_ad)

    exp_x = 4.0_RP
    exp_x_ad = 2.0_RP

    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol) then
       print *, 'test_scale_rp failed', x, x_ad
       error stop 1
    end if
    return
  end subroutine test_scale_rp_rev

  subroutine test_scale_dp_rev
    double precision :: x, x_ad
    double precision :: exp_x, exp_x_ad

    x = 2.0d0
    call scale_dp(x)

    x_ad = 1.0d0
    call scale_dp_rev_ad(x, x_ad)

    exp_x = 4.0d0
    exp_x_ad = 2.0d0

    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol) then
       print *, 'test_scale_dp failed', x, x_ad
       error stop 1
    end if
    return
  end subroutine test_scale_dp_rev

  subroutine test_scale_8_fwd
    real(8) :: x, x_eps, x_ad, fd, eps

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
    return
  end subroutine test_scale_8_fwd

  subroutine test_scale_rp_fwd
    real(RP) :: x, x_eps, x_ad, fd, eps

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
    return
  end subroutine test_scale_rp_fwd

  subroutine test_scale_dp_fwd
    double precision :: x, x_eps, x_ad, fd, eps

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
    return
  end subroutine test_scale_dp_fwd

end program run_real_kind
