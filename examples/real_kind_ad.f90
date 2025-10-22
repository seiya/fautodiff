module real_kind_ad
  implicit none

  integer, parameter, public :: RP = kind(1.0d0)

contains

  subroutine scale_8(x)
    real(8), intent(inout) :: x

    x = x * 2.0d0

    return
  end subroutine scale_8

  subroutine scale_rp(x)
    real(kind=RP), intent(inout) :: x

    x = x * 2.0_RP

    return
  end subroutine scale_rp

  subroutine scale_dp(x)
    double precision, intent(inout) :: x

    x = x * 2.0d0

    return
  end subroutine scale_dp

  subroutine scale_8_fwd_ad(x, x_ad)
    real(8), intent(inout) :: x
    real(8), intent(inout) :: x_ad

    x_ad = x_ad * 2.0d0 ! x = x * 2.0_8
    x = x * 2.0d0

    return
  end subroutine scale_8_fwd_ad

  subroutine scale_8_rev_ad(x_ad)
    real(8), intent(inout) :: x_ad

    x_ad = x_ad * 2.0d0 ! x = x * 2.0_8

    return
  end subroutine scale_8_rev_ad

  subroutine scale_rp_fwd_ad(x, x_ad)
    real(kind=RP), intent(inout) :: x
    real(kind=RP), intent(inout) :: x_ad

    x_ad = x_ad * 2.0_RP ! x = x * 2.0_RP
    x = x * 2.0_RP

    return
  end subroutine scale_rp_fwd_ad

  subroutine scale_rp_rev_ad(x_ad)
    real(kind=RP), intent(inout) :: x_ad

    x_ad = x_ad * 2.0_RP ! x = x * 2.0_RP

    return
  end subroutine scale_rp_rev_ad

  subroutine scale_dp_fwd_ad(x, x_ad)
    double precision, intent(inout) :: x
    double precision, intent(inout) :: x_ad

    x_ad = x_ad * 2.0d0 ! x = x * 2.0d0
    x = x * 2.0d0

    return
  end subroutine scale_dp_fwd_ad

  subroutine scale_dp_rev_ad(x_ad)
    double precision, intent(inout) :: x_ad

    x_ad = x_ad * 2.0d0 ! x = x * 2.0d0

    return
  end subroutine scale_dp_rev_ad

end module real_kind_ad
