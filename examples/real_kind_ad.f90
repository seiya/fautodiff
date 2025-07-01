module real_kind_ad
  implicit none

contains

  subroutine scale_8_ad(x, x_ad)
    real(8), intent(inout) :: x
    real(8), intent(inout) :: x_ad

    x_ad = x_ad * 2.0d0 ! x = x * 2.0_8

    return
  end subroutine scale_8_ad

  subroutine scale_rp_ad(x, x_ad)
    real(RP), intent(inout) :: x
    real(RP), intent(inout) :: x_ad

    x_ad = x_ad * 2.0_RP ! x = x * 2.0_RP

    return
  end subroutine scale_rp_ad

  subroutine scale_dp_ad(x, x_ad)
    double precision, intent(inout) :: x
    double precision, intent(inout) :: x_ad

    x_ad = x_ad * 2.0e0d0 ! x = x * 2.0d0

    return
  end subroutine scale_dp_ad

end module real_kind_ad
