module use_parameter_ad
  use use_parameter
  use support_mod
  use support_mod_ad
  implicit none

contains

  subroutine scale_and_shift_fwd_ad(x, x_ad, y, y_ad)
    use support_mod
    use support_mod_ad
    real, parameter :: c = 2.0
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad

    call add_one_fwd_ad(x) ! call add_one(x)
    y_ad = x_ad * c ! y = c * x
    y = c * x

    return
  end subroutine scale_and_shift_fwd_ad

  subroutine scale_and_shift_rev_ad(x_ad, y_ad)
    use support_mod
    use support_mod_ad
    real, parameter :: c = 2.0
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad

    x_ad = y_ad * c + x_ad ! y = c * x
    y_ad = 0.0 ! y = c * x

    return
  end subroutine scale_and_shift_rev_ad

end module use_parameter_ad
