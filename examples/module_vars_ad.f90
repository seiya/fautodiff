module module_vars_ad
  use module_vars
  implicit none

contains

  subroutine inc_and_use_fwd_ad(x, x_ad, y_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y_ad

    y_ad = x_ad * 2.0 ! y = (c + x) * 2.0

    return
  end subroutine inc_and_use_fwd_ad

  subroutine inc_and_use_rev_ad(x, x_ad, y_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y_ad

    x_ad = y_ad * 2.0 ! y = (c + x) * 2.0
    y_ad = 0.0 ! y = (c + x) * 2.0

    return
  end subroutine inc_and_use_rev_ad

end module module_vars_ad
