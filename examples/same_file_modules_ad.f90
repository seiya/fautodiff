module var_mod_ad
  implicit none

  real, public :: b

contains

end module var_mod_ad

module use_mod_ad
  use var_mod_ad
  implicit none


contains

  subroutine add_b(x, y)
    real, intent(in) :: x
    real, intent(out) :: y

    y = x + b

    return
  end subroutine add_b

  subroutine add_b_fwd_ad(x, x_ad, y, y_ad)
    real, intent(in) :: x
    real, intent(in) :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad

    y_ad = x_ad ! y = x + b
    y = x + b

    return
  end subroutine add_b_fwd_ad

  subroutine add_b_rev_ad(x_ad, y_ad)
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad

    x_ad = y_ad + x_ad ! y = x + b
    y_ad = 0.0 ! y = x + b

    return
  end subroutine add_b_rev_ad

end module use_mod_ad
