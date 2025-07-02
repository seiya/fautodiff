module directive_const_arg_ad
  use directive_const_arg
  implicit none

contains

  subroutine add_const_fwd_ad(x, x_ad, y_ad, z)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y_ad
    real, intent(in)  :: z

    y_ad = x_ad ! y = x + z

    return
  end subroutine add_const_fwd_ad

  subroutine add_const_rev_ad(x, x_ad, y_ad, z)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y_ad
    real, intent(in)  :: z

    x_ad = y_ad ! y = x + z
    y_ad = 0.0 ! y = x + z

    return
  end subroutine add_const_rev_ad

end module directive_const_arg_ad
