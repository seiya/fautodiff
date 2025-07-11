module call_module_vars_ad
  use call_module_vars
  implicit none

contains

  subroutine call_inc_and_use_fwd_ad(x, x_ad, y_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y_ad
    real :: z_ad
    real :: z

    z_ad = a_ad ! z = a
    z = a
    call inc_and_use_fwd_ad(x, x_ad, y_ad) ! call inc_and_use(x, y)
    y_ad = y_ad + z_ad * a + a_ad * z ! y = y + z * a

    return
  end subroutine call_inc_and_use_fwd_ad

  subroutine call_inc_and_use_rev_ad(x, x_ad, y_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y_ad
    real :: z_ad
    real :: z

    z = a

    z_ad = y_ad * a ! y = y + z * a
    a_ad = y_ad * z + a_ad ! y = y + z * a
    call inc_and_use_rev_ad(x, x_ad, y_ad) ! call inc_and_use(x, y)
    a_ad = z_ad + a_ad ! z = a

    return
  end subroutine call_inc_and_use_rev_ad

end module call_module_vars_ad
