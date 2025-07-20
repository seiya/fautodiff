module call_module_vars_ad
  use call_module_vars
  use module_vars
  use module_vars_ad
  implicit none

contains

  subroutine call_inc_and_use_fwd_ad(x, x_ad, y, y_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad
    real :: z_ad
    real :: z

    z_ad = x_ad * 2.0 ! z = x * 2.0
    z = x * 2.0
    call inc_and_use_fwd_ad(x, x_ad, y, y_ad) ! call inc_and_use(x, y)
    y_ad = y_ad * z + z_ad * y ! y = y * z
    y = y * z

    return
  end subroutine call_inc_and_use_fwd_ad

  subroutine call_inc_and_use_rev_ad(x, x_ad, y_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y_ad
    real :: z_ad
    real :: z
    real :: y

    z = x * 2.0
    call inc_and_use_fwd_rev_ad(x, y)
    call inc_and_use(x, y)

    z_ad = y_ad * y ! y = y * z
    y_ad = y_ad * z ! y = y * z
    call inc_and_use_rev_ad(x, x_ad, y_ad) ! call inc_and_use(x, y)
    x_ad = z_ad * 2.0 + x_ad ! z = x * 2.0

    return
  end subroutine call_inc_and_use_rev_ad

end module call_module_vars_ad
