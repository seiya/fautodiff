module generic_interface_ad
  use generic_interface
  implicit none

contains

  subroutine add_real_fwd_ad(x, x_ad, y, y_ad, r, r_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(in)  :: y
    real, intent(in)  :: y_ad
    real, intent(out) :: r
    real, intent(out) :: r_ad

    r_ad = x_ad + y_ad ! r = x + y
    r = x + y

    return
  end subroutine add_real_fwd_ad

  subroutine add_real_rev_ad(x, x_ad, y, y_ad, r_ad)
    real, intent(in)     :: x
    real, intent(inout)  :: x_ad
    real, intent(in)     :: y
    real, intent(inout)  :: y_ad
    real, intent(inout)  :: r_ad

    x_ad = r_ad + x_ad ! r = x + y
    y_ad = r_ad + y_ad ! r = x + y
    r_ad = 0.0          ! r = x + y

    return
  end subroutine add_real_rev_ad

  subroutine call_add_real_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(in)  :: y
    real, intent(in)  :: y_ad
    real, intent(out) :: z
    real, intent(out) :: z_ad

    call add_real_fwd_ad(x, x_ad, y, y_ad, z, z_ad) ! z = add(x, y)

    return
  end subroutine call_add_real_fwd_ad

  subroutine call_add_real_rev_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)     :: x
    real, intent(inout)  :: x_ad
    real, intent(in)     :: y
    real, intent(inout)  :: y_ad
    real, intent(inout)  :: z_ad

    call add_real_rev_ad(x, x_ad, y, y_ad, z_ad) ! z = add(x, y)

    return
  end subroutine call_add_real_rev_ad

end module generic_interface_ad
