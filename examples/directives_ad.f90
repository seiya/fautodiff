module directives_ad
  use directives
  implicit none

contains

  subroutine add_const_fwd_ad(x, x_ad, y, y_ad, z)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad
    real, intent(in)  :: z

    y_ad = x_ad ! y = x + z
    y = x + z

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

  subroutine worker_fwd_ad(x, x_ad, z, z_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: z
    real, intent(out) :: z_ad
    real :: y_ad
    real :: y

    y_ad = x_ad ! y = x + 1.0
    y = x + 1.0
    z_ad = y_ad ! z = y
    z = y

    return
  end subroutine worker_fwd_ad

  subroutine worker_rev_ad(x, x_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: z_ad
    real :: y_ad

    y_ad = z_ad ! z = y
    z_ad = 0.0 ! z = y
    x_ad = y_ad ! y = x + 1.0

    return
  end subroutine worker_rev_ad

end module directives_ad
