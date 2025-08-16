module generic_interface_ad
  use generic_interface
  implicit none

contains

  subroutine add_real4_fwd_ad(x, x_ad, y, y_ad, r, r_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(in)  :: y
    real, intent(in)  :: y_ad
    real, intent(out) :: r
    real, intent(out) :: r_ad

    r_ad = x_ad + y_ad ! r = x + y
    r = x + y

    return
  end subroutine add_real4_fwd_ad

  subroutine add_real4_rev_ad(x_ad, y_ad, r_ad)
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad
    real, intent(inout) :: r_ad

    x_ad = r_ad + x_ad ! r = x + y
    y_ad = r_ad + y_ad ! r = x + y
    r_ad = 0.0 ! r = x + y

    return
  end subroutine add_real4_rev_ad

  subroutine add_real8_fwd_ad(x, x_ad, y, y_ad, r, r_ad)
    real(8), intent(in)  :: x
    real(8), intent(in)  :: x_ad
    real(8), intent(in)  :: y
    real(8), intent(in)  :: y_ad
    real(8), intent(out) :: r
    real(8), intent(out) :: r_ad

    r_ad = x_ad + y_ad ! r = x + y
    r = x + y

    return
  end subroutine add_real8_fwd_ad

  subroutine add_real8_rev_ad(x_ad, y_ad, r_ad)
    real(8), intent(inout) :: x_ad
    real(8), intent(inout) :: y_ad
    real(8), intent(inout) :: r_ad

    x_ad = r_ad + x_ad ! r = x + y
    y_ad = r_ad + y_ad ! r = x + y
    r_ad = 0.0d0 ! r = x + y

    return
  end subroutine add_real8_rev_ad

  subroutine call_add_real_8_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    integer, parameter :: RP = 8
    real(RP), intent(in)  :: x
    real(RP), intent(in)  :: x_ad
    real(RP), intent(in)  :: y
    real(RP), intent(in)  :: y_ad
    real(RP), intent(out) :: z
    real(RP), intent(out) :: z_ad

    call add_real8_fwd_ad(x, x_ad, y, y_ad, z, z_ad) ! z = add(x, y)

    return
  end subroutine call_add_real_8_fwd_ad

  subroutine call_add_real_8_rev_ad(x_ad, y_ad, z_ad)
    integer, parameter :: RP = 8
    real(RP), intent(inout) :: x_ad
    real(RP), intent(inout) :: y_ad
    real(RP), intent(inout) :: z_ad

    call add_real8_rev_ad(x_ad, y_ad, z_ad) ! z = add(x, y)

    return
  end subroutine call_add_real_8_rev_ad

  subroutine call_add_real_selected_real_kind_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    integer, parameter :: RP = selected_real_kind(15, 307)
    real(kind=RP), intent(in)  :: x
    real(kind=RP), intent(in)  :: x_ad
    real(kind=RP), intent(in)  :: y
    real(kind=RP), intent(in)  :: y_ad
    real(kind=RP), intent(out) :: z
    real(kind=RP), intent(out) :: z_ad

    call add_real8_fwd_ad(x, x_ad, y, y_ad, z, z_ad) ! z = add(x, y)

    return
  end subroutine call_add_real_selected_real_kind_fwd_ad

  subroutine call_add_real_selected_real_kind_rev_ad(x_ad, y_ad, z_ad)
    integer, parameter :: RP = selected_real_kind(15, 307)
    real(kind=RP), intent(inout) :: x_ad
    real(kind=RP), intent(inout) :: y_ad
    real(kind=RP), intent(inout) :: z_ad

    call add_real8_rev_ad(x_ad, y_ad, z_ad) ! z = add(x, y)

    return
  end subroutine call_add_real_selected_real_kind_rev_ad

  subroutine call_add_real_kind_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    integer, parameter :: RP = kind(1.0d0)
    real(kind=RP), intent(in)  :: x
    real(kind=RP), intent(in)  :: x_ad
    real(kind=RP), intent(in)  :: y
    real(kind=RP), intent(in)  :: y_ad
    real(kind=RP), intent(out) :: z
    real(kind=RP), intent(out) :: z_ad

    call add_real8_fwd_ad(x, x_ad, y, y_ad, z, z_ad) ! z = add(x, y)

    return
  end subroutine call_add_real_kind_fwd_ad

  subroutine call_add_real_kind_rev_ad(x_ad, y_ad, z_ad)
    integer, parameter :: RP = kind(1.0d0)
    real(kind=RP), intent(inout) :: x_ad
    real(kind=RP), intent(inout) :: y_ad
    real(kind=RP), intent(inout) :: z_ad

    call add_real8_rev_ad(x_ad, y_ad, z_ad) ! z = add(x, y)

    return
  end subroutine call_add_real_kind_rev_ad

end module generic_interface_ad
