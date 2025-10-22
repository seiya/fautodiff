module use_module_conflict_ad
  use real4_module_ad, only: r
  implicit none


contains

  subroutine add_with_mod(x, y)
    use real8_module_ad
    real(8), intent(in) :: x
    real(8), intent(out) :: y

    y = x + 1.0d0
    r = x

    return
  end subroutine add_with_mod

  subroutine add_with_mod_fwd_ad(x, x_ad, y, y_ad)
    use real8_module_ad
    real(8), intent(in) :: x
    real(8), intent(in) :: x_ad
    real(8), intent(out) :: y
    real(8), intent(out) :: y_ad

    y_ad = x_ad ! y = x + 1.0d0
    y = x + 1.0d0
    r_ad = x_ad ! r = x
    r = x

    return
  end subroutine add_with_mod_fwd_ad

  subroutine add_with_mod_rev_ad(x_ad, y_ad)
    use real8_module_ad
    real(8), intent(inout) :: x_ad
    real(8), intent(inout) :: y_ad

    x_ad = r_ad + x_ad ! r = x
    r_ad = 0.0d0 ! r = x
    x_ad = y_ad + x_ad ! y = x + 1.0d0
    y_ad = 0.0d0 ! y = x + 1.0d0

    return
  end subroutine add_with_mod_rev_ad

end module use_module_conflict_ad
