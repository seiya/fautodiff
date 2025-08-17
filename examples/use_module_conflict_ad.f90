module use_module_conflict_ad
  use use_module_conflict
  use real4_module, only: r4 => r
  use real8_module
  implicit none

contains

  subroutine add_with_mod_fwd_ad(x, x_ad, y, y_ad)
    real(8), intent(in)  :: x
    real(8), intent(in)  :: x_ad
    real(8), intent(out) :: y
    real(8), intent(out) :: y_ad

    y_ad = x_ad ! y = x + r
    y = x + r

    return
  end subroutine add_with_mod_fwd_ad

  subroutine add_with_mod_rev_ad(x_ad, y_ad)
    real(8), intent(inout) :: x_ad
    real(8), intent(inout) :: y_ad

    x_ad = y_ad + x_ad ! y = x + r
    y_ad = 0.0d0 ! y = x + r

    return
  end subroutine add_with_mod_rev_ad

end module use_module_conflict_ad
