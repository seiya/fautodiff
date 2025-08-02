module module_vars_ad
  use module_vars
  use fautodiff_stack
  implicit none

  real :: a_ad = 0.0

contains

  subroutine inc_and_use_fwd_ad(x, x_ad, y, y_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad

    y_ad = x_ad * a + a_ad * (c + x) ! y = (c + x) * a
    y = (c + x) * a
    a_ad = a_ad + x_ad ! a = a + x
    a = a + x
    y_ad = y_ad * a + a_ad * y ! y = y * a
    y = y * a

    return
  end subroutine inc_and_use_fwd_ad

  subroutine inc_and_use_rev_ad(x, x_ad, y_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y_ad
    real :: y
    real :: a_save_13_ad

    call fautodiff_stack_pop_r(a)
    y = (c + x) * a
    a_save_13_ad = a
    a = a + x

    a_ad = y_ad * y + a_ad ! y = y * a
    y_ad = y_ad * a ! y = y * a
    a = a_save_13_ad
    x_ad = a_ad ! a = a + x
    x_ad = y_ad * a + x_ad ! y = (c + x) * a
    a_ad = y_ad * (c + x) + a_ad ! y = (c + x) * a
    y_ad = 0.0 ! y = (c + x) * a

    return
  end subroutine inc_and_use_rev_ad

  subroutine inc_and_use_fwd_rev_ad()

    call fautodiff_stack_push_r(a)

    return
  end subroutine inc_and_use_fwd_rev_ad

end module module_vars_ad
