module preprocessor_example_ad
  use preprocessor_example
  implicit none

contains

  subroutine foo_fwd_ad(x, x_ad, y, y_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad

    y_ad = x_ad ! y = x
    y = x
#define SCALE_TWO 2
    #ifdef USE_ADD
    y = y + 1.0
    #endif

    return
  end subroutine foo_fwd_ad

  subroutine foo_rev_ad(x_ad, y_ad)
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad

    x_ad = y_ad + x_ad ! y = x
    y_ad = 0.0 ! y = x

    return
  end subroutine foo_rev_ad

end module preprocessor_example_ad
