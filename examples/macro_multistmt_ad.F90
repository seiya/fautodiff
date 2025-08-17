#ifdef INC
#define DO_TWO x  = x + 1; y = y * 2
#else
#define DO_TWO x  = x - 1; y = y * 2
#endif

module macro_multistmt_ad
  use macro_multistmt
  implicit none

contains

  subroutine foo_fwd_ad(x, y, y_ad)
    real :: x
    real :: y
    real :: y_ad

#ifdef INC
    x = x + 1
    y_ad = y_ad * 2 ! y = y * 2
    y = y * 2
#else
    x = x - 1
    y_ad = y_ad * 2 ! y = y * 2
    y = y * 2
#endif

    return
  end subroutine foo_fwd_ad

  subroutine foo_rev_ad(y_ad)
    real, intent(inout) :: y_ad

#ifdef INC
    y_ad = y_ad * 2 ! y = y * 2
#else
    y_ad = y_ad * 2 ! y = y * 2
#endif

    return
  end subroutine foo_rev_ad

end module macro_multistmt_ad
