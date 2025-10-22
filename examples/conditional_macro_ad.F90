#define BASE 1
#define TEMP BASE
#define DOUBLE TEMP
#ifndef OMIT
#define COND 5
#endif

module conditional_macro_ad
  implicit none


contains

  subroutine foo(x, y)
    real, intent(in)  :: x
    real, intent(out) :: y
    real :: z

    z = x + BASE
    z = z + DOUBLE
#ifdef COND
    y = z + COND
#else
    y = z
#endif

    return
  end subroutine foo

  subroutine foo_fwd_ad(x, x_ad, y, y_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad
    real :: z_ad
    real :: z

    z_ad = x_ad ! z = x + BASE
    z = x + BASE
    z = z + DOUBLE
#ifdef COND
    y_ad = z_ad ! y = z + COND
    y = z + COND
#else
    y_ad = z_ad ! y = z
    y = z
#endif

    return
  end subroutine foo_fwd_ad

  subroutine foo_rev_ad(x_ad, y_ad)
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad
    real :: z_ad

#ifdef COND
    z_ad = y_ad ! y = z + COND
    y_ad = 0.0 ! y = z + COND
#else
    z_ad = y_ad ! y = z
    y_ad = 0.0 ! y = z
#endif
    x_ad = z_ad + x_ad ! z = x + BASE

    return
  end subroutine foo_rev_ad

end module conditional_macro_ad
