module return_example_ad
  use return_example
  implicit none

contains

  subroutine conditional_return_fwd_ad(x, x_ad, y, y_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad

    if (x < 0.0) then
      y_ad = - x_ad ! y = -x
      y = - x
      return
    end if
    y_ad = x_ad * (x + x) ! y = x * x
    y = x * x

    return
  end subroutine conditional_return_fwd_ad

  subroutine conditional_return_rev_ad(x, x_ad, y_ad)
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad
    logical :: return_flag_11_ad

    return_flag_11_ad = .true.
    if (x < 0.0) then
      return_flag_11_ad = .false.
    end if

    if (return_flag_11_ad) then
      x_ad = y_ad * (x + x) + x_ad ! y = x * x
      y_ad = 0.0 ! y = x * x
    end if
    if (x < 0.0) then
      x_ad = - y_ad + x_ad ! y = -x
      y_ad = 0.0 ! y = -x
    end if

    return
  end subroutine conditional_return_rev_ad

end module return_example_ad
