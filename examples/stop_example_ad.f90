module stop_example_ad
  implicit none


contains

  subroutine stop_sub(x, y)
    real, intent(in)  :: x
    real, intent(out) :: y

    if (x < 0.0) then
      stop 'negative'
    end if
    if (x > 10.0) then
      error stop 1
    end if
    y = x

    return
  end subroutine stop_sub

  subroutine stop_sub_fwd_ad(x, x_ad, y, y_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad

    if (x < 0.0) then
      stop 'negative'
    end if
    if (x > 10.0) then
      error stop 1
    end if
    y_ad = x_ad ! y = x
    y = x

    return
  end subroutine stop_sub_fwd_ad

  subroutine stop_sub_rev_ad(x, x_ad, y_ad)
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad

    x_ad = y_ad + x_ad ! y = x
    y_ad = 0.0 ! y = x
    if (x > 10.0) then
      error stop 1
    end if
    if (x < 0.0) then
      stop 'negative'
    end if

    return
  end subroutine stop_sub_rev_ad

end module stop_example_ad
