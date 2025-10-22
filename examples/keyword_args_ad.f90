module keyword_args_ad
  implicit none


contains

  subroutine inc(a, b)
    real, intent(inout) :: a
    real, intent(in)  :: b

    a = a + b

    return
  end subroutine inc

  subroutine do_inc(x, y)
    real, intent(inout) :: x
    real, intent(in)  :: y

    call inc(a=x, b=y)

    return
  end subroutine do_inc

  subroutine inc_fwd_ad(a, a_ad, b, b_ad)
    real, intent(inout) :: a
    real, intent(inout) :: a_ad
    real, intent(in)  :: b
    real, intent(in)  :: b_ad

    a_ad = a_ad + b_ad ! a = a + b
    a = a + b

    return
  end subroutine inc_fwd_ad

  subroutine inc_rev_ad(a_ad, b_ad)
    real, intent(inout) :: a_ad
    real, intent(inout) :: b_ad

    b_ad = a_ad + b_ad ! a = a + b

    return
  end subroutine inc_rev_ad

  subroutine do_inc_fwd_ad(x, x_ad, y, y_ad)
    real, intent(inout) :: x
    real, intent(inout) :: x_ad
    real, intent(in)  :: y
    real, intent(in)  :: y_ad

    call inc_fwd_ad(x, x_ad, y, y_ad) ! call inc(a=x, b=y)

    return
  end subroutine do_inc_fwd_ad

  subroutine do_inc_rev_ad(x_ad, y_ad)
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad

    call inc_rev_ad(x_ad, y_ad) ! call inc(a=x, b=y)

    return
  end subroutine do_inc_rev_ad

end module keyword_args_ad
