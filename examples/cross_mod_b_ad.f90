module cross_mod_b_ad
  use cross_mod_a_ad
  implicit none


contains

  subroutine call_inc(b)
    real, intent(inout) :: b
    real :: inc

    inc = 1.0
    call incval(b, inc)

    return
  end subroutine call_inc

  subroutine call_inc_kw(b)
    real, intent(inout) :: b
    real :: inc

    inc = 1.0
    call incval(inc=inc, a=b)

    return
  end subroutine call_inc_kw

  subroutine call_inc_fwd_ad(b, b_ad)
    real, intent(inout) :: b
    real, intent(inout) :: b_ad
    real :: inc_ad
    real :: inc

    inc_ad = 0.0 ! inc = 1.0
    inc = 1.0
    call incval_fwd_ad(b, b_ad, inc, inc_ad) ! call incval(b, inc)

    return
  end subroutine call_inc_fwd_ad

  subroutine call_inc_rev_ad(b_ad)
    real, intent(inout) :: b_ad
    real :: inc_ad

    inc_ad = 0.0

    call incval_rev_ad(b_ad, inc_ad) ! call incval(b, inc)

    return
  end subroutine call_inc_rev_ad

  subroutine call_inc_kw_fwd_ad(b, b_ad)
    real, intent(inout) :: b
    real, intent(inout) :: b_ad
    real :: inc_ad
    real :: inc

    inc_ad = 0.0 ! inc = 1.0
    inc = 1.0
    call incval_fwd_ad(b, b_ad, inc, inc_ad) ! call incval(inc=inc, a=b)

    return
  end subroutine call_inc_kw_fwd_ad

  subroutine call_inc_kw_rev_ad(b_ad)
    real, intent(inout) :: b_ad
    real :: inc_ad

    inc_ad = 0.0

    call incval_rev_ad(b_ad, inc_ad) ! call incval(inc=inc, a=b)

    return
  end subroutine call_inc_kw_rev_ad

end module cross_mod_b_ad
