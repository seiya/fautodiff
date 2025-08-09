module cross_mod_a_ad
  use cross_mod_a
  implicit none

contains

  subroutine incval_fwd_ad(a, a_ad, inc, inc_ad)
    real, intent(inout) :: a
    real, intent(inout) :: a_ad
    real, intent(in)  :: inc
    real, intent(in)  :: inc_ad

    a_ad = a_ad + inc_ad ! a = a + inc
    a = a + inc

    return
  end subroutine incval_fwd_ad

  subroutine incval_rev_ad(a_ad, inc_ad)
    real, intent(inout) :: a_ad
    real, intent(inout) :: inc_ad

    inc_ad = a_ad + inc_ad ! a = a + inc

    return
  end subroutine incval_rev_ad

end module cross_mod_a_ad
