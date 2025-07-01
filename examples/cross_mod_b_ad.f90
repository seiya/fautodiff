module cross_mod_b_ad
  implicit none

contains

  subroutine call_inc_ad(b, b_ad)
    real, intent(inout) :: b
    real, intent(inout) :: b_ad

    call incval_ad(b, b_ad) ! call incval(b)

    return
  end subroutine call_inc_ad

end module cross_mod_b_ad
