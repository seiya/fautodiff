module cross_mod_b_ad
  use cross_mod_b
  use cross_mod_a
  use cross_mod_a_ad
  implicit none

contains

  subroutine call_inc_ad(b, b_ad)
    real, intent(inout) :: b
    real, intent(inout) :: b_ad

    call incval_ad(b, b_ad) ! call incval(b)

    return
  end subroutine call_inc_ad

end module cross_mod_b_ad
