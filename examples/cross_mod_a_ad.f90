module cross_mod_a_ad
  use cross_mod_a
  implicit none

contains

  subroutine incval_rev_ad(a, a_ad)
    real, intent(inout) :: a
    real, intent(inout) :: a_ad

    return
  end subroutine incval_rev_ad

end module cross_mod_a_ad
