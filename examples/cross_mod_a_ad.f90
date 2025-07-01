module cross_mod_a_ad
  implicit none

contains

  subroutine incval_ad(a, a_ad)
    real, intent(inout) :: a
    real, intent(inout) :: a_ad

    return
  end subroutine incval_ad

end module cross_mod_a_ad
