module support_mod_ad
  use support_mod
  implicit none

contains

  subroutine add_one_fwd_ad(val)
    real, intent(inout) :: val

    val = val + 1.0

    return
  end subroutine add_one_fwd_ad

  subroutine add_one_rev_ad()

    return
  end subroutine add_one_rev_ad

end module support_mod_ad
