#define CONST 1

module macro_sample_ad
  use macro_sample
  implicit none

#define CONST_MOD 2
  real :: modvar_ad = 0.0

contains

  subroutine foo_fwd_ad(x, x_ad)
#define CONST_SUB 3
    real, intent(out) :: x
    real, intent(out) :: x_ad

    x_ad = 0.0 ! x = CONST + CONST_MOD + CONST_SUB
    x = CONST + CONST_MOD + CONST_SUB

    return
  end subroutine foo_fwd_ad

  subroutine foo_rev_ad(x_ad)
#define CONST_SUB 3
    real, intent(inout) :: x_ad

    x_ad = 0.0 ! x = CONST + CONST_MOD + CONST_SUB

    return
  end subroutine foo_rev_ad

end module macro_sample_ad
