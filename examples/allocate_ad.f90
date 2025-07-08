module allocate_example_ad
  use allocate_example
  implicit none

contains

  subroutine allocate_and_sum_fwd_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res_ad

    res_ad = x_ad * 2.0 ! res = x * 2.0

    return
  end subroutine allocate_and_sum_fwd_ad

  subroutine allocate_and_sum_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad

    x_ad = res_ad * 2.0 ! res = x * 2.0
    res_ad = 0.0 ! res = x * 2.0

    return
  end subroutine allocate_and_sum_rev_ad

end module allocate_example_ad
