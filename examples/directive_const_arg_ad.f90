module directive_const_arg_ad
  use directive_const_arg
  implicit none

contains

  subroutine add_const_rev_ad(i, i_ad, j_ad, k)
    real, intent(in)  :: i
    real, intent(out) :: i_ad
    real, intent(inout) :: j_ad
    real, intent(in)  :: k

    i_ad = j_ad ! j = i + k
    j_ad = 0.0 ! j = i + k

    return
  end subroutine add_const_rev_ad

end module directive_const_arg_ad
