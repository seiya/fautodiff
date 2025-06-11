module control_flow_ad
  implicit none

contains

  subroutine if_example_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y
    real, intent(inout) :: y_ad
    real, intent(in)  :: z_ad


    return
  end subroutine if_example_ad

  subroutine select_example_ad(i, i_ad, z_ad)
    integer, intent(in)  :: i
    real, intent(out) :: i_ad
    real, intent(in)  :: z_ad


    return
  end subroutine select_example_ad

  subroutine do_example_ad(n, n_ad, sum_ad)
    integer, intent(in)  :: n
    real, intent(out) :: n_ad
    real, intent(in)  :: sum_ad


    return
  end subroutine do_example_ad

  subroutine do_while_example_ad(x, x_ad, limit, limit_ad, count_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: limit
    real, intent(out) :: limit_ad
    real, intent(in)  :: count_ad


    return
  end subroutine do_while_example_ad

end module control_flow_ad
