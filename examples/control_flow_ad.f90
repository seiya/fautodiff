module control_flow_ad
  implicit none

contains

  subroutine if_example_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y
    real, intent(inout) :: y_ad
    real, intent(in)  :: z_ad
    real :: dz_dx

    x_ad = 0.0

    IF (x > 0.0) THEN
      dz_dx = 1.0
      x_ad = z_ad * dz_dx + x_ad
    ELSE IF (x < 0.0) THEN
      dz_dx = - 1.0
      x_ad = z_ad * dz_dx
    ELSE
    END IF

    return
  end subroutine if_example_ad

  subroutine select_example_ad(i, i_ad, z_ad)
    integer, intent(in)  :: i
    real, intent(out) :: i_ad
    real, intent(in)  :: z_ad

    i_ad = 0.0

    SELECT CASE (i)
    CASE (1)
    CASE (2)
    CASE DEFAULT
    END SELECT

    return
  end subroutine select_example_ad

  subroutine do_example_ad(n, n_ad, sum_ad)
    integer, intent(in)  :: n
    real, intent(out) :: n_ad
    real, intent(in)  :: sum_ad
    real :: dsum_dsum
    real :: sum_ad_

    n_ad = 0.0

    DO i = n, 1, -1
      dsum_dsum = 1.0
      sum_ad_ = sum_ad * dsum_dsum
    END DO

    return
  end subroutine do_example_ad

  subroutine do_while_example_ad(x, x_ad, limit, limit_ad, count_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: limit
    real, intent(out) :: limit_ad
    real, intent(in)  :: count_ad
    real :: dcount_dcount
    real :: count_ad_

    x_ad = 0.0
    limit_ad = 0.0

    DO WHILE (y < limit)
      dcount_dcount = 1.0
      count_ad_ = count_ad * dcount_dcount
    END DO

    return
  end subroutine do_while_example_ad

end module control_flow_ad
