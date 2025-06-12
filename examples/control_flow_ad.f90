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


    IF (x > 0.0) THEN
      dz_dx = 1.0
      x_ad = z_ad * dz_dx
    ELSE IF (x < 0.0) THEN
      dz_dx = - 1.0
      x_ad = z_ad * dz_dx
    ELSE
    END IF

    return
  end subroutine if_example_ad

  subroutine select_example_ad(i, x, x_ad, z_ad)
    integer, intent(in)  :: i
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: z_ad
    real :: dz_dx


    SELECT CASE (i)
    CASE (1)
      dz_dx = 1.0
      x_ad = z_ad * dz_dx
    CASE (2)
      dz_dx = 1.0
      x_ad = z_ad * dz_dx
    CASE DEFAULT
    END SELECT

    return
  end subroutine select_example_ad

  subroutine do_example_ad(n, x, x_ad, sum_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: sum_ad
    integer :: i
    real :: sum_ad_
    real :: dsum_dsum
    real :: dsum_dx

    x_ad = 0.0

    sum_ad_ = sum_ad

    DO i = n, 1, -1
      dsum_dsum = 1.0
      dsum_dx = i
      x_ad = sum_ad_ * dsum_dx + x_ad
      sum_ad_ = sum_ad_ * dsum_dsum
    END DO

    return
  end subroutine do_example_ad

  subroutine do_while_example_ad(x, x_ad, limit, limit_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: limit
    real, intent(out) :: limit_ad

    x_ad = 0.0
    limit_ad = 0.0

    return
  end subroutine do_while_example_ad

end module control_flow_ad
