module parameter_var_ad
  implicit none


contains

  subroutine compute_area(r, area)
    real, intent(in)  :: r
    real, intent(out) :: area
    real, parameter :: pi = 3.14159

    area = pi * r * r

    return
  end subroutine compute_area

  subroutine compute_area_fwd_ad(r, r_ad, area, area_ad)
    real, parameter :: pi = 3.14159
    real, intent(in)  :: r
    real, intent(in)  :: r_ad
    real, intent(out) :: area
    real, intent(out) :: area_ad

    area_ad = r_ad * (pi * r + pi * r) ! area = pi * r * r
    area = pi * r * r

    return
  end subroutine compute_area_fwd_ad

  subroutine compute_area_rev_ad(r, r_ad, area_ad)
    real, parameter :: pi = 3.14159
    real, intent(in)  :: r
    real, intent(inout) :: r_ad
    real, intent(inout) :: area_ad

    r_ad = area_ad * (pi * r + pi * r) + r_ad ! area = pi * r * r
    area_ad = 0.0 ! area = pi * r * r

    return
  end subroutine compute_area_rev_ad

end module parameter_var_ad
