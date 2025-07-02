module parameter_var_ad
  use parameter_var
  implicit none

contains

  subroutine compute_area_fwd_ad(r, r_ad, area_ad)
    real, intent(in)  :: r
    real, intent(in)  :: r_ad
    real, intent(out) :: area_ad
    real, parameter :: pi = 3.14159

    area_ad = r_ad * (pi * r + pi * r) ! area = pi * r * r

    return
  end subroutine compute_area_fwd_ad

  subroutine compute_area_rev_ad(r, r_ad, area_ad)
    real, intent(in)  :: r
    real, intent(out) :: r_ad
    real, intent(inout) :: area_ad
    real, parameter :: pi = 3.14159

    r_ad = area_ad * (pi * r + pi * r) ! area = pi * r * r
    area_ad = 0.0 ! area = pi * r * r

    return
  end subroutine compute_area_rev_ad

end module parameter_var_ad
