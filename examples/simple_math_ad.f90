module simple_math_ad
  implicit none

contains

  subroutine add_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(in)  :: c_ad
    real :: dc_dc
    real :: dc_dwork
    real :: work_ad
    real :: c_ad_
    real :: dc_da
    real :: dwork_da
    real :: dwork_db

    dc_dc = 1.0
    dc_dwork = 1.0
    work_ad = c_ad * dc_dwork
    c_ad_ = c_ad * dc_dc
    dc_da = 1.0
    a_ad = c_ad_ * dc_da
    dwork_da = 1.0
    dwork_db = 1.0
    b_ad = work_ad * dwork_db
    a_ad = work_ad * dwork_da + a_ad

    return
  end subroutine add_numbers_ad

  subroutine multiply_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(in)  :: c_ad
    real :: dc_dc
    real :: dc_da
    real :: c_ad_
    real :: dc_db

    dc_dc = 3.0
    dc_da = 1.0
    a_ad = c_ad * dc_da
    c_ad_ = c_ad * dc_dc
    dc_da = b
    dc_db = a
    b_ad = c_ad_ * dc_db
    a_ad = c_ad_ * dc_da + a_ad

    return
  end subroutine multiply_numbers_ad

end module simple_math_ad
