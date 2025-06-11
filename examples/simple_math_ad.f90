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

  subroutine subtract_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(in)  :: c_ad
    real :: dc_dc
    real :: dc_db
    real :: c_ad_
    real :: dc_da

    dc_dc = - 1.0
    dc_db = 1.0
    b_ad = c_ad * dc_db
    c_ad_ = c_ad * dc_dc
    dc_da = 1.0
    dc_db = - 1.0
    b_ad = c_ad_ * dc_db + b_ad
    a_ad = c_ad_ * dc_da

    return
  end subroutine subtract_numbers_ad

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
    dc_da = b + 1.0
    dc_db = a
    b_ad = c_ad_ * dc_db
    a_ad = c_ad_ * dc_da + a_ad

    return
  end subroutine multiply_numbers_ad

  subroutine divide_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(in)  :: c_ad
    real :: dc_dc
    real :: dc_da
    real :: c_ad_
    real :: dc_db

    dc_dc = 1.0 / 2.0
    dc_da = 1.0
    a_ad = c_ad * dc_da
    c_ad_ = c_ad * dc_dc
    dc_da = 1.0 / (b + 1.5)
    dc_db = - a / (b + 1.5)**2
    b_ad = c_ad_ * dc_db
    a_ad = c_ad_ * dc_da + a_ad

    return
  end subroutine divide_numbers_ad

  subroutine power_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(in)  :: c_ad
    real :: dc_dc
    real :: dc_da
    real :: dc_db
    real :: c_ad_

    dc_dc = 1.0
    dc_da = b * a**(b - 1.0) + b * (4.0 * a + 2.0)**(b - 1.0) * 4.0 + (b * 5.0 + 3.0) * a**(b * 5.0 + 2.0)
    dc_db = a**b * log(a) + (4.0 * a + 2.0)**b * log(4.0 * a + 2.0) + a**(b * 5.0 + 3.0) * log(a) * 5.0
    b_ad = c_ad * dc_db
    a_ad = c_ad * dc_da
    c_ad_ = c_ad * dc_dc
    dc_da = 3 * a**2
    dc_db = 5.5 * b**4.5
    b_ad = c_ad_ * dc_db + b_ad
    a_ad = c_ad_ * dc_da + a_ad

    return
  end subroutine power_numbers_ad

end module simple_math_ad
