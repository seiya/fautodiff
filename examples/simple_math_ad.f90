module simple_math_ad
contains
  subroutine add_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(in)  :: c_ad
    real :: dc_da
    real :: dc_db
    dc_da = 1.0
    dc_db = 1.0
    a_ad = c_ad * dc_da
    b_ad = c_ad * dc_db
  end subroutine add_numbers_ad

  subroutine multiply_numbers_ad(a, a_ad, b, b_ad, result_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(in)  :: result_ad
    real :: dresult_da
    real :: dresult_db
    dresult_da = b
    dresult_db = a
    a_ad = result_ad * dresult_da
    b_ad = result_ad * dresult_db
  end subroutine multiply_numbers_ad
end module simple_math_ad
