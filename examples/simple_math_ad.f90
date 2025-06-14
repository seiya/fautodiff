module simple_math_ad
  implicit none

contains

  subroutine add_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(inout) :: c_ad
    real :: work_ad

    work_ad = c_ad
    a_ad = c_ad
    c_ad = 0.0
    a_ad = work_ad + a_ad
    b_ad = work_ad

    return
  end subroutine add_numbers_ad

  subroutine subtract_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(inout) :: c_ad

    c_ad = - c_ad
    b_ad = c_ad
    a_ad = c_ad
    b_ad = - c_ad + b_ad
    c_ad = 0.0

    return
  end subroutine subtract_numbers_ad

  subroutine multiply_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(inout) :: c_ad

    c_ad = c_ad * 3.0
    a_ad = c_ad
    a_ad = c_ad * (b + 1.0) + a_ad
    b_ad = c_ad * a
    c_ad = 0.0

    return
  end subroutine multiply_numbers_ad

  subroutine divide_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(inout) :: c_ad

    c_ad = c_ad / 2.0
    a_ad = c_ad
    a_ad = c_ad / (b + 1.5) + a_ad
    b_ad = - c_ad * a / (b + 1.5)**2
    c_ad = 0.0

    return
  end subroutine divide_numbers_ad

  subroutine power_numbers_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(inout) :: c_ad

    a_ad = c_ad * (b * a**(b - 1.0) + b * (4.0 * a + 2.0)**(b - 1.0) * 4.0 + (b * 5.0 + 3.0) * a**(b * 5.0 + 2.0))
    b_ad = c_ad * (a**b * log(a) + (4.0 * a + 2.0)**b * log(4.0 * a + 2.0) + a**(b * 5.0 + 3.0) * log(a) * 5.0)
    a_ad = c_ad * 3.0 * a**2 + a_ad
    b_ad = c_ad * 5.5 * b**4.5 + b_ad
    c_ad = 0.0

    return
  end subroutine power_numbers_ad

end module simple_math_ad
