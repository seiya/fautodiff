module simple_math_ad
  use simple_math
  implicit none

contains

  subroutine add_numbers_fwd_ad(a, a_ad, b, b_ad, c, c_ad)
    real, intent(in)  :: a
    real, intent(in)  :: a_ad
    real, intent(in)  :: b
    real, intent(in)  :: b_ad
    real, intent(out) :: c
    real, intent(out) :: c_ad
    real :: work_ad
    real :: work

    work_ad = a_ad + b_ad ! work = a + b
    work = a + b
    c_ad = a_ad ! c = a + 1.0
    c = a + 1.0
    c_ad = c_ad + work_ad ! c = c + 2.0 + work
    c = c + 2.0 + work

    return
  end subroutine add_numbers_fwd_ad

  subroutine add_numbers_rev_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(inout) :: c_ad
    real :: work_ad

    work_ad = c_ad ! c = c + 2.0 + work
    a_ad = c_ad ! c = a + 1.0
    c_ad = 0.0 ! c = a + 1.0
    a_ad = work_ad + a_ad ! work = a + b
    b_ad = work_ad ! work = a + b

    return
  end subroutine add_numbers_rev_ad

  subroutine subtract_numbers_fwd_ad(a, a_ad, b, b_ad, c, c_ad)
    real, intent(in)  :: a
    real, intent(in)  :: a_ad
    real, intent(in)  :: b
    real, intent(in)  :: b_ad
    real, intent(out) :: c
    real, intent(out) :: c_ad

    c_ad = a_ad - b_ad ! c = a - b
    c = a - b
    c_ad = - c_ad + b_ad ! c = - c + b
    c = - c + b

    return
  end subroutine subtract_numbers_fwd_ad

  subroutine subtract_numbers_rev_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(inout) :: c_ad

    b_ad = c_ad ! c = - c + b
    c_ad = - c_ad ! c = - c + b
    a_ad = c_ad ! c = a - b
    b_ad = - c_ad + b_ad ! c = a - b
    c_ad = 0.0 ! c = a - b

    return
  end subroutine subtract_numbers_rev_ad

  subroutine multiply_numbers_fwd_ad(a, a_ad, b, b_ad, c, c_ad)
    real, intent(in)  :: a
    real, intent(in)  :: a_ad
    real, intent(in)  :: b
    real, intent(in)  :: b_ad
    real, intent(out) :: c
    real, intent(out) :: c_ad

    c_ad = a_ad * (b + 1.0) + b_ad * a ! c = a * b + a
    c = a * b + a
    c_ad = c_ad * 3.0 + a_ad ! c = c * 3.0 + a
    c = c * 3.0 + a

    return
  end subroutine multiply_numbers_fwd_ad

  subroutine multiply_numbers_rev_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(inout) :: c_ad

    a_ad = c_ad ! c = c * 3.0 + a
    c_ad = c_ad * 3.0 ! c = c * 3.0 + a
    a_ad = c_ad * (b + 1.0) + a_ad ! c = a * b + a
    b_ad = c_ad * a ! c = a * b + a
    c_ad = 0.0 ! c = a * b + a

    return
  end subroutine multiply_numbers_rev_ad

  subroutine divide_numbers_fwd_ad(a, a_ad, b, b_ad, c, c_ad)
    real, intent(in)  :: a
    real, intent(in)  :: a_ad
    real, intent(in)  :: b
    real, intent(in)  :: b_ad
    real, intent(out) :: c
    real, intent(out) :: c_ad

    c_ad = a_ad / (b + 1.5) - b_ad * a / (b + 1.5)**2 ! c = a / (b + 1.5)
    c = a / (b + 1.5)
    c_ad = c_ad / 2.0 + a_ad ! c = c / 2.0 + a
    c = c / 2.0 + a

    return
  end subroutine divide_numbers_fwd_ad

  subroutine divide_numbers_rev_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(inout) :: c_ad

    a_ad = c_ad ! c = c / 2.0 + a
    c_ad = c_ad / 2.0 ! c = c / 2.0 + a
    a_ad = c_ad / (b + 1.5) + a_ad ! c = a / (b + 1.5)
    b_ad = - c_ad * a / (b + 1.5)**2 ! c = a / (b + 1.5)
    c_ad = 0.0 ! c = a / (b + 1.5)

    return
  end subroutine divide_numbers_rev_ad

  subroutine power_numbers_fwd_ad(a, a_ad, b, b_ad, c, c_ad)
    real, intent(in)  :: a
    real, intent(in)  :: a_ad
    real, intent(in)  :: b
    real, intent(in)  :: b_ad
    real, intent(out) :: c
    real, intent(out) :: c_ad

    c_ad = a_ad * 3.0 * a**2 + b_ad * 5.5 * b**4.5 ! c = a**3 + b**5.5
    c = a**3 + b**5.5
    c_ad = c_ad + a_ad * (b * a**(b - 1.0) + b * (4.0 * a + 2.0)**(b - 1.0) * 4.0 + (b * 5.0 + 3.0) * a**(b * 5.0 + 2.0)) + b_ad * (a**b * log(a) + (4.0 * a + 2.0)**b * log(4.0 * a + 2.0) + a**(b * 5.0 + 3.0) * log(a) * 5.0) ! c = c + a**b + (4.0 * a + 2.0)**b + a**(b * 5.0 + 3.0)
    c = c + a**b + (4.0 * a + 2.0)**b + a**(b * 5.0 + 3.0)

    return
  end subroutine power_numbers_fwd_ad

  subroutine power_numbers_rev_ad(a, a_ad, b, b_ad, c_ad)
    real, intent(in)  :: a
    real, intent(out) :: a_ad
    real, intent(in)  :: b
    real, intent(out) :: b_ad
    real, intent(inout) :: c_ad

    a_ad = c_ad * (b * a**(b - 1.0) + b * (4.0 * a + 2.0)**(b - 1.0) * 4.0 + (b * 5.0 + 3.0) * a**(b * 5.0 + 2.0)) ! c = c + a**b + (4.0 * a + 2.0)**b + a**(b * 5.0 + 3.0)
    b_ad = c_ad * (a**b * log(a) + (4.0 * a + 2.0)**b * log(4.0 * a + 2.0) + a**(b * 5.0 + 3.0) * log(a) * 5.0) ! c = c + a**b + (4.0 * a + 2.0)**b + a**(b * 5.0 + 3.0)
    a_ad = c_ad * 3.0 * a**2 + a_ad ! c = a**3 + b**5.5
    b_ad = c_ad * 5.5 * b**4.5 + b_ad ! c = a**3 + b**5.5
    c_ad = 0.0 ! c = a**3 + b**5.5

    return
  end subroutine power_numbers_rev_ad

end module simple_math_ad
