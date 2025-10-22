program main_program_ad
  implicit none

  real :: x
  real :: y
  real :: z
  real :: x_ad = 0.0
  real :: y_ad = 0.0
  real :: z_ad = 0.0

contains

  subroutine simple(a, b, c)
    real, intent(in) :: a
    real, intent(in) :: b
    real, intent(out) :: c

    c = a + b

    return
  end subroutine simple

  subroutine simple_fwd_ad(a, a_ad, b, b_ad, c, c_ad)
    real, intent(in) :: a
    real, intent(in) :: a_ad
    real, intent(in) :: b
    real, intent(in) :: b_ad
    real, intent(out) :: c
    real, intent(out) :: c_ad

    c_ad = a_ad + b_ad ! c = a + b
    c = a + b

    return
  end subroutine simple_fwd_ad

  subroutine simple_rev_ad(a_ad, b_ad, c_ad)
    real, intent(inout) :: a_ad
    real, intent(inout) :: b_ad
    real, intent(inout) :: c_ad

    a_ad = c_ad + a_ad ! c = a + b
    b_ad = c_ad + b_ad ! c = a + b
    c_ad = 0.0 ! c = a + b

    return
  end subroutine simple_rev_ad

end program main_program_ad
