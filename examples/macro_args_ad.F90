#define SQR(x) ((x) * (x))
#define MUL(x, y) ((x) * (y))

module macro_args_ad
  implicit none


contains

  subroutine foo(a, b)
    real, intent(in)  :: a
    real, intent(out) :: b

    b = SQR(a + 1.0) + MUL(a, a - 1.0)

    return
  end subroutine foo

  subroutine foo_fwd_ad(a, a_ad, b, b_ad)
    real, intent(in)  :: a
    real, intent(in)  :: a_ad
    real, intent(out) :: b
    real, intent(out) :: b_ad

    b_ad = a_ad * (a + 1.0 + a + 1.0 + a - 1.0 + a) ! b = SQR(a + 1.0) + MUL(a, a - 1.0)
    b = SQR(a + 1.0) + MUL(a, a - 1.0)

    return
  end subroutine foo_fwd_ad

  subroutine foo_rev_ad(a, a_ad, b_ad)
    real, intent(in)  :: a
    real, intent(inout) :: a_ad
    real, intent(inout) :: b_ad

    a_ad = b_ad * (a + 1.0 + a + 1.0 + a - 1.0 + a) + a_ad ! b = SQR(a + 1.0) + MUL(a, a - 1.0)
    b_ad = 0.0 ! b = SQR(a + 1.0) + MUL(a, a - 1.0)

    return
  end subroutine foo_rev_ad

end module macro_args_ad
