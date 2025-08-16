#define SQR(x) ((x) * (x))
#define MUL(x, y) ((x) * (y))
module macro_args
  implicit none
contains

  subroutine foo(a, b)
    real, intent(in) :: a
    real, intent(out) :: b
    b = SQR(a + 1.0) + MUL(a, a - 1.0)
  end subroutine foo

end module macro_args
