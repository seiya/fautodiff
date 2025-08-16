#define BASE 1
#define TEMP BASE
#define DOUBLE TEMP
#undef TEMP
#ifndef OMIT
#define COND 5
#endif
module conditional_macro
contains
  subroutine foo(x, y)
    real, intent(in) :: x
    real, intent(out) :: y
    real :: z
    z = x + BASE
    z = z + DOUBLE
#ifdef COND
    y = z + COND
#else
    y = z
#endif
  end subroutine foo
end module conditional_macro
