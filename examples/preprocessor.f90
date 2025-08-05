module preprocessor_example
contains
  subroutine foo(x, y)
    real, intent(in) :: x
    real, intent(out) :: y
    y = x
#define SCALE_TWO 2
#ifdef USE_ADD
    y = y + 1.0
#endif
  end subroutine foo
end module preprocessor_example
