#ifdef INC
#define DO_TWO x  = x + 1; y = y * 2
#else
#define DO_TWO x  = x - 1; y = y * 2
#endif
module macro_multistmt
contains
  subroutine foo(x, y)
    real :: x, y
    DO_TWO
  end subroutine foo
end module macro_multistmt
