#define CONST 1
module macro_sample
#define CONST_MOD 2
  real :: modvar = CONST_MOD
contains
  subroutine foo(x)
#define CONST_SUB 3
    real, intent(out) :: x
    real :: subvar = CONST_SUB
    x = CONST + CONST_MOD + CONST_SUB
  end subroutine foo
end module macro_sample
