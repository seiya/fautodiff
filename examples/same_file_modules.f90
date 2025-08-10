module var_mod
  implicit none
  real :: b
  !$FAD CONSTANT_VARS: b
end module var_mod

module use_mod
  use var_mod
  implicit none
contains

  subroutine add_b(x, y)
    real, intent(in) :: x
    real, intent(out) :: y
    y = x + b
  end subroutine add_b

end module use_mod
