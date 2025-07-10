module module_vars
  implicit none
  real :: c = 2.0
  real :: a
  !$FAD DIFF_MODULE_VARS: a
contains
  subroutine inc_and_use(x, y)
    real, intent(in) :: x
    real, intent(out) :: y

    y = (c + x) * a
    a = a + x
    y = y * a

    return
  end subroutine inc_and_use
end module module_vars
