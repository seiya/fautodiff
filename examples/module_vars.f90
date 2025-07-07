module module_vars
  implicit none
  real :: c
contains
  subroutine inc_and_use(x, y)
    real, intent(in) :: x
    real, intent(out) :: y

    y = (c + x) * 2.0
    c = c + x

    return
  end subroutine inc_and_use
end module module_vars
