module call_module_vars
  use module_vars
  implicit none

contains

  subroutine call_inc_and_use(x, y)
    real, intent(in) :: x
    real, intent(out) :: y
    real :: z

    z = x * 2.0
    call inc_and_use(x, y)
    y = y * z

    return
  end subroutine call_inc_and_use

end module call_module_vars
