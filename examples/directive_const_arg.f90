module directive_const_arg
  implicit none

contains

!$FAD CONSTANT_ARGS: z
  subroutine add_const(x, y, z)
    real, intent(in) :: x
    real, intent(out) :: y
    real, intent(in) :: z

    y = x + z

    return
  end subroutine add_const

end module directive_const_arg

