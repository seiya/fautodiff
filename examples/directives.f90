module directives
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

!$FAD NO_AD
  subroutine skip_me(x, y)
    real, intent(inout) :: x, y

    y = x + y

  end subroutine skip_me

end module directives

