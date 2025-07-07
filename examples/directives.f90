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

!$FAD SKIP
  subroutine skip_me(x)
    real, intent(in) :: x

    ! routine is intentionally empty but must be parsed
  end subroutine skip_me

  subroutine worker(x, z)
    real, intent(in) :: x
    real, intent(out) :: z
    real :: y

    y = x + 1.0
    call skip_me(y)
    z = y
  end subroutine worker

end module directives

