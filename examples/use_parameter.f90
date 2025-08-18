module use_parameter
  use support_mod
  implicit none
contains
  subroutine scale_and_shift(x, y)
    use support_mod
    real, parameter :: c = 2.0
    real, intent(inout) :: x
    real, intent(out) :: y

    call add_one(x)
    y = c * x

    return
  end subroutine scale_and_shift
end module use_parameter
