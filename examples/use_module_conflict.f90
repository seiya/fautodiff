module use_module_conflict
  use real4_module
  implicit none

contains

  subroutine add_with_mod(x, y)
    use real8_module
    real(8), intent(in) :: x
    real(8), intent(out) :: y

    y = x + 1.0d0
    r = x

    return
  end subroutine add_with_mod

end module use_module_conflict
