module directive_const_arg
  implicit none
contains
!$FAD CONSTANT_ARGS: k
  subroutine add_const(i, j, k)
    real, intent(in) :: i
    real, intent(out) :: j
    real, intent(in) :: k

    j = i + k
  end subroutine add_const
end module directive_const_arg

