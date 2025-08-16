module generic_interface
  implicit none
  interface add
    module procedure :: add_real4, add_real8, add_int
  end interface
contains
  real function add_real4(x, y) result(r)
    real, intent(in) :: x, y
    r = x + y
  end function add_real4

  real(8) function add_real8(x, y) result(r)
    real(8), intent(in) :: x, y
    r = x + y
  end function add_real8

  integer function add_int(i, j) result(k)
    integer, intent(in) :: i, j
    k = i + j
  end function add_int

  subroutine call_add_real(x, y, z)
    integer, parameter :: RP = selected_real_kind(15, 307)
    real(kind=RP), intent(in) :: x, y
    real(kind=RP), intent(out) :: z
    z = add(x, y)
  end subroutine call_add_real
end module generic_interface
