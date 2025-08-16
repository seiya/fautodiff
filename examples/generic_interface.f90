module generic_interface
  implicit none
  interface add
    module procedure :: add_real, add_int
  end interface
contains
  real function add_real(x, y) result(r)
    real, intent(in) :: x, y
    r = x + y
  end function add_real

  integer function add_int(i, j) result(k)
    integer, intent(in) :: i, j
    k = i + j
  end function add_int

  subroutine call_add_real(x, y, z)
    real, intent(in) :: x, y
    real, intent(out) :: z
    z = add(x, y)
  end subroutine call_add_real
end module generic_interface
