module parameter_var
  implicit none
contains
  subroutine compute_area(r, area)
    real, intent(in) :: r
    real, intent(out) :: area
    real, parameter :: pi = 3.14159
    area = pi * r * r
  end subroutine compute_area
end module parameter_var
