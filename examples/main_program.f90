program main_program
  implicit none
  real :: x, y, z

  x = 1.0
  y = 2.0
  call simple(x, y, z)
  print *, "z =", z

contains

  subroutine simple(a, b, c)
    real, intent(in) :: a, b
    real, intent(out) :: c

    c = a + b

    return
  end subroutine simple
end program main_program
