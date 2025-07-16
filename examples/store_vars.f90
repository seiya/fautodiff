module store_vars
  implicit none

contains

  subroutine do_with_recurrent_scalar(n, x, z)
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    real, intent(out) :: z(n)
    real :: work
    integer :: i

    work = 1.0
    work = x(1) * work
    z(:) = x(:) * work
    do i = 1, n
       work = x(i) * work
       z(i) = work**2 + z(i)
    end do

    return
  end subroutine do_with_recurrent_scalar

  subroutine do_while(x, y, z)
    real, intent(in) :: x
    real, intent(out) :: y, z
    real :: a

    y = 0.0
    z = 1.0
    a = y * x
    do while (y < 10.0)
       a = a + x
       y = y + a
       a = a + 1.0
       z = z * a
    end do
    y = z * y

    return
  end subroutine do_while

end module store_vars
