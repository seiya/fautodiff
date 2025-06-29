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

    do i = 1, n
       work = x(i) * work
       z(i) = work
    end do

    return
  end subroutine do_with_recurrent_scalar

end module store_vars
