module save_vars
  implicit none

contains

  subroutine simple(x, y, z)
    real, intent(in) :: x, y
    real, intent(out) :: z
    real :: work

    work = x + 1.0
    z = work + y
    work = work**2
    z = work * x + z
    work = x**2
    z = work * x + z

    return
  end subroutine simple

  subroutine if_example(x, y, z)
    real, intent(in) :: x, y
    real, intent(out) :: z
    real :: work

    work = x + 1.0
    z = work * y
    if ( work > 0.0 ) then
       work = work**2
       z = work * x + z
    else if ( work < 0.0 ) then
       work = x
       z = work * y
       work = work * x
    else
       z = work * x
    end if
    work = work * x
    z = work * x + z

    return
  end subroutine if_example

  subroutine do_with_array_private(n, m, x, y, z)
    integer, intent(in) :: n, m
    real, intent(in) :: x(n,m), y(n,m)
    real, intent(out) :: z(n,m)
    real :: ary(n,m)
    real :: scalar
    integer :: i, j

    ary(:,:) = x(:,:)
    do j = 1, m
       do i = 1, n
          z(i,j) = ary(i,j) * y(i,j)
          ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
          scalar = ary(i,j) * z(i,j)
          z(i,j) = x(i,j) + scalar
          z(i,j) = y(i,j) * scalar + z(i,j)
          scalar = z(i,j) * y(i,j)
          z(i,j) = z(i,j) * scalar
       end do
    end do

    return
  end subroutine do_with_array_private

  subroutine do_with_array(n, m, x, y, z)
    integer, intent(in) :: n, m
    real, intent(in) :: x(n,m), y(n,m)
    real, intent(out) :: z(n,m)
    real :: ary(n,m)
    integer :: i, j

    do j = 1, m
       do i = 1, n
          ary(i,j) = x(i,j) + 1.0
       end do
    end do

    do j = 1, m
       do i = 1, n
          z(i,j) = ary(i,j) * x(i,j)
          ary(i,j) = ary(i,j) + z(i,j) * y(i,j)
       end do
    end do

    do j = 1, m
       do i = 1, n
          z(i,j) = z(i,j) * x(i,j) + ary(i,j)
          ary(i,j) = y(i,j) * ary(i,j)
          z(i,j) = z(i,j) + ary(i,j)
       end do
    end do

    return
  end subroutine do_with_array

end module save_vars
