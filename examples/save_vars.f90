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

  subroutine do_with_local_array(n, m, x, y, z)
    integer, intent(in) :: n, m
    real, intent(in) :: x(n,m), y(n,m)
    real, intent(out) :: z(n,m)
    real :: work1(2,n,m)
    real :: work2(2,m)
    real :: work3(2)
    integer :: i, j, k

    do j = 1, m
       do i = 1, n
          work3(1) = x(i,j) + y(i,j)
          work3(2) = x(i,j) - y(i,j)
          work2(1,i) = work3(1) + work3(2) * x(i,j)
          work2(2,i) = work3(1) * work3(2) + y(i,j)
       end do

       do i = 1, n
          work1(1,i,j) = work2(1,i) * x(i,j)
          work1(2,i,j) = work2(2,i) * y(i,j)
       end do
    end do

    do j = 1, m
       do i = 1, n
          z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
          do k = 1, 2
             work3(k) = work1(k,i,j)
             work1(k,i,j) = x(i,j) * work3(k)
          end do
          z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
       end do
    end do

    return
  end subroutine do_with_local_array

  subroutine do_with_stencil_array(n, x)
    integer, intent(in) :: n
    real, intent(inout) :: x(n)
    integer :: i

    x(1) = x(1) * x(2) * 0.5
    do i = 2, n-1
       x(i) = x(i) * (x(i+1) - x(i-1)) * 0.5
    end do
    x(n) = - x(n) * x(n-1) * 0.5

    return
  end subroutine do_with_stencil_array

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

end module save_vars
