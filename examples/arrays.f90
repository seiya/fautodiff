module array
  implicit none

contains

  subroutine elementwise_add(n, a, b, c)
    integer, intent(in) :: n
    real, intent(in) :: a(n), b(n)
    real, intent(out) :: c(n)

    c(:) = a + b
    c = c(:) + b(:n:1)

    return
  end subroutine elementwise_add

  subroutine scale_array(n, a)
    integer, intent(in) :: n
    real, intent(inout) :: a(n)
    integer :: i

    do i = 1, n
      a(i) = a(i) * 2.0
    end do

    return
  end subroutine scale_array

  subroutine multidimension(n, m, a, b, c, d)
    integer, intent(in) :: n, m
    real, intent(in) :: a(n, m), b(n, m)
    real, intent(in) :: c
    real, intent(out) :: d(n, m)
    integer :: i, j

    do j = 1, m
       do i = 1, n
          d(i,j) = a(i,j) + b(i,j) * c
       end do
    end do

    return
  end subroutine multidimension

  function dot_product(n, a, b) result(res)
    integer, intent(in) :: n
    real, intent(in) :: a(n), b(n)
    real :: res
    integer :: i

    res = 0.0
    do i = 1, n
      res = res + a(i) * b(i)
    end do

    return
  end function dot_product

  subroutine indirect(n, a, b, c, idx)
    integer, intent(in) :: n
    real, intent(in) :: a(n)
    real, intent(out) :: b(n)
    real, intent(out) :: c(n)
    integer, intent(in) :: idx(n)
    integer :: i

    do i = 1, n
       b(i) = a(idx(i)) + 1.0
       c(idx(i)) = a(idx(i))**2
    end do

    return
  end subroutine indirect

  subroutine stencil(n, a, b)
    integer, intent(in) :: n
    real, intent(in) :: a(n)
    real, intent(out) :: b(n)
    integer :: i, in, ip

    do i = 1, n
       in = i - 1
       ip = i + 1
       if (i == 1) then
          in = n
       else if (i == n) then
          ip = 1
       end if
       b(i) = (2.0 * a(i) + a(in) + a(ip)) / 4.0
    end do

    return
  end subroutine stencil

end module array
