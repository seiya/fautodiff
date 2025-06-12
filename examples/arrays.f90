module array_examples
  implicit none
contains
  subroutine elementwise_add(a, b, c, n)
    integer, intent(in) :: n
    real, intent(in) :: a(n), b(n)
    real, intent(out) :: c(n)
    c = a + b
  end subroutine elementwise_add

  subroutine scale_array(a, n)
    integer, intent(in) :: n
    real, intent(inout) :: a(n)
    integer :: i
    do i = 1, n
      a(i) = a(i) * 2.0
    end do
  end subroutine scale_array

  function dot_product(a, b, n) result(res)
    integer, intent(in) :: n
    real, intent(in) :: a(n), b(n)
    real :: res
    integer :: i
    res = 0.0
    do i = 1, n
      res = res + a(i) * b(i)
    end do
  end function dot_product
end module array_examples
