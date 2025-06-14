module array_examples_ad
  implicit none

contains

  subroutine elementwise_add_ad(n, a, a_ad, b, b_ad, c_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(out) :: b_ad(n)
    real, intent(inout) :: c_ad(n)

    b_ad(:n:1) = c_ad
    a_ad = c_ad(:)
    b_ad = c_ad(:) + b_ad
    c_ad(:) = 0.0

    return
  end subroutine elementwise_add_ad

  subroutine scale_array_ad(n, a, a_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: a(n)
    real, intent(inout) :: a_ad(n)

    return
  end subroutine scale_array_ad

  subroutine multidimension_ad(n, m, a, a_ad, b, b_ad, c, c_ad, d_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: a(n,m)
    real, intent(out) :: a_ad(n,m)
    real, intent(in)  :: b(n,m)
    real, intent(out) :: b_ad(n,m)
    real, intent(in)  :: c
    real, intent(out) :: c_ad
    real, intent(inout) :: d_ad(n,m)
    integer :: i
    integer :: j

    c_ad = 0.0

    do j = m, 1, - 1
      do i = n, 1, - 1
        a_ad(i,j) = d_ad(i,j)
        b_ad(i,j) = d_ad(i,j) * c
        c_ad = d_ad(i,j) * b(i,j) + c_ad
        d_ad(i,j) = 0.0
      end do
    end do

    return
  end subroutine multidimension_ad

  subroutine dot_product_ad(n, a, a_ad, b, b_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(out) :: b_ad(n)
    real, intent(inout) :: res_ad
    integer :: i

    do i = n, 1, - 1
      a_ad(i) = res_ad * b(i)
      b_ad(i) = res_ad * a(i)
    end do
    res_ad = 0.0

    return
  end subroutine dot_product_ad

  subroutine indirect_ad(n, a, a_ad, b_ad, c_ad, idx)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(inout) :: b_ad(n)
    real, intent(inout) :: c_ad(n)
    integer, intent(in)  :: idx(n)
    integer :: i

    a_ad(:) = 0.0

    do i = n, 1, - 1
      a_ad(idx(i)) = c_ad(idx(i)) * 2.0 * a(idx(i)) + a_ad(idx(i))
      c_ad(idx(i)) = 0.0
      a_ad(idx(i)) = b_ad(i) + a_ad(idx(i))
      b_ad(i) = 0.0
    end do

    return
  end subroutine indirect_ad

  subroutine stencil_ad(n, a, a_ad, b_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(inout) :: b_ad(n)
    integer :: in
    integer :: i
    integer :: ip

    a_ad(:) = 0.0

    do i = n, 1, - 1
      in = i - 1
      ip = i + 1
      if (i == 1) then
        in = n
      else if (i == n) then
        ip = 1
      end if
      a_ad(i) = b_ad(i) * 2.0 / 4.0 + a_ad(i)
      a_ad(in) = b_ad(i) / 4.0 + a_ad(in)
      a_ad(ip) = b_ad(i) / 4.0 + a_ad(ip)
      b_ad(i) = 0.0
    end do

    return
  end subroutine stencil_ad

end module array_examples_ad
