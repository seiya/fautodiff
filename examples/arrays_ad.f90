module array_ad
  use array
  implicit none

contains

  subroutine elementwise_add_fwd_ad(n, a, a_ad, b, b_ad, c_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(in)  :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(in)  :: b_ad(n)
    real, intent(out) :: c_ad(n)

    c_ad(:) = a_ad + b_ad ! c(:) = a + b
    c_ad = c_ad(:) + b_ad(:n:1) ! c = c(:) + b(:n:1)

    return
  end subroutine elementwise_add_fwd_ad

  subroutine elementwise_add_rev_ad(n, a, a_ad, b, b_ad, c_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(out) :: b_ad(n)
    real, intent(inout) :: c_ad(n)

    b_ad(:n:1) = c_ad ! c = c(:) + b(:n:1)
    a_ad = c_ad(:) ! c(:) = a + b
    b_ad = c_ad(:) + b_ad ! c(:) = a + b
    c_ad(:) = 0.0 ! c(:) = a + b

    return
  end subroutine elementwise_add_rev_ad

  subroutine scale_array_fwd_ad(n, a, a_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: a(n)
    real, intent(inout) :: a_ad(n)
    integer :: i

    do i = 1, n
      a_ad(i) = a_ad(i) * 2.0 ! a(i) = a(i) * 2.0
    end do

    return
  end subroutine scale_array_fwd_ad

  subroutine scale_array_rev_ad(n, a, a_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: a(n)
    real, intent(inout) :: a_ad(n)
    integer :: i

    do i = n, 1, - 1
      a_ad(i) = a_ad(i) * 2.0 ! a(i) = a(i) * 2.0
    end do

    return
  end subroutine scale_array_rev_ad

  subroutine multidimension_fwd_ad(n, m, a, a_ad, b, b_ad, c, c_ad, d_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: a(n,m)
    real, intent(in)  :: a_ad(n,m)
    real, intent(in)  :: b(n,m)
    real, intent(in)  :: b_ad(n,m)
    real, intent(in)  :: c
    real, intent(in)  :: c_ad
    real, intent(out) :: d_ad(n,m)
    integer :: i
    integer :: j

    do j = 1, m
      do i = 1, n
        d_ad(i,j) = a_ad(i,j) + b_ad(i,j) * c + c_ad * b(i,j) ! d(i,j) = a(i,j) + b(i,j) * c
      end do
    end do

    return
  end subroutine multidimension_fwd_ad

  subroutine multidimension_rev_ad(n, m, a, a_ad, b, b_ad, c, c_ad, d_ad)
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
        a_ad(i,j) = d_ad(i,j) ! d(i,j) = a(i,j) + b(i,j) * c
        b_ad(i,j) = d_ad(i,j) * c ! d(i,j) = a(i,j) + b(i,j) * c
        c_ad = d_ad(i,j) * b(i,j) + c_ad ! d(i,j) = a(i,j) + b(i,j) * c
        d_ad(i,j) = 0.0 ! d(i,j) = a(i,j) + b(i,j) * c
      end do
    end do

    return
  end subroutine multidimension_rev_ad

  subroutine dot_product_fwd_ad(n, a, a_ad, b, b_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(in)  :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(in)  :: b_ad(n)
    real, intent(out) :: res_ad
    integer :: i

    res_ad = 0.0 ! res = 0.0
    do i = 1, n
      res_ad = res_ad + a_ad(i) * b(i) + b_ad(i) * a(i) ! res = res + a(i) * b(i)
    end do

    return
  end subroutine dot_product_fwd_ad

  subroutine dot_product_rev_ad(n, a, a_ad, b, b_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(out) :: b_ad(n)
    real, intent(inout) :: res_ad
    integer :: i

    do i = n, 1, - 1
      a_ad(i) = res_ad * b(i) ! res = res + a(i) * b(i)
      b_ad(i) = res_ad * a(i) ! res = res + a(i) * b(i)
    end do
    res_ad = 0.0 ! res = 0.0

    return
  end subroutine dot_product_rev_ad

  subroutine indirect_fwd_ad(n, a, a_ad, b_ad, c_ad, idx)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(in)  :: a_ad(n)
    real, intent(out) :: b_ad(n)
    real, intent(out) :: c_ad(n)
    integer, intent(in)  :: idx(n)
    integer :: i

    c_ad(:) = 0.0

    do i = 1, n
      b_ad(i) = a_ad(idx(i)) ! b(i) = a(idx(i)) + 1.0
      c_ad(idx(i)) = a_ad(idx(i)) * 2.0 * a(idx(i)) ! c(idx(i)) = a(idx(i))**2
    end do

    return
  end subroutine indirect_fwd_ad

  subroutine indirect_rev_ad(n, a, a_ad, b_ad, c_ad, idx)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(inout) :: b_ad(n)
    real, intent(inout) :: c_ad(n)
    integer, intent(in)  :: idx(n)
    integer :: i

    a_ad(:) = 0.0

    do i = n, 1, - 1
      a_ad(idx(i)) = c_ad(idx(i)) * 2.0 * a(idx(i)) + a_ad(idx(i)) ! c(idx(i)) = a(idx(i))**2
      c_ad(idx(i)) = 0.0 ! c(idx(i)) = a(idx(i))**2
      a_ad(idx(i)) = b_ad(i) + a_ad(idx(i)) ! b(i) = a(idx(i)) + 1.0
      b_ad(i) = 0.0 ! b(i) = a(idx(i)) + 1.0
    end do

    return
  end subroutine indirect_rev_ad

  subroutine stencil_fwd_ad(n, a, a_ad, b_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(in)  :: a_ad(n)
    real, intent(out) :: b_ad(n)
    integer :: i
    integer :: in
    integer :: ip

    do i = 1, n
      in = i - 1
      ip = i + 1
      if (i == 1) then
        in = n
      else if (i == n) then
        ip = 1
      end if
      b_ad(i) = a_ad(i) * 2.0 / 4.0 + a_ad(in) / 4.0 + a_ad(ip) / 4.0 ! b(i) = (2.0 * a(i) + a(in) + a(ip)) / 4.0
    end do

    return
  end subroutine stencil_fwd_ad

  subroutine stencil_rev_ad(n, a, a_ad, b_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(inout) :: b_ad(n)
    integer :: i
    integer :: in
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
      a_ad(i) = b_ad(i) * 2.0 / 4.0 + a_ad(i) ! b(i) = (2.0 * a(i) + a(in) + a(ip)) / 4.0
      a_ad(in) = b_ad(i) / 4.0 + a_ad(in) ! b(i) = (2.0 * a(i) + a(in) + a(ip)) / 4.0
      a_ad(ip) = b_ad(i) / 4.0 + a_ad(ip) ! b(i) = (2.0 * a(i) + a(in) + a(ip)) / 4.0
      b_ad(i) = 0.0 ! b(i) = (2.0 * a(i) + a(in) + a(ip)) / 4.0
    end do

    return
  end subroutine stencil_rev_ad

end module array_ad
