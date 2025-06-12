module array_examples_ad
  implicit none

contains

  subroutine elementwise_add_ad(n, a, a_ad, b, b_ad, c_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(out) :: b_ad(n)
    real, intent(in)  :: c_ad(n)
    real :: dc_da(n)
    real :: dc_db(n)

    dc_da(:) = 1.0
    dc_db(:) = 1.0
    b_ad(:) = c_ad * dc_db
    a_ad(:) = c_ad * dc_da

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
    real, intent(in)  :: a(n, m)
    real, intent(out) :: a_ad(n, m)
    real, intent(in)  :: b(n, m)
    real, intent(out) :: b_ad(n, m)
    real, intent(in)  :: c
    real, intent(out) :: c_ad
    real, intent(in)  :: d_ad(n, m)
    integer :: i
    integer :: j
    real :: dd_da_i__j
    real :: dd_db_i__j
    real :: dd_dc

    c_ad = 0.0
    DO j = m, 1, -1
      DO i = n, 1, -1
        dd_da_i__j = 1.0
        dd_db_i__j = c
        dd_dc = b(i, j)
        a_ad(i, j) = d_ad(i, j) * dd_da_i__j
        b_ad(i, j) = d_ad(i, j) * dd_db_i__j
        c_ad = d_ad(i, j) * dd_dc + c_ad
      END DO
    END DO

    return
  end subroutine multidimension_ad

  subroutine dot_product_ad(n, a, a_ad, b, b_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(out) :: b_ad(n)
    real, intent(in)  :: res_ad
    integer :: i
    real :: res_ad_
    real :: dres_dres
    real :: dres_da_i
    real :: dres_db_i

    res_ad_ = res_ad

    DO i = n, 1, -1
      dres_dres = 1.0
      dres_da_i = b(i)
      dres_db_i = a(i)
      a_ad(i) = res_ad_ * dres_da_i
      b_ad(i) = res_ad_ * dres_db_i
      res_ad_ = res_ad_ * dres_dres
    END DO

    return
  end subroutine dot_product_ad

  subroutine indirect_ad(n, a, a_ad, b_ad, c_ad, idx)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b_ad(n)
    real, intent(in)  :: c_ad(n)
    integer, intent(in)  :: idx(n)
    integer :: i
    real :: dc_da_idx_i_
    real :: db_da_idx_i_

    a_ad(:) = 0.0
    DO i = n, 1, -1
      dc_da_idx_i_ = 2 * a(idx(i))
      a_ad(idx(i)) = c_ad(idx(i)) * dc_da_idx_i_ + a_ad(idx(i))
      db_da_idx_i_ = 1.0
      a_ad(idx(i)) = b_ad(i) * db_da_idx_i_ + a_ad(idx(i))
    END DO

    return
  end subroutine indirect_ad

  subroutine stencil_ad(n, a, a_ad, b_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b_ad(n)
    integer :: i
    integer :: in
    integer :: ip
    real :: db_da_in
    real :: db_da_i
    real :: db_da_ip

    a_ad(:) = 0.0
    DO i = n, 1, -1
      in = i - 1
      ip = i + 1
      IF (i == 1) THEN
        in = n
      ELSE IF (i == n) THEN
        ip = 1
      END IF
      db_da_in = 1.0 / 4.0
      db_da_i = 2.0 / 4.0
      db_da_ip = 1.0 / 4.0
      a_ad(in) = b_ad(i) * db_da_in + a_ad(in)
      a_ad(i) = b_ad(i) * db_da_i + a_ad(i)
      a_ad(ip) = b_ad(i) * db_da_ip + a_ad(ip)
    END DO

    return
  end subroutine stencil_ad

end module array_examples_ad
