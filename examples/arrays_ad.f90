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
    real :: d_ad_(n, m)
    real :: dd_da
    real :: dd_db
    real :: dd_dc

    c_ad = 0.0

    d_ad_(:) = d_ad(:)

    DO j = m, 1, -1
      DO i = n, 1, -1
        dd_da = 1.0
        dd_db = c
        dd_dc = b(i, j)
        a_ad(i, j) = d_ad_(i, j) * dd_da
        b_ad(i, j) = d_ad_(i, j) * dd_db
        c_ad = d_ad_(i, j) * dd_dc + c_ad
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
    real :: dres_da
    real :: dres_db


    res_ad_ = res_ad

    DO i = n, 1, -1
      dres_dres = 1.0
      dres_da = b(i)
      dres_db = a(i)
      a_ad(i) = res_ad_ * dres_da
      b_ad(i) = res_ad_ * dres_db
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
    real :: b_ad_(n)
    real :: c_ad_(n)
    real :: dc_da
    real :: db_da

    a_ad(:) = 0.0

    b_ad_(:) = b_ad(:)
    c_ad_(:) = c_ad(:)

    DO i = n, 1, -1
      dc_da = 2 * a(idx(i))**1
      a_ad(i) = c_ad_(i) * dc_da + a_ad(i)
      db_da = 1.0
      a_ad(i) = b_ad_(i) * db_da + a_ad(i)
    END DO

    return
  end subroutine indirect_ad

  subroutine stencil_ad(n, a, a_ad, b_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b_ad(n)
    integer :: i
    real :: b_ad_(n)
    real :: db_da


    b_ad_(:) = b_ad(:)

    DO i = n, 1, -1
      db_da = 1.0 + 2.0 + 1.0 / 4.0
      a_ad(i) = b_ad_(i) * db_da
      IF (i == 1) THEN
      ELSE IF (i == n) THEN
      END IF
    END DO

    return
  end subroutine stencil_ad

end module array_examples_ad
