module array_examples_ad
  implicit none

contains

  subroutine elementwise_add_ad(a, a_ad, b, b_ad, c_ad, n)
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(out) :: b_ad(n)
    real, intent(in)  :: c_ad(n)
    integer, intent(in)  :: n
    real :: dc_da(n)
    real :: dc_db(n)


    dc_da(:) = 1.0
    dc_db(:) = 1.0
    b_ad(:) = c_ad * dc_db
    a_ad(:) = c_ad * dc_da

    return
  end subroutine elementwise_add_ad

  subroutine scale_array_ad(a, a_ad, n)
    real, intent(inout) :: a(n)
    real, intent(inout) :: a_ad(n)
    integer, intent(in)  :: n

    return
  end subroutine scale_array_ad

  subroutine dot_product_ad(a, a_ad, b, b_ad, n, res_ad)
    real, intent(in)  :: a(n)
    real, intent(out) :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(out) :: b_ad(n)
    integer, intent(in)  :: n
    real, intent(in)  :: res_ad
    real :: dres_dres
    real :: dres_da
    real :: dres_db
    real :: res_ad_


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

end module array_examples_ad
