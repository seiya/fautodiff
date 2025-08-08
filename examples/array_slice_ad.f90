module array_slice_ad
  use array_slice
  implicit none

contains

  subroutine slice_copy_fwd_ad(u, u_ad, n, m, i, j)
    real :: u(:)
    real :: u_ad(:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j

    u_ad(n:m) = u_ad(i:j) ! u(n:m) = u(i:j)
    u(n:m) = u(i:j)

    return
  end subroutine slice_copy_fwd_ad

  subroutine slice_copy_rev_ad(u, u_ad, n, m, i, j)
    real, intent(inout) :: u_ad(:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j
    integer :: i0_12
    real :: tmp_save_12_ad

    do i0_12 = 1, m - n + 1
      tmp_save_12_ad = u_ad(n + i0_12 - 1) ! u(n:m) = u(i:j)
      u_ad(n + i0_12 - 1) = 0.0 ! u(n:m) = u(i:j)
      u_ad(i + i0_12 - 1) = tmp_save_12_ad + u_ad(i + i0_12 - 1) ! u(n:m) = u(i:j)
    end do

    return
  end subroutine slice_copy_rev_ad

  subroutine slice_copy_ptr_fwd_ad(u, u_ad, n, m, i, j)
    real, target :: u(:)
    real, target :: u_ad(:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j
    real, pointer :: p_ad(:)
    real, pointer :: p(:)

    p_ad => u_ad ! p => u
    p => u
    u_ad(n:m) = p_ad(i:j) ! u(n:m) = p(i:j)
    u(n:m) = p(i:j)
    p_ad => null()
    p => null()

    return
  end subroutine slice_copy_ptr_fwd_ad

  subroutine slice_copy_ptr_rev_ad(u, u_ad, n, m, i, j)
    real, intent(inout), target :: u_ad(:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j
    real, pointer :: p_ad(:)
    integer :: i0_25
    real :: tmp_save_25_ad

    p_ad => u_ad
    do i0_25 = 1, m - n + 1
      tmp_save_25_ad = u_ad(n + i0_25 - 1) ! u(n:m) = p(i:j)
      u_ad(n + i0_25 - 1) = 0.0 ! u(n:m) = p(i:j)
      p_ad(i + i0_25 - 1) = tmp_save_25_ad + p_ad(i + i0_25 - 1) ! u(n:m) = p(i:j)
    end do
    p_ad => null() ! p => u

    return
  end subroutine slice_copy_ptr_rev_ad

  subroutine slice_copy_expr_fwd_ad(u, u_ad, v, v_ad, w, w_ad, n, m, i, j)
    real :: u(:)
    real :: u_ad(:)
    real :: v(:)
    real :: v_ad(:)
    real :: w(:)
    real :: w_ad(:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j

    u_ad(n:m) = v_ad(i:j) + w_ad(i:j) ! u(n:m) = v(i:j) + w(i:j)
    u(n:m) = v(i:j) + w(i:j)

    return
  end subroutine slice_copy_expr_fwd_ad

  subroutine slice_copy_expr_rev_ad(u, u_ad, v, v_ad, w, w_ad, n, m, i, j)
    real, intent(inout) :: u_ad(:)
    real, intent(inout) :: v_ad(:)
    real, intent(inout) :: w_ad(:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j

    v_ad(i:j) = u_ad(n:m) + v_ad(i:j) ! u(n:m) = v(i:j) + w(i:j)
    w_ad(i:j) = u_ad(n:m) + w_ad(i:j) ! u(n:m) = v(i:j) + w(i:j)
    u_ad(n:m) = 0.0 ! u(n:m) = v(i:j) + w(i:j)

    return
  end subroutine slice_copy_expr_rev_ad

end module array_slice_ad
