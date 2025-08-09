module self_reference_ad
  use self_reference
  implicit none

contains

  subroutine self_ref_slice_fwd_ad(u, u_ad, n, m, i, j)
    real, intent(inout) :: u(:)
    real, intent(inout) :: u_ad(:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j

    u_ad(n:m) = u_ad(i:j) ! u(n:m) = u(i:j)
    u(n:m) = u(i:j)

    return
  end subroutine self_ref_slice_fwd_ad

  subroutine self_ref_slice_rev_ad(u_ad, n, m, i)
    real, intent(inout) :: u_ad(:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer :: n1_12_ad
    real :: tmp_save_12_ad

    do n1_12_ad = n, m
      tmp_save_12_ad = u_ad(n1_12_ad) ! u(n:m) = u(i:j)
      u_ad(n1_12_ad) = 0.0 ! u(n:m) = u(i:j)
      u_ad(i + n1_12_ad - n) = tmp_save_12_ad + u_ad(i + n1_12_ad - n) ! u(n:m) = u(i:j)
    end do

    return
  end subroutine self_ref_slice_rev_ad

  subroutine self_ref_slice_ptr_fwd_ad(u, u_ad, v, v_ad, n, m, i, j)
    real, intent(inout), target :: u(:)
    real, intent(inout), target :: u_ad(:)
    real, intent(inout), target :: v(:)
    real, intent(inout), target :: v_ad(:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j
    real, pointer :: p_ad(:)
    real, pointer :: q_ad(:)
    real, pointer :: p(:)
    real, pointer :: q(:)
    integer :: k

    p_ad => u_ad ! p => u
    p => u
    q_ad => v_ad ! q => v
    q => v
    do k = n, m
      u_ad(k) = p_ad(i + k - n) ! u(k) = p(i+k-n)
      u(k) = p(i + k - n)
    end do
    v_ad(n:m) = p_ad(i:j) + q_ad(i:j) ! v(n:m) = p(i:j) + q(i:j)
    v(n:m) = p(i:j) + q(i:j)
    u_ad(n:m) = u_ad(n:m) + q_ad(i:j) ! u(n:m) = u(n:m) + q(i:j)
    u(n:m) = u(n:m) + q(i:j)

    return
  end subroutine self_ref_slice_ptr_fwd_ad

  subroutine self_ref_slice_ptr_rev_ad(u_ad, v_ad, n, m, i, j)
    real, intent(inout), target :: u_ad(:)
    real, intent(inout), target :: v_ad(:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j
    real, pointer :: p_ad(:)
    real, pointer :: q_ad(:)
    integer :: k
    real :: tmp_save_31_ad
    integer :: n1_33_ad
    real :: tmp_save_33_ad

    q_ad => v_ad
    p_ad => u_ad
    q_ad(i:j) = u_ad(n:m) + q_ad(i:j) ! u(n:m) = u(n:m) + q(i:j)
    do n1_33_ad = n, m
      tmp_save_33_ad = v_ad(n1_33_ad) ! v(n:m) = p(i:j) + q(i:j)
      v_ad(n1_33_ad) = 0.0 ! v(n:m) = p(i:j) + q(i:j)
      p_ad(i + n1_33_ad - n) = tmp_save_33_ad + p_ad(i + n1_33_ad - n) ! v(n:m) = p(i:j) + q(i:j)
      q_ad(i + n1_33_ad - n) = tmp_save_33_ad + q_ad(i + n1_33_ad - n) ! v(n:m) = p(i:j) + q(i:j)
    end do
    do k = m, n, - 1
      tmp_save_31_ad = u_ad(k) ! u(k) = p(i+k-n)
      u_ad(k) = 0.0 ! u(k) = p(i+k-n)
      p_ad(i + k - n) = tmp_save_31_ad + p_ad(i + k - n) ! u(k) = p(i+k-n)
    end do
    q_ad => null() ! q => v
    p_ad => null() ! p => u

    return
  end subroutine self_ref_slice_ptr_rev_ad

  subroutine self_ref_slice_expr_fwd_ad(u, u_ad, v, v_ad, w, w_ad, n, m, i, j, l1, l2)
    real, intent(inout) :: u(:,:)
    real, intent(inout) :: u_ad(:,:)
    real, intent(in)  :: v(:,:)
    real, intent(in)  :: v_ad(:,:)
    real, intent(inout) :: w(:,:)
    real, intent(inout) :: w_ad(:,:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j
    integer, intent(in)  :: l1
    integer, intent(in)  :: l2
    integer :: k1
    integer :: k2

    u_ad(n:n + l1 - 1,m:m + l2 - 1) = v_ad(i:i + l1 - 1,j:j + l2 - 1) * u(i:i + l1 - 1,j:j + l2 - 1) + u_ad(i:i + l1 - 1,j:j + l2 - 1) * v(i:i + l1 - 1,j:j + l2 - 1) ! u(n:n+l1-1,m:m+l2-1) = v(i:i+l1-1,j:j+l2-1) * u(i:i+l1-1,j:j+l2-1)
    u(n:n + l1 - 1,m:m + l2 - 1) = v(i:i + l1 - 1,j:j + l2 - 1) * u(i:i + l1 - 1,j:j + l2 - 1)
    do k2 = 1, l2
      do k1 = 1, l1
        w_ad(n + k1 - 1,m + k2 - 1) = w_ad(i + k1 - 1,j + k2 - 1) * v(i + k1 - 1,j + k2 - 1) + v_ad(i + k1 - 1,j + k2 - 1) * w(i + k1 - 1,j + k2 - 1) ! w(n+k1-1,m+k2-1) = w(i+k1-1,j+k2-1) * v(i+k1-1,j+k2-1)
        w(n + k1 - 1,m + k2 - 1) = w(i + k1 - 1,j + k2 - 1) * v(i + k1 - 1,j + k2 - 1)
      end do
    end do

    return
  end subroutine self_ref_slice_expr_fwd_ad

  subroutine self_ref_slice_expr_rev_ad(u, u_ad, v, v_ad, w, w_ad, n, m, i, j, l1, l2)
    real, intent(inout) :: u(:,:)
    real, intent(inout) :: u_ad(:,:)
    real, intent(in)  :: v(:,:)
    real, intent(inout) :: v_ad(:,:)
    real, intent(inout) :: w(:,:)
    real, intent(inout) :: w_ad(:,:)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: i
    integer, intent(in)  :: j
    integer, intent(in)  :: l1
    integer, intent(in)  :: l2
    integer :: k1
    integer :: k2
    integer :: n2_52_ad
    integer :: n1_52_ad
    real :: tmp_save_52_ad
    real :: tmp_save_55_ad

    do k2 = l2, 1, - 1
      do k1 = l1, 1, - 1
        tmp_save_55_ad = w_ad(n + k1 - 1,m + k2 - 1) ! w(n+k1-1,m+k2-1) = w(i+k1-1,j+k2-1) * v(i+k1-1,j+k2-1)
        w_ad(n + k1 - 1,m + k2 - 1) = 0.0 ! w(n+k1-1,m+k2-1) = w(i+k1-1,j+k2-1) * v(i+k1-1,j+k2-1)
        w_ad(i + k1 - 1,j + k2 - 1) = tmp_save_55_ad * v(i + k1 - 1,j + k2 - 1) + w_ad(i + k1 - 1,j + k2 - 1) ! w(n+k1-1,m+k2-1) = w(i+k1-1,j+k2-1) * v(i+k1-1,j+k2-1)
        v_ad(i + k1 - 1,j + k2 - 1) = tmp_save_55_ad * w(i + k1 - 1,j + k2 - 1) + v_ad(i + k1 - 1,j + k2 - 1) ! w(n+k1-1,m+k2-1) = w(i+k1-1,j+k2-1) * v(i+k1-1,j+k2-1)
      end do
    end do
    do n2_52_ad = m, m + l2 - 1
      do n1_52_ad = n, n + l1 - 1
        tmp_save_52_ad = u_ad(n1_52_ad,n2_52_ad) ! u(n:n+l1-1,m:m+l2-1) = v(i:i+l1-1,j:j+l2-1) * u(i:i+l1-1,j:j+l2-1)
        u_ad(n1_52_ad,n2_52_ad) = 0.0 ! u(n:n+l1-1,m:m+l2-1) = v(i:i+l1-1,j:j+l2-1) * u(i:i+l1-1,j:j+l2-1)
        v_ad(i + n1_52_ad - n,j + n2_52_ad - m) = tmp_save_52_ad * u(i + n1_52_ad - n,j + n2_52_ad - m) + v_ad(i + n1_52_ad - n,j + n2_52_ad - m) ! u(n:n+l1-1,m:m+l2-1) = v(i:i+l1-1,j:j+l2-1) * u(i:i+l1-1,j:j+l2-1)
        u_ad(i + n1_52_ad - n,j + n2_52_ad - m) = tmp_save_52_ad * v(i + n1_52_ad - n,j + n2_52_ad - m) + u_ad(i + n1_52_ad - n,j + n2_52_ad - m) ! u(n:n+l1-1,m:m+l2-1) = v(i:i+l1-1,j:j+l2-1) * u(i:i+l1-1,j:j+l2-1)
      end do
    end do

    return
  end subroutine self_ref_slice_expr_rev_ad

end module self_reference_ad
