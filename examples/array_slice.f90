
module array_slice

contains
  subroutine slice_copy(u, n, m, i, j)
    integer, intent(in) :: n, m, i, j
    real, intent(inout) :: u(:)

    u(n:m) = u(i:j)

    return
  end subroutine slice_copy

  subroutine slice_copy_ptr(u, v, n, m, i, j)
    integer, intent(in) :: n, m, i, j
    real, intent(inout), target :: u(:), v(:)
    real, pointer :: p(:), q(:)
    integer :: k

    p => u
    q => v
    do k = n, m
      u(k) = p(i+k-n)
    end do
    v(n:m) = p(i:j) + q(i:j)
    u(n:m) = u(n:m) + q(i:j)

    return
  end subroutine slice_copy_ptr

  subroutine slice_copy_expr(u, v, w, n, m, i, j, l1, l2)
    integer, intent(in) :: n, m, i, j, l1, l2
    real, intent(inout) :: u(:,:)
    real, intent(in) :: v(:,:)
    real, intent(inout) :: w(:,:)
    integer :: k1, k2

    u(n:n+l1-1,m:m+l2-1) = v(i:i+l1-1,j:j+l2-1) * u(i:i+l1-1,j:j+l2-1)
    do k2 = 1, l2
      do k1 = 1, l1
        w(n+k1-1,m+k2-1) = w(i+k1-1,j+k2-1) * v(i+k1-1,j+k2-1)
      end do
    end do

    return
  end subroutine slice_copy_expr
end module array_slice
