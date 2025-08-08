
module array_slice
contains
  subroutine slice_copy(u, n, m, i, j)
    integer, intent(in) :: n, m, i, j
    real :: u(:)
    u(n:m) = u(i:j)
  end subroutine slice_copy

  subroutine slice_copy_ptr(u, n, m, i, j)
    integer, intent(in) :: n, m, i, j
    real, target :: u(:)
    real, pointer :: p(:)
    p => u
    u(n:m) = p(i:j)
  end subroutine slice_copy_ptr

  subroutine slice_copy_expr(u, v, w, n, m, i, j)
    integer, intent(in) :: n, m, i, j
    real :: u(:)
    real :: v(:)
    real :: w(:)
    u(n:m) = v(i:j) + w(i:j)
  end subroutine slice_copy_expr
end module array_slice
