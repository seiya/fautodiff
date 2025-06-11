module intrinsic_func
  implicit none
contains
  subroutine math_intrinsics(x, y, z)
    real, intent(in) :: x
    real, intent(inout) :: y
    real, intent(out) :: z
    real :: pi
    pi = acos(-1.0)
    y = sqrt(abs(x)) + exp(y) + log(x) + log10(abs(y) + 1.0)
    y = y + sin(x) + cos(y) + tan(x)
    y = y + asin(x / pi) + acos(y / (pi + 1.0)) + atan(x)
    y = y + atan2(x, y) + cosh(x) + sinh(y) + tanh(x)
    y = y + sign(x, y) + max(x, y) + min(x, y)
    z = mod(x, y) + epsilon(x) + huge(x) / tiny(x)
    return
  end subroutine math_intrinsics

  subroutine non_math_intrinsics(str, arr, mat, idx, lb, ub)
    character(len=*), intent(in) :: str
    real, intent(in) :: arr(:)
    real, allocatable, intent(out) :: mat(:,:)
    integer, intent(out) :: idx, lb, ub
    integer :: n, len_trimmed

    len_trimmed = len_trim(adjustl(str))
    idx = index(str, 'a')
    lb = lbound(arr, 1)
    ub = ubound(arr, 1)
    n = size(arr)

    allocate(mat(2, n))
    mat(1, :) = arr
    mat(2, :) = arr
    mat = transpose(mat)
    mat = cshift(mat, 1, 2)
  end subroutine non_math_intrinsics

  subroutine casting_intrinsics(i, r, d, c, n)
    integer, intent(in) :: i
    real, intent(in) :: r
    double precision, intent(out) :: d
    character(len=1), intent(inout) :: c
    integer, intent(out) :: n
    integer :: i2
    real :: r2

    i2 = int(r)
    r2 = real(i)
    d = dble(r) + dble(i2)
    n = nint(r)
    c = achar(ichar(c) + i2)
    return
  end subroutine casting_intrinsics
end module intrinsic_func

