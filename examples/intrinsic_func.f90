module intrinsic_func
  implicit none

contains

  subroutine math_intrinsics(x, y, z)
    real, intent(in) :: x
    real, intent(inout) :: y
    real, intent(out) :: z
    real :: pi
    real :: a, b, c, d, e, f, g, h
    real :: o, p, q

    pi = acos(-1.0)
    a = sqrt(abs(x))
    b = exp(x) + log(y) + log10(abs(x) + 1.0)
    c = sin(x) + cos(y) + tan(x)
    d = asin(x / pi) + acos(y / (pi + 1.0)) + atan(x)
    e = asinh(x) + acosh(y) + atanh(x)
    f = atan2(x, y) + cosh(x) + sinh(y) + tanh(x)
    g = sign(x, y)
    h = max(x, y)
    o = min(x, y)
    p = erf(x) + erfc(y)
    q = mod(x, y)
    z = a + b + c + d + e + f + g + h + o + p + q

    return
  end subroutine math_intrinsics

  subroutine reduction(x, a, b, c, d)
    real, intent(in) :: x(:)
    real, intent(out) :: a, b, c, d

    a = sum(x)
    b = sum(x(:))
    c = minval(x)
    d = maxval(x)

    return
  end subroutine reduction

  subroutine non_differentiable_intrinsics(str, arr, idx, lb, ub, x, y)
    character(len=*), intent(in) :: str
    real, intent(in) :: arr(:)
    integer, intent(out) :: idx, lb, ub
    real, intent(in) :: x
    real, intent(out) :: y
    integer :: n, len_trimmed
    real :: a
    real :: b
    real :: c

    len_trimmed = len_trim(adjustl(str))
    idx = index(str, 'a')
    lb = lbound(arr, 1)
    ub = ubound(arr, 1)
    n = size(arr)

    a = epsilon(x)
    b = huge(x)
    c = tiny(x)
    y = a + b + c

    return
  end subroutine non_differentiable_intrinsics

  subroutine special_intrinsics(mat_in, mat_out)
    real, intent(in)  :: mat_in(:,:)
    real, intent(out) :: mat_out(:,:)

    mat_out = transpose(mat_in)
    mat_out = cshift(mat_out, 1, 2)

    return
  end subroutine special_intrinsics

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

