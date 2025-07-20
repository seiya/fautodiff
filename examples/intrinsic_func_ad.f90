module intrinsic_func_ad
  use intrinsic_func
  implicit none

contains

  subroutine math_intrinsics_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(inout) :: y
    real, intent(inout) :: y_ad
    real, intent(out) :: z
    real, intent(out) :: z_ad
    real :: a_ad
    real :: b_ad
    real :: c_ad
    real :: d_ad
    real :: e_ad
    real :: f_ad
    real :: g_ad
    real :: h_ad
    real :: o_ad
    real :: p_ad
    real :: q_ad
    real :: pi
    real :: a
    real :: b
    real :: c
    real :: d
    real :: e
    real :: f
    real :: g
    real :: h
    real :: o
    real :: p
    real :: q

    pi = acos(- 1.0)
    a_ad = x_ad * sign(1.0, x) / (2.0 * sqrt(abs(x))) ! a = sqrt(abs(x))
    a = sqrt(abs(x))
    b_ad = x_ad * (exp(x) + sign(1.0, x) / ((abs(x) + 1.0) * log(10.0))) + y_ad / y ! b = exp(x) + log(y) + log10(abs(x) + 1.0)
    b = exp(x) + log(y) + log10(abs(x) + 1.0)
    c_ad = x_ad * (cos(x) + 1.0 / cos(x)**2) - y_ad * sin(y) ! c = sin(x) + cos(y) + tan(x)
    c = sin(x) + cos(y) + tan(x)
    d_ad = x_ad * (1.0 / (pi * sqrt(1.0 - (x / pi)**2)) + 1.0 / (1.0 + x**2)) - y_ad / ((pi + 1.0) * sqrt(1.0 - (y / (pi + 1.0))**2)) ! d = asin(x / pi) + acos(y / (pi + 1.0)) + atan(x)
    d = asin(x / pi) + acos(y / (pi + 1.0)) + atan(x)
    e_ad = x_ad * (1.0 / sqrt(x**2 + 1.0) + 1.0 / (1.0 - x**2)) + y_ad / (sqrt(y - 1.0) * sqrt(y + 1.0)) ! e = asinh(x) + acosh(y) + atanh(x)
    e = asinh(x) + acosh(y) + atanh(x)
    f_ad = x_ad * (y / (x**2 + y**2) + sinh(x) + 1.0 / cosh(x)**2) + y_ad * (- x / (x**2 + y**2) + cosh(y)) ! f = atan2(x, y) + cosh(x) + sinh(y) + tanh(x)
    f = atan2(x, y) + cosh(x) + sinh(y) + tanh(x)
    g_ad = x_ad * sign(1.0, x) * sign(1.0, y) ! g = sign(x, y)
    g = sign(x, y)
    h_ad = x_ad * merge(1.0, 0.0, x <= y) + y_ad * merge(0.0, 1.0, x <= y) ! h = max(x, y)
    h = max(x, y)
    o_ad = x_ad * merge(1.0, 0.0, x >= y) + y_ad * merge(0.0, 1.0, x >= y) ! o = min(x, y)
    o = min(x, y)
    p_ad = x_ad * 2.0 / sqrt(acos(- 1.0)) * exp(- x**2) - y_ad * 2.0 / sqrt(acos(- 1.0)) * exp(- y**2) ! p = erf(x) + erfc(y)
    p = erf(x) + erfc(y)
    q_ad = x_ad - y_ad * real(int(x / y), kind(x)) ! q = mod(x, y)
    q = mod(x, y)
    z_ad = a_ad + b_ad + c_ad + d_ad + e_ad + f_ad + g_ad + h_ad + o_ad + p_ad + q_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    z = a + b + c + d + e + f + g + h + o + p + q

    return
  end subroutine math_intrinsics_fwd_ad

  subroutine math_intrinsics_rev_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y
    real, intent(inout) :: y_ad
    real, intent(inout) :: z_ad
    real :: a_ad
    real :: b_ad
    real :: c_ad
    real :: d_ad
    real :: e_ad
    real :: f_ad
    real :: g_ad
    real :: h_ad
    real :: o_ad
    real :: p_ad
    real :: q_ad
    real :: pi

    pi = acos(- 1.0)

    a_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    b_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    c_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    d_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    e_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    f_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    g_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    h_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    o_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    p_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    q_ad = z_ad ! z = a + b + c + d + e + f + g + h + o + p + q
    z_ad = 0.0 ! z = a + b + c + d + e + f + g + h + o + p + q
    x_ad = q_ad ! q = mod(x, y)
    y_ad = - q_ad * real(int(x / y), kind(x)) + y_ad ! q = mod(x, y)
    x_ad = p_ad * 2.0 / sqrt(acos(- 1.0)) * exp(- x**2) + x_ad ! p = erf(x) + erfc(y)
    y_ad = - p_ad * 2.0 / sqrt(acos(- 1.0)) * exp(- y**2) + y_ad ! p = erf(x) + erfc(y)
    x_ad = o_ad * merge(1.0, 0.0, x >= y) + x_ad ! o = min(x, y)
    y_ad = o_ad * merge(0.0, 1.0, x >= y) + y_ad ! o = min(x, y)
    x_ad = h_ad * merge(1.0, 0.0, x <= y) + x_ad ! h = max(x, y)
    y_ad = h_ad * merge(0.0, 1.0, x <= y) + y_ad ! h = max(x, y)
    x_ad = g_ad * sign(1.0, x) * sign(1.0, y) + x_ad ! g = sign(x, y)
    x_ad = f_ad * (y / (x**2 + y**2) + sinh(x) + 1.0 / cosh(x)**2) + x_ad ! f = atan2(x, y) + cosh(x) + sinh(y) + tanh(x)
    y_ad = f_ad * (- x / (x**2 + y**2) + cosh(y)) + y_ad ! f = atan2(x, y) + cosh(x) + sinh(y) + tanh(x)
    x_ad = e_ad * (1.0 / sqrt(x**2 + 1.0) + 1.0 / (1.0 - x**2)) + x_ad ! e = asinh(x) + acosh(y) + atanh(x)
    y_ad = e_ad / (sqrt(y - 1.0) * sqrt(y + 1.0)) + y_ad ! e = asinh(x) + acosh(y) + atanh(x)
    x_ad = d_ad * (1.0 / (pi * sqrt(1.0 - (x / pi)**2)) + 1.0 / (1.0 + x**2)) + x_ad ! d = asin(x / pi) + acos(y / (pi + 1.0)) + atan(x)
    y_ad = - d_ad / ((pi + 1.0) * sqrt(1.0 - (y / (pi + 1.0))**2)) + y_ad ! d = asin(x / pi) + acos(y / (pi + 1.0)) + atan(x)
    x_ad = c_ad * (cos(x) + 1.0 / cos(x)**2) + x_ad ! c = sin(x) + cos(y) + tan(x)
    y_ad = - c_ad * sin(y) + y_ad ! c = sin(x) + cos(y) + tan(x)
    x_ad = b_ad * (exp(x) + sign(1.0, x) / ((abs(x) + 1.0) * log(10.0))) + x_ad ! b = exp(x) + log(y) + log10(abs(x) + 1.0)
    y_ad = b_ad / y + y_ad ! b = exp(x) + log(y) + log10(abs(x) + 1.0)
    x_ad = a_ad * sign(1.0, x) / (2.0 * sqrt(abs(x))) + x_ad ! a = sqrt(abs(x))

    return
  end subroutine math_intrinsics_rev_ad

  subroutine non_differentiable_intrinsics_fwd_ad(str, arr, arr_ad, idx, lb, ub, x, x_ad, y, y_ad)
    character(len=*), intent(in)  :: str
    real, intent(in)  :: arr(:)
    real, intent(in)  :: arr_ad(:)
    integer, intent(out) :: idx
    integer, intent(out) :: lb
    integer, intent(out) :: ub
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad
    real :: a_ad
    real :: b_ad
    real :: c_ad
    real :: a
    real :: b
    real :: c

    idx = index(str, 'a')
    lb = lbound(arr, 1)
    ub = ubound(arr, 1)
    a_ad = 0.0 ! a = epsilon(x)
    a = epsilon(x)
    b_ad = 0.0 ! b = huge(x)
    b = huge(x)
    c_ad = 0.0 ! c = tiny(x)
    c = tiny(x)
    y_ad = a_ad + b_ad + c_ad ! y = a + b + c
    y = a + b + c

    return
  end subroutine non_differentiable_intrinsics_fwd_ad

  subroutine non_differentiable_intrinsics_rev_ad(str, arr, arr_ad, x, x_ad, y_ad)
    character(len=*), intent(in)  :: str
    real, intent(in)  :: arr(:)
    real, intent(out) :: arr_ad(:)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y_ad

    arr_ad(:) = 0.0

    y_ad = 0.0 ! y = a + b + c
    x_ad = 0.0 ! c = tiny(x)

    return
  end subroutine non_differentiable_intrinsics_rev_ad

  subroutine special_intrinsics_fwd_ad(mat_in, mat_in_ad, mat_out, mat_out_ad)
    real, intent(in)  :: mat_in(:,:)
    real, intent(in)  :: mat_in_ad(:,:)
    real, intent(out) :: mat_out(:,:)
    real, intent(out) :: mat_out_ad(:,:)

    mat_out_ad = transpose(mat_in_ad) ! mat_out = transpose(mat_in)
    mat_out = transpose(mat_in)
    mat_out_ad = cshift(mat_out_ad, - 1, 2) ! mat_out = cshift(mat_out, 1, 2)
    mat_out = cshift(mat_out, 1, 2)

    return
  end subroutine special_intrinsics_fwd_ad

  subroutine special_intrinsics_rev_ad(mat_in, mat_in_ad, mat_out_ad)
    real, intent(in)  :: mat_in(:,:)
    real, intent(out) :: mat_in_ad(:,:)
    real, intent(inout) :: mat_out_ad(:,:)

    mat_out_ad = cshift(mat_out_ad, - 1, 2) ! mat_out = cshift(mat_out, 1, 2)
    mat_in_ad = transpose(mat_out_ad) ! mat_out = transpose(mat_in)

    return
  end subroutine special_intrinsics_rev_ad

  subroutine casting_intrinsics_fwd_ad(i, r, r_ad, d, d_ad, c, n)
    integer, intent(in)  :: i
    real, intent(in)  :: r
    real, intent(in)  :: r_ad
    double precision, intent(out) :: d
    double precision, intent(out) :: d_ad
    character(len=1), intent(inout) :: c
    integer, intent(out) :: n
    integer :: i2

    i2 = int(r)
    d_ad = r_ad ! d = dble(r) + dble(i2)
    d = dble(r) + dble(i2)
    n = nint(r)
    c = achar(ichar(c) + i2)

    return
  end subroutine casting_intrinsics_fwd_ad

  subroutine casting_intrinsics_rev_ad(i, r, r_ad, d_ad, c)
    integer, intent(in)  :: i
    real, intent(in)  :: r
    real, intent(out) :: r_ad
    double precision, intent(inout) :: d_ad
    character(len=1), intent(inout) :: c

    r_ad = d_ad ! d = dble(r) + dble(i2)
    d_ad = 0.0d0 ! d = dble(r) + dble(i2)

    return
  end subroutine casting_intrinsics_rev_ad

end module intrinsic_func_ad
