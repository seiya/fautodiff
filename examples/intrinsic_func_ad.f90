module intrinsic_func_ad
  implicit none

contains

  subroutine math_intrinsics_ad(x, x_ad, y, y_ad, z_ad)
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
  end subroutine math_intrinsics_ad

  subroutine non_differentiable_intrinsics_ad(str, arr, arr_ad, x, x_ad, y_ad)
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
  end subroutine non_differentiable_intrinsics_ad

  subroutine special_intrinsics_ad(mat_in, mat_in_ad, mat_out_ad)
    real, intent(in)  :: mat_in(:,:)
    real, intent(out) :: mat_in_ad(:,:)
    real, intent(inout) :: mat_out_ad(:,:)

    mat_out_ad = cshift(mat_out_ad, - 1, 2) ! mat_out = cshift(mat_out, 1, 2)
    mat_in_ad = transpose(mat_out_ad) ! mat_out = transpose(mat_in)

    return
  end subroutine special_intrinsics_ad

  subroutine casting_intrinsics_ad(i, r, r_ad, d_ad, c)
    integer, intent(in)  :: i
    real, intent(in)  :: r
    real, intent(out) :: r_ad
    double precision, intent(inout) :: d_ad
    character(len=1), intent(inout) :: c

    r_ad = d_ad ! d = dble(r) + dble(i2)
    d_ad = 0.0d0 ! d = dble(r) + dble(i2)

    return
  end subroutine casting_intrinsics_ad

end module intrinsic_func_ad
