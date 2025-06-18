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

    a_ad = z_ad
    b_ad = z_ad
    c_ad = z_ad
    d_ad = z_ad
    e_ad = z_ad
    f_ad = z_ad
    g_ad = z_ad
    h_ad = z_ad
    o_ad = z_ad
    p_ad = z_ad
    q_ad = z_ad
    z_ad = 0.0
    x_ad = q_ad
    y_ad = - q_ad * real(int(x / y), kind(x)) + y_ad
    x_ad = p_ad * 2.0 / sqrt(acos(- 1.0)) * exp(- x**2) + x_ad
    y_ad = - p_ad * 2.0 / sqrt(acos(- 1.0)) * exp(- y**2) + y_ad
    x_ad = o_ad * merge(1.0, 0.0, x >= y) + x_ad
    y_ad = o_ad * merge(0.0, 1.0, x >= y) + y_ad
    x_ad = h_ad * merge(1.0, 0.0, x <= y) + x_ad
    y_ad = h_ad * merge(0.0, 1.0, x <= y) + y_ad
    x_ad = g_ad * sign(1.0, x) * sign(1.0, y) + x_ad
    x_ad = f_ad * (y / (x**2 + y**2) + sinh(x) + 1.0 / cosh(x)**2) + x_ad
    y_ad = f_ad * (- x / (x**2 + y**2) + cosh(y)) + y_ad
    x_ad = e_ad * (1.0 / sqrt(x**2 + 1.0) + 1.0 / (1.0 - x**2)) + x_ad
    y_ad = e_ad / (sqrt(y - 1.0) * sqrt(y + 1.0)) + y_ad
    x_ad = d_ad * (1.0 / (pi * sqrt(1.0 - (x / pi)**2)) + 1.0 / (1.0 + x**2)) + x_ad
    y_ad = - d_ad / ((pi + 1.0) * sqrt(1.0 - (y / (pi + 1.0))**2)) + y_ad
    x_ad = c_ad * (cos(x) + 1.0 / cos(x)**2) + x_ad
    y_ad = - c_ad * sin(y) + y_ad
    x_ad = b_ad * (exp(x) + sign(1.0, x) / ((abs(x) + 1.0) * log(10.0))) + x_ad
    y_ad = b_ad / y + y_ad
    x_ad = a_ad * sign(1.0, x) / (2.0 * sqrt(abs(x))) + x_ad

    return
  end subroutine math_intrinsics_ad

  subroutine non_differentiable_intrinsics_ad(str, arr, arr_ad, x, x_ad, y_ad)
    character(len=*), intent(in)  :: str
    real, intent(in)  :: arr(:)
    real, intent(out) :: arr_ad(:)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y_ad

    y_ad = 0.0
    x_ad = 0.0

    return
  end subroutine non_differentiable_intrinsics_ad

  subroutine special_intrinsics_ad(mat_in, mat_in_ad, mat_out_ad)
    real, intent(in)  :: mat_in(:,:)
    real, intent(out) :: mat_in_ad(:,:)
    real, intent(inout) :: mat_out_ad(:,:)

    mat_out_ad = cshift(mat_out_ad, - 1, 2)
    mat_in_ad = transpose(mat_out_ad)

    return
  end subroutine special_intrinsics_ad

  subroutine casting_intrinsics_ad(i, r, r_ad, d_ad, c)
    integer, intent(in)  :: i
    real, intent(in)  :: r
    real, intent(out) :: r_ad
    double precision, intent(inout) :: d_ad
    character(len=1), intent(inout) :: c

    r_ad = d_ad
    d_ad = 0.0d0

    return
  end subroutine casting_intrinsics_ad

end module intrinsic_func_ad
