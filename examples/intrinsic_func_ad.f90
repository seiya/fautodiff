module intrinsic_func_ad
  implicit none

contains

  subroutine math_intrinsics_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y
    real, intent(inout) :: y_ad
    real, intent(in)  :: z_ad
    real :: pi
    real :: dz_da
    real :: a_ad
    real :: dz_db
    real :: b_ad
    real :: dz_dc
    real :: c_ad
    real :: dz_dd
    real :: d_ad
    real :: dz_de
    real :: e_ad
    real :: dz_df
    real :: f_ad
    real :: dz_dg
    real :: g_ad
    real :: dz_dh
    real :: h_ad
    real :: dz_do
    real :: o_ad
    real :: dz_dp
    real :: p_ad
    real :: dz_dq
    real :: q_ad
    real :: dq_dx
    real :: dq_dy
    real :: dp_dx
    real :: dp_dy
    real :: do_dx
    real :: do_dy
    real :: dh_dx
    real :: dh_dy
    real :: dg_dx
    real :: df_dx
    real :: df_dy
    real :: de_dx
    real :: de_dy
    real :: dd_dx
    real :: dd_dy
    real :: dc_dx
    real :: dc_dy
    real :: db_dx
    real :: db_dy
    real :: da_dx

    pi = ACOS(- 1.0)

    dz_da = 1.0
    dz_db = 1.0
    dz_dc = 1.0
    dz_dd = 1.0
    dz_de = 1.0
    dz_df = 1.0
    dz_dg = 1.0
    dz_dh = 1.0
    dz_do = 1.0
    dz_dp = 1.0
    dz_dq = 1.0
    q_ad = z_ad * dz_dq
    p_ad = z_ad * dz_dp
    o_ad = z_ad * dz_do
    h_ad = z_ad * dz_dh
    g_ad = z_ad * dz_dg
    f_ad = z_ad * dz_df
    e_ad = z_ad * dz_de
    d_ad = z_ad * dz_dd
    c_ad = z_ad * dz_dc
    b_ad = z_ad * dz_db
    a_ad = z_ad * dz_da
    dq_dx = 1.0
    dq_dy = -real(int(x / y), kind(x))
    y_ad = q_ad * dq_dy
    x_ad = q_ad * dq_dx
    dp_dx = 2.0 / sqrt(acos(-1.0)) * exp(-(x)**2)
    dp_dy = -2.0 / sqrt(acos(-1.0)) * exp(-(y)**2)
    y_ad = p_ad * dp_dy + y_ad
    x_ad = p_ad * dp_dx + x_ad
    do_dx = merge(1.0, 0.0, x <= y)
    do_dy = merge(0.0, 1.0, x <= y)
    y_ad = o_ad * do_dy + y_ad
    x_ad = o_ad * do_dx + x_ad
    dh_dx = merge(1.0, 0.0, x >= y)
    dh_dy = merge(0.0, 1.0, x >= y)
    y_ad = h_ad * dh_dy + y_ad
    x_ad = h_ad * dh_dx + x_ad
    dg_dx = sign(1.0, x) * sign(1.0, y)
    x_ad = g_ad * dg_dx + x_ad
    df_dx = y / (x**2 + y**2) + sinh(x) + 1.0 / cosh(x)**2
    df_dy = -x / (x**2 + y**2) + cosh(y)
    x_ad = f_ad * df_dx + x_ad
    y_ad = f_ad * df_dy + y_ad
    de_dx = 1.0 / sqrt((x)**2 + 1.0) + 1.0 / (1.0 - (x)**2)
    de_dy = 1.0 / sqrt(y - 1.0) / sqrt(y + 1.0)
    x_ad = e_ad * de_dx + x_ad
    y_ad = e_ad * de_dy + y_ad
    dd_dx = 1.0 / sqrt(1.0 - (x / pi)**2) * 1.0 / pi + 1.0 / (1.0 + (x)**2)
    dd_dy = -1.0 / sqrt(1.0 - (y / (pi + 1.0))**2) * 1.0 / (pi + 1.0)
    x_ad = d_ad * dd_dx + x_ad
    y_ad = d_ad * dd_dy + y_ad
    dc_dx = cos(x) + 1.0 / cos(x)**2
    dc_dy = -sin(y)
    x_ad = c_ad * dc_dx + x_ad
    y_ad = c_ad * dc_dy + y_ad
    db_dx = exp(x) + 1.0 / ((ABS(x) + 1.0) * log(10.0)) * sign(1.0, x)
    db_dy = 1.0 / y
    x_ad = b_ad * db_dx + x_ad
    y_ad = b_ad * db_dy + y_ad
    da_dx = 0.5 / sqrt(ABS(x)) * sign(1.0, x)
    x_ad = a_ad * da_dx + x_ad

    return
  end subroutine math_intrinsics_ad

  subroutine non_differentiable_intrinsics_ad(str, arr, arr_ad, mat_in, mat_in_ad, mat_out_ad, idx_ad, lb_ad, ub_ad, x, x_ad, y_ad)
    character(len = *), intent(in)  :: str
    real, intent(in)  :: arr
    real, intent(out) :: arr_ad
    real, intent(in)  :: mat_in
    real, intent(out) :: mat_in_ad
    real, intent(in)  :: mat_out_ad
    real, intent(in)  :: idx_ad
    real, intent(in)  :: lb_ad
    real, intent(in)  :: ub_ad
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: y_ad
    real :: dy_da
    real :: a_ad
    real :: dy_db
    real :: b_ad
    real :: dy_dc
    real :: c_ad

    dy_da = 1.0
    dy_db = 1.0
    dy_dc = 1.0
    c_ad = y_ad * dy_dc
    b_ad = y_ad * dy_db
    a_ad = y_ad * dy_da

    return
  end subroutine non_differentiable_intrinsics_ad

  subroutine special_intrinsics_ad(mat_in, mat_in_ad, mat_out_ad)
    real, intent(in)  :: mat_in
    real, intent(out) :: mat_in_ad
    real, intent(in)  :: mat_out_ad
    real :: mat_out_ad_

    mat_out_ad_ = cshift(mat_out_ad, -1, 2)
    mat_in_ad = transpose(mat_out_ad_)

    return
  end subroutine special_intrinsics_ad

  subroutine casting_intrinsics_ad(i, i_ad, r, r_ad, d_ad, c, n_ad)
    integer, intent(in)  :: i
    real, intent(out) :: i_ad
    real, intent(in)  :: r
    real, intent(out) :: r_ad
    real, intent(in)  :: d_ad
    character(len = 1), intent(inout) :: c
    real, intent(in)  :: n_ad
    real :: dd_dr
    real :: dd_di2
    real :: i2_ad
    real :: dr2_di

    dd_dr = 1.0
    dd_di2 = 1.0
    i2_ad = d_ad * dd_di2
    r_ad = d_ad * dd_dr
    dr2_di = 1.0
    i_ad = r2_ad * dr2_di

    return
  end subroutine casting_intrinsics_ad

end module intrinsic_func_ad
