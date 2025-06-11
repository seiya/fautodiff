module intrinsic_func_ad
  implicit none

contains

  subroutine math_intrinsics_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y
    real, intent(inout) :: y_ad
    real, intent(in)  :: z_ad
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
    real :: df_dx
    real :: df_dy
    real :: dd_dx
    real :: dc_dx
    real :: dc_dy
    real :: db_dx
    real :: db_dy
    real :: da_dx

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
    df_dx = sinh(x) + 1.0 / cosh(x)**2
    df_dy = cosh(y)
    x_ad = f_ad * df_dx
    y_ad = f_ad * df_dy
    dd_dx = 1.0 / sqrt(1.0 - (x / pi)**2) * 1.0 / pi + 1.0 / (1.0 + (x)**2)
    x_ad = d_ad * dd_dx + x_ad
    dc_dx = cos(x) + 1.0 / cos(x)**2
    dc_dy = -sin(y)
    x_ad = c_ad * dc_dx + x_ad
    y_ad = c_ad * dc_dy + y_ad
    db_dx = exp(x) + 1.0 / (ABS(x) + 1.0 * log(10.0)) * sign(1.0, x)
    db_dy = 1.0 / y
    y_ad = b_ad * db_dy + y_ad
    x_ad = b_ad * db_dx + x_ad
    da_dx = 0.5 / sqrt(ABS(x)) * sign(1.0, x)
    x_ad = a_ad * da_dx + x_ad

    return
  end subroutine math_intrinsics_ad

  subroutine non_differentiable_intrinsics_ad(str, str_ad, arr, arr_ad, mat_in, mat_in_ad, mat_out_ad, idx_ad, lb_ad, ub_ad, x, x_ad, y_ad)
    character(len = *), intent(in)  :: str
    real, intent(out) :: str_ad
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

  subroutine casting_intrinsics_ad(i, i_ad, r, r_ad, d_ad, c, c_ad, n_ad)
    integer, intent(in)  :: i
    real, intent(out) :: i_ad
    real, intent(in)  :: r
    real, intent(out) :: r_ad
    real, intent(in)  :: d_ad
    character(len = 1), intent(inout) :: c
    real, intent(inout) :: c_ad
    real, intent(in)  :: n_ad


    return
  end subroutine casting_intrinsics_ad

end module intrinsic_func_ad
