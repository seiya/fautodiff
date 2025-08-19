module allocate_vars_ad
  use allocate_vars
  implicit none

  real, allocatable :: mod_arr_diff_ad(:)

contains

  subroutine allocate_and_sum_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    real, allocatable :: arr_ad(:)
    integer :: i
    real, allocatable :: arr(:)

    allocate(arr(n))
    allocate(arr_ad(n))
    do i = 1, n
      arr_ad(i) = x_ad * i ! arr(i) = i * x
      arr(i) = i * x
    end do
    res_ad = 0.0 ! res = 0.0
    res = 0.0
    do i = 1, n
      res_ad = res_ad + arr_ad(i) * x + x_ad * arr(i) ! res = res + arr(i) * x
      res = res + arr(i) * x
    end do
    deallocate(arr_ad)
    deallocate(arr)

    return
  end subroutine allocate_and_sum_fwd_ad

  subroutine allocate_and_sum_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: res_ad
    real, allocatable :: arr_ad(:)
    integer :: i
    real, allocatable :: arr(:)

    allocate(arr(n))
    do i = 1, n
      arr(i) = i * x
    end do

    allocate(arr_ad(n))
    do i = n, 1, - 1
      arr_ad(i) = res_ad * x ! res = res + arr(i) * x
      x_ad = res_ad * arr(i) + x_ad ! res = res + arr(i) * x
    end do
    res_ad = 0.0 ! res = 0.0
    do i = n, 1, - 1
      x_ad = arr_ad(i) * i + x_ad ! arr(i) = i * x
    end do
    deallocate(arr_ad)
    deallocate(arr)

    return
  end subroutine allocate_and_sum_rev_ad

  subroutine save_alloc_fwd_ad(n, x, x_ad, y, y_ad, z, z_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(in)  :: x_ad(n)
    real, intent(in)  :: y
    real, intent(in)  :: y_ad
    real, intent(out) :: z
    real, intent(out) :: z_ad
    real, allocatable :: htmp_ad(:)
    real, allocatable :: htmp(:)
    integer :: i

    allocate(htmp(n))
    allocate(htmp_ad(n))
    htmp_ad = x_ad ! htmp = x
    htmp = x
    z_ad = 0.0 ! z = 0.0
    z = 0.0
    do i = 1, n
      z_ad = z_ad + htmp_ad(i) * y + y_ad * htmp(i) ! z = z + htmp(i) * y
      z = z + htmp(i) * y
    end do
    htmp_ad = x_ad * 2.0 * x ! htmp = x**2
    htmp = x**2
    do i = 1, n
      z_ad = z_ad + htmp_ad(i) * y + y_ad * htmp(i) ! z = z + htmp(i) * y
      z = z + htmp(i) * y
    end do
    deallocate(htmp_ad)
    deallocate(htmp)

    return
  end subroutine save_alloc_fwd_ad

  subroutine save_alloc_rev_ad(n, x, x_ad, y, y_ad, z_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(inout) :: x_ad(n)
    real, intent(in)  :: y
    real, intent(inout) :: y_ad
    real, intent(inout) :: z_ad
    real, allocatable :: htmp_ad(:)
    real, allocatable :: htmp(:)
    integer :: i
    real, allocatable :: htmp_save_42_ad(:)

    allocate(htmp(n))
    htmp = x
    allocate(htmp_save_42_ad, mold=htmp)
    htmp_save_42_ad(1:n) = htmp(1:n)
    htmp = x**2

    allocate(htmp_ad(n))
    do i = n, 1, - 1
      htmp_ad(i) = z_ad * y ! z = z + htmp(i) * y
      y_ad = z_ad * htmp(i) + y_ad ! z = z + htmp(i) * y
    end do
    if (.not. allocated(htmp)) then
      allocate(htmp, mold=htmp_save_42_ad)
    end if
    htmp(1:n) = htmp_save_42_ad(1:n)
    x_ad = htmp_ad * 2.0 * x + x_ad ! htmp = x**2
    do i = n, 1, - 1
      htmp_ad(i) = z_ad * y ! z = z + htmp(i) * y
      y_ad = z_ad * htmp(i) + y_ad ! z = z + htmp(i) * y
    end do
    z_ad = 0.0 ! z = 0.0
    x_ad = htmp_ad + x_ad ! htmp = x
    deallocate(htmp_ad)
    deallocate(htmp)

    return
  end subroutine save_alloc_rev_ad

  subroutine module_vars_init_fwd_ad(n, x, x_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    integer :: i

    if (.not. allocated(mod_arr_diff_ad)) then
      allocate(mod_arr_diff_ad(n))
    end if
    do i = 1, n
      mod_arr(i) = i * 2.0
      mod_arr_diff_ad(i) = x_ad * i ! mod_arr_diff(i) = i * x
      mod_arr_diff(i) = i * x
    end do

    return
  end subroutine module_vars_init_fwd_ad

  subroutine module_vars_init_rev_ad(n, x_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: x_ad
    integer :: i

    do i = n, 1, - 1
      x_ad = mod_arr_diff_ad(i) * i + x_ad ! mod_arr_diff(i) = i * x
    end do
    if (allocated(mod_arr_diff_ad)) then
      deallocate(mod_arr_diff_ad)
    end if

    return
  end subroutine module_vars_init_rev_ad

  subroutine module_vars_main_fwd_ad(n, x, x_ad)
    integer, intent(in)  :: n
    real, intent(out) :: x
    real, intent(out) :: x_ad
    integer :: i

    x_ad = 0.0 ! x = 0.0
    x = 0.0
    do i = 1, n
      mod_arr_diff_ad(i) = mod_arr_diff_ad(i) * (2.0 + i) ! mod_arr_diff(i) = mod_arr_diff(i) * (2.0 + i)
      mod_arr_diff(i) = mod_arr_diff(i) * (2.0 + i)
      x_ad = x_ad + mod_arr_diff_ad(i) * mod_arr(i) ! x = x + mod_arr(i) * mod_arr_diff(i)
      x = x + mod_arr(i) * mod_arr_diff(i)
    end do

    return
  end subroutine module_vars_main_fwd_ad

  subroutine module_vars_main_rev_ad(n, x_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: x_ad
    integer :: i

    do i = n, 1, - 1
      mod_arr_diff_ad(i) = x_ad * mod_arr(i) + mod_arr_diff_ad(i) ! x = x + mod_arr(i) * mod_arr_diff(i)
      mod_arr_diff_ad(i) = mod_arr_diff_ad(i) * (2.0 + i) ! mod_arr_diff(i) = mod_arr_diff(i) * (2.0 + i)
    end do
    x_ad = 0.0 ! x = 0.0

    return
  end subroutine module_vars_main_rev_ad

  subroutine module_vars_finalize_fwd_ad(n, x, x_ad)
    integer, intent(in)  :: n
    real, intent(out) :: x
    real, intent(out) :: x_ad
    integer :: i

    x_ad = 0.0 ! x = 0.0
    x = 0.0
    do i = 1, n
      x_ad = x_ad + mod_arr_diff_ad(i) * mod_arr(i) ! x = x + mod_arr(i) * mod_arr_diff(i)
      x = x + mod_arr(i) * mod_arr_diff(i)
    end do
    deallocate(mod_arr_diff_ad)

    return
  end subroutine module_vars_finalize_fwd_ad

  subroutine module_vars_finalize_rev_ad(n, x_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: x_ad
    integer :: i

    if (.not. allocated(mod_arr_diff_ad)) then
      allocate(mod_arr_diff_ad, mold=mod_arr_diff)
    end if
    do i = n, 1, - 1
      mod_arr_diff_ad(i) = x_ad * mod_arr(i) ! x = x + mod_arr(i) * mod_arr_diff(i)
    end do
    x_ad = 0.0 ! x = 0.0

    return
  end subroutine module_vars_finalize_rev_ad

end module allocate_vars_ad
