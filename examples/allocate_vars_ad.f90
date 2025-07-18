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
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    real, allocatable :: arr_ad(:)
    integer :: i
    real, allocatable :: arr(:)

    allocate(arr(n))
    do i = 1, n
      arr(i) = i * x
    end do

    x_ad = 0.0

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

  subroutine module_vars_init_fwd_ad(n, x, x_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    integer :: i

    if (.not. allocated(mod_arr_diff_ad)) then
      allocate(mod_arr_diff_ad(n))
    end if
    do i = 1, n
      mod_arr_diff_ad(i) = x_ad * i ! mod_arr_diff(i) = i * x
    end do

    return
  end subroutine module_vars_init_fwd_ad

  subroutine module_vars_init_rev_ad(n, x, x_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    integer :: i

    x_ad = 0.0

    do i = n, 1, - 1
      x_ad = mod_arr_diff_ad(i) * i + x_ad ! mod_arr_diff(i) = i * x
      mod_arr_diff_ad(i) = 0.0 ! mod_arr_diff(i) = i * x
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
    if (allocated(mod_arr_diff_ad)) then
      deallocate(mod_arr_diff_ad)
    end if

    return
  end subroutine module_vars_finalize_fwd_ad

  subroutine module_vars_finalize_rev_ad(n, x_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: x_ad
    integer :: i

    if (.not. allocated(mod_arr_diff_ad)) then
      allocate(mod_arr_diff_ad(n))
    end if
    do i = n, 1, - 1
      mod_arr_diff_ad(i) = x_ad * mod_arr(i) ! x = x + mod_arr(i) * mod_arr_diff(i)
    end do
    x_ad = 0.0 ! x = 0.0

    return
  end subroutine module_vars_finalize_rev_ad

end module allocate_vars_ad
