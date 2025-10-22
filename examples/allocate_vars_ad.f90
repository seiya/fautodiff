module allocate_vars_ad
  implicit none

  real, public, allocatable :: mod_arr(:)
  real, public, allocatable :: mod_arr_diff(:)
  real, allocatable :: mod_arr_diff_ad(:)

contains

  subroutine allocate_and_sum(n, x, res)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: res
    real, allocatable :: arr(:)
    integer :: i

    allocate(arr(n))
    do i = 1, n
      arr(i) = i * x
    end do
    res = 0.0
    do i = 1, n
      res = res + arr(i) * x
    end do
    if (allocated(arr)) then
      deallocate(arr)
    end if

    return
  end subroutine allocate_and_sum

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
    if (allocated(arr_ad)) then
      deallocate(arr_ad)
    end if
    if (allocated(arr)) then
      deallocate(arr)
    end if

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

    allocate(arr_ad(n))
    allocate(arr(n))
    do i = 1, n
      arr(i) = i * x
    end do

    do i = n, 1, - 1
      arr_ad(i) = res_ad * x ! res = res + arr(i) * x
      x_ad = res_ad * arr(i) + x_ad ! res = res + arr(i) * x
    end do
    res_ad = 0.0 ! res = 0.0
    do i = n, 1, - 1
      x_ad = arr_ad(i) * i + x_ad ! arr(i) = i * x
    end do
    if (allocated(arr_ad)) then
      deallocate(arr_ad)
    end if
    if (allocated(arr)) then
      deallocate(arr)
    end if

    return
  end subroutine allocate_and_sum_rev_ad

  subroutine allocate_in_if(n, x, res)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: res
    real, allocatable :: arr(:)
    real, allocatable :: arr2(:)
    integer :: i

    allocate(arr(n))
    do i = 1, n
      arr(i) = i * x
    end do
    if (n > 0) then
      res = 0.0
      allocate(arr2(n))
      arr2(:) = arr(:)
      do i = 1, n
        res = res + arr2(i) * x
      end do
      if (allocated(arr2)) then
        deallocate(arr2)
      end if
    else
      res = 0.0
    end if
    if (allocated(arr)) then
      deallocate(arr)
    end if

    return
  end subroutine allocate_in_if

  subroutine allocate_in_if_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    real, allocatable :: arr2_ad(:)
    real, allocatable :: arr_ad(:)
    integer :: i
    real, allocatable :: arr(:)
    real, allocatable :: arr2(:)

    allocate(arr(n))
    allocate(arr_ad(n))
    do i = 1, n
      arr_ad(i) = x_ad * i ! arr(i) = i * x
      arr(i) = i * x
    end do
    if (n > 0) then
      res_ad = 0.0 ! res = 0.0
      res = 0.0
      allocate(arr2(n))
      allocate(arr2_ad(n))
      arr2_ad(:) = arr_ad(:) ! arr2(:) = arr(:)
      arr2(:) = arr(:)
      do i = 1, n
        res_ad = res_ad + arr2_ad(i) * x + x_ad * arr2(i) ! res = res + arr2(i) * x
        res = res + arr2(i) * x
      end do
      if (allocated(arr2_ad)) then
        deallocate(arr2_ad)
      end if
      if (allocated(arr2)) then
        deallocate(arr2)
      end if
    else
      res_ad = 0.0 ! res = 0.0
      res = 0.0
    end if
    if (allocated(arr_ad)) then
      deallocate(arr_ad)
    end if
    if (allocated(arr)) then
      deallocate(arr)
    end if

    return
  end subroutine allocate_in_if_fwd_ad

  subroutine allocate_in_if_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: res_ad
    real, allocatable :: arr2_ad(:)
    real, allocatable :: arr_ad(:)
    integer :: i
    real, allocatable :: arr(:)
    real, allocatable :: arr2(:)

    allocate(arr_ad(n))
    arr_ad = 0.0
    allocate(arr(n))
    do i = 1, n
      arr(i) = i * x
    end do

    if (n > 0) then
      allocate(arr2(n))
      arr2(:) = arr(:)
      allocate(arr2_ad(n))
      do i = n, 1, - 1
        arr2_ad(i) = res_ad * x ! res = res + arr2(i) * x
        x_ad = res_ad * arr2(i) + x_ad ! res = res + arr2(i) * x
      end do
      arr_ad(:) = arr2_ad(:) ! arr2(:) = arr(:)
      if (allocated(arr2_ad)) then
        deallocate(arr2_ad)
      end if
      if (allocated(arr2)) then
        deallocate(arr2)
      end if
      res_ad = 0.0 ! res = 0.0
    else
      res_ad = 0.0 ! res = 0.0
    end if
    do i = n, 1, - 1
      x_ad = arr_ad(i) * i + x_ad ! arr(i) = i * x
    end do
    if (allocated(arr_ad)) then
      deallocate(arr_ad)
    end if
    if (allocated(arr)) then
      deallocate(arr)
    end if

    return
  end subroutine allocate_in_if_rev_ad

  subroutine allocate_in_loop(n, x, res)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: res
    real, allocatable :: arr(:)
    integer :: i
    integer :: j

    res = 0.0
    do i = 1, n
      allocate(arr(i))
      do j = 1, i
        arr(j) = j * x
      end do
      do j = 1, i
        res = res + arr(j) * x
      end do
      if (allocated(arr)) then
        deallocate(arr)
      end if
    end do

    return
  end subroutine allocate_in_loop

  subroutine allocate_in_loop_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    real, allocatable :: arr_ad(:)
    integer :: i
    integer :: j
    real, allocatable :: arr(:)

    res_ad = 0.0 ! res = 0.0
    res = 0.0
    do i = 1, n
      allocate(arr(i))
      allocate(arr_ad(i))
      do j = 1, i
        arr_ad(j) = x_ad * j ! arr(j) = j * x
        arr(j) = j * x
      end do
      do j = 1, i
        res_ad = res_ad + arr_ad(j) * x + x_ad * arr(j) ! res = res + arr(j) * x
        res = res + arr(j) * x
      end do
      if (allocated(arr_ad)) then
        deallocate(arr_ad)
      end if
      if (allocated(arr)) then
        deallocate(arr)
      end if
    end do

    return
  end subroutine allocate_in_loop_fwd_ad

  subroutine allocate_in_loop_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: res_ad
    real, allocatable :: arr_ad(:)
    integer :: i
    integer :: j
    real, allocatable :: arr(:)

    do i = n, 1, - 1
      allocate(arr(i))
      do j = 1, i
        arr(j) = j * x
      end do
      allocate(arr_ad(i))
      do j = i, 1, - 1
        arr_ad(j) = res_ad * x ! res = res + arr(j) * x
        x_ad = res_ad * arr(j) + x_ad ! res = res + arr(j) * x
      end do
      do j = i, 1, - 1
        x_ad = arr_ad(j) * j + x_ad ! arr(j) = j * x
      end do
      if (allocated(arr_ad)) then
        deallocate(arr_ad)
      end if
      if (allocated(arr)) then
        deallocate(arr)
      end if
    end do
    res_ad = 0.0 ! res = 0.0

    return
  end subroutine allocate_in_loop_rev_ad

  subroutine save_alloc(n, x, y, z)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(in)  :: y
    real, intent(out) :: z
    real, allocatable :: htmp(:)
    integer :: i

    allocate(htmp(n))
    htmp = x
    z = 0.0
    do i = 1, n
      z = z + htmp(i) * y
    end do
    htmp = x**2
    do i = 1, n
      z = z + htmp(i) * y
    end do
    if (allocated(htmp)) then
      deallocate(htmp)
    end if

    return
  end subroutine save_alloc

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
    if (allocated(htmp_ad)) then
      deallocate(htmp_ad)
    end if
    if (allocated(htmp)) then
      deallocate(htmp)
    end if

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
    real, allocatable :: htmp_save_92_ad(:)

    allocate(htmp_ad(n))
    allocate(htmp(n))
    htmp = x
    allocate(htmp_save_92_ad, mold=htmp)
    htmp_save_92_ad(:) = htmp(:)
    htmp = x**2

    do i = n, 1, - 1
      htmp_ad(i) = z_ad * y ! z = z + htmp(i) * y
      y_ad = z_ad * htmp(i) + y_ad ! z = z + htmp(i) * y
    end do
    htmp(:) = htmp_save_92_ad(:)
    if (allocated(htmp_save_92_ad)) then
      deallocate(htmp_save_92_ad)
    end if
    x_ad = htmp_ad * 2.0 * x + x_ad ! htmp = x**2
    do i = n, 1, - 1
      htmp_ad(i) = z_ad * y ! z = z + htmp(i) * y
      y_ad = z_ad * htmp(i) + y_ad ! z = z + htmp(i) * y
    end do
    z_ad = 0.0 ! z = 0.0
    x_ad = htmp_ad + x_ad ! htmp = x
    if (allocated(htmp_ad)) then
      deallocate(htmp_ad)
    end if
    if (allocated(htmp)) then
      deallocate(htmp)
    end if

    return
  end subroutine save_alloc_rev_ad

  subroutine module_vars_init(n, x)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    integer :: i

    allocate(mod_arr(n))
    allocate(mod_arr_diff(n))
    do i = 1, n
      mod_arr(i) = i * 2.0
      mod_arr_diff(i) = i * x
    end do

    return
  end subroutine module_vars_init

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

  subroutine module_vars_main(n, x)
    integer, intent(in)  :: n
    real, intent(out) :: x
    integer :: i

    x = 0.0
    do i = 1, n
      mod_arr_diff(i) = mod_arr_diff(i) * (2.0 + i)
      x = x + mod_arr(i) * mod_arr_diff(i)
    end do

    return
  end subroutine module_vars_main

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

  subroutine module_vars_finalize(n, x)
    integer, intent(in)  :: n
    real, intent(out) :: x
    integer :: i

    x = 0.0
    do i = 1, n
      x = x + mod_arr(i) * mod_arr_diff(i)
    end do
    if (allocated(mod_arr)) then
      deallocate(mod_arr)
    end if
    if (allocated(mod_arr_diff)) then
      deallocate(mod_arr_diff)
    end if

    return
  end subroutine module_vars_finalize

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
      allocate(mod_arr_diff_ad, mold=mod_arr_diff)
    end if
    mod_arr_diff_ad = 0.0
    do i = n, 1, - 1
      mod_arr_diff_ad(i) = x_ad * mod_arr(i) ! x = x + mod_arr(i) * mod_arr_diff(i)
    end do
    x_ad = 0.0 ! x = 0.0

    return
  end subroutine module_vars_finalize_rev_ad

  subroutine allocate_with_early_return(n, x, res)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: res
    real, allocatable :: arr(:)
    integer :: i

    allocate(arr(n))
    if (n <= 0) then
      res = 0.0
      return
    end if
    do i = 1, n
      arr(i) = i * x
    end do
    res = 0.0
    do i = 1, n
      res = res + arr(i) * x
    end do
    if (allocated(arr)) then
      deallocate(arr)
    end if

    return
  end subroutine allocate_with_early_return

  subroutine allocate_with_early_return_fwd_ad(n, x, x_ad, res, res_ad)
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
    if (n <= 0) then
      res_ad = 0.0 ! res = 0.0
      res = 0.0
      if (allocated(arr_ad)) then
        deallocate(arr_ad)
      end if
      if (allocated(arr)) then
        deallocate(arr)
      end if
      return
    end if
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
    if (allocated(arr_ad)) then
      deallocate(arr_ad)
    end if
    if (allocated(arr)) then
      deallocate(arr)
    end if

    return
  end subroutine allocate_with_early_return_fwd_ad

  subroutine allocate_with_early_return_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: res_ad
    real, allocatable :: arr_ad(:)
    logical :: return_flag_155_ad
    integer :: i
    real, allocatable :: arr(:)

    return_flag_155_ad = .true.
    allocate(arr_ad(n))
    allocate(arr(n))
    if (n <= 0) then
      return_flag_155_ad = .false.
    end if
    if (return_flag_155_ad) then
      do i = 1, n
        arr(i) = i * x
      end do
    end if

    if (return_flag_155_ad) then
      do i = n, 1, - 1
        arr_ad(i) = res_ad * x ! res = res + arr(i) * x
        x_ad = res_ad * arr(i) + x_ad ! res = res + arr(i) * x
      end do
      res_ad = 0.0 ! res = 0.0
      do i = n, 1, - 1
        x_ad = arr_ad(i) * i + x_ad ! arr(i) = i * x
      end do
    end if
    if (n <= 0) then
      return_flag_155_ad = .true. ! return
      res_ad = 0.0 ! res = 0.0
    end if
    if (return_flag_155_ad) then
      if (allocated(arr_ad)) then
        deallocate(arr_ad)
      end if
      if (allocated(arr)) then
        deallocate(arr)
      end if
    end if

    return
  end subroutine allocate_with_early_return_rev_ad

end module allocate_vars_ad
