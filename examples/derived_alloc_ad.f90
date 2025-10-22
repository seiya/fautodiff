module derived_alloc_ad
  use fautodiff_stack
  implicit none

  type :: derived_t
    real, allocatable :: arr(:)
  end type derived_t
  type(derived_t), public, allocatable :: obj(:)
  type :: derived_ad_t
    real, allocatable :: arr_ad(:)
  end type derived_ad_t
  type(derived_ad_t), allocatable :: obj_ad(:)

contains

  subroutine derived_alloc_init(n, m)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer :: j

    allocate(obj(m))
    do j = 1, m
      allocate(obj(j)%arr(n))
      obj(j)%arr(:) = 1.0
    end do

    return
  end subroutine derived_alloc_init

  subroutine derived_alloc_finalize(m)
    integer, intent(in)  :: m
    integer :: j

    do j = 1, m
      if (allocated(obj) .and. allocated(obj(j)%arr)) then
        deallocate(obj(j)%arr)
      end if
    end do
    if (allocated(obj)) then
      deallocate(obj)
    end if

    return
  end subroutine derived_alloc_finalize

  subroutine derived_alloc_run(n, m, x, res)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x
    real, intent(out) :: res
    integer :: i
    integer :: j

    do j = 1, m
      do i = 1, n
        obj(j)%arr(i) = obj(j)%arr(i) * x + j
      end do
    end do
    res = 0.0
    do j = 1, m
      do i = 1, n
        res = res + obj(j)%arr(i) * x
      end do
    end do

    return
  end subroutine derived_alloc_run

  subroutine derived_alloc_init_fwd_ad(n, m)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer :: j

    if (.not. allocated(obj_ad)) then
      allocate(obj_ad(m))
    end if
    do j = 1, m
      if (.not. allocated(obj_ad(j)%arr_ad)) then
        allocate(obj_ad(j)%arr_ad(n))
      end if
      obj_ad(j)%arr_ad(:) = 0.0 ! obj(j)%arr(:) = 1.0
      obj(j)%arr(:) = 1.0
    end do

    return
  end subroutine derived_alloc_init_fwd_ad

  subroutine derived_alloc_init_rev_ad(m)
    integer, intent(in)  :: m
    integer :: j

    do j = m, 1, - 1
      if (allocated(obj_ad) .and. allocated(obj_ad(j)%arr_ad)) then
        deallocate(obj_ad(j)%arr_ad)
      end if
    end do
    if (allocated(obj_ad)) then
      deallocate(obj_ad)
    end if

    return
  end subroutine derived_alloc_init_rev_ad

  subroutine derived_alloc_finalize_fwd_ad(m)
    integer, intent(in)  :: m
    integer :: j

    do j = 1, m
      if (allocated(obj_ad) .and. allocated(obj_ad(j)%arr_ad)) then
        deallocate(obj_ad(j)%arr_ad)
      end if
    end do
    if (allocated(obj_ad)) then
      deallocate(obj_ad)
    end if

    return
  end subroutine derived_alloc_finalize_fwd_ad

  subroutine derived_alloc_finalize_rev_ad(m)
    integer, intent(in)  :: m
    integer :: j

    if (.not. allocated(obj_ad)) then
      allocate(obj_ad(size(obj, 1)))
    end if
    do j = m, 1, - 1
      if (.not. allocated(obj_ad(j)%arr_ad)) then
        allocate(obj_ad(j)%arr_ad, mold=obj(j)%arr)
      end if
      obj_ad(j)%arr_ad = 0.0
    end do

    return
  end subroutine derived_alloc_finalize_rev_ad

  subroutine derived_alloc_run_fwd_ad(n, m, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    integer :: i
    integer :: j

    do j = 1, m
      do i = 1, n
        obj_ad(j)%arr_ad(i) = obj_ad(j)%arr_ad(i) * x + x_ad * obj(j)%arr(i) ! obj(j)%arr(i) = obj(j)%arr(i) * x + j
        obj(j)%arr(i) = obj(j)%arr(i) * x + j
      end do
    end do
    res_ad = 0.0 ! res = 0.0
    res = 0.0
    do j = 1, m
      do i = 1, n
        res_ad = res_ad + obj_ad(j)%arr_ad(i) * x + x_ad * obj(j)%arr(i) ! res = res + obj(j)%arr(i) * x
        res = res + obj(j)%arr(i) * x
      end do
    end do

    return
  end subroutine derived_alloc_run_fwd_ad

  subroutine derived_alloc_run_rev_ad(n, m, x, x_ad, res_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: res_ad
    integer :: n0_ad
    integer :: i
    integer :: j
    real :: obj_arr_save_42_ad(m,n)

    do n0_ad = ubound(obj, 1), lbound(obj, 1), - 1
      call fautodiff_stack_pop_r(obj(n0_ad)%arr(:))
    end do
    do j = 1, m
      obj_arr_save_42_ad(j,1:n) = obj(j)%arr(1:n)
      do i = 1, n
        obj(j)%arr(i) = obj(j)%arr(i) * x + j
      end do
    end do

    do j = m, 1, - 1
      do i = n, 1, - 1
        obj_ad(j)%arr_ad(i) = res_ad * x + obj_ad(j)%arr_ad(i) ! res = res + obj(j)%arr(i) * x
        x_ad = res_ad * obj(j)%arr(i) + x_ad ! res = res + obj(j)%arr(i) * x
      end do
    end do
    res_ad = 0.0 ! res = 0.0
    do j = m, 1, - 1
      obj(j)%arr(1:n) = obj_arr_save_42_ad(j,1:n)
      do i = n, 1, - 1
        x_ad = obj_ad(j)%arr_ad(i) * obj(j)%arr(i) + x_ad ! obj(j)%arr(i) = obj(j)%arr(i) * x + j
        obj_ad(j)%arr_ad(i) = obj_ad(j)%arr_ad(i) * x ! obj(j)%arr(i) = obj(j)%arr(i) * x + j
      end do
    end do

    return
  end subroutine derived_alloc_run_rev_ad

  subroutine derived_alloc_run_fwd_rev_ad()
    integer :: n0_ad

    do n0_ad = lbound(obj, 1), ubound(obj, 1)
      call fautodiff_stack_push_r(obj(n0_ad)%arr(:))
    end do

    return
  end subroutine derived_alloc_run_fwd_rev_ad

end module derived_alloc_ad
