module derived_alloc_ad
  use derived_alloc
  use fautodiff_data_storage
  implicit none

  type :: derived_ad_t
    real, allocatable :: arr_ad(:)
  end type derived_ad_t

  type(derived_ad_t), allocatable :: obj_ad(:)

contains

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
        obj_ad(j)%arr_ad(:) = 0.0
      end if
      obj(j)%arr(:) = 1.0
    end do


    return
  end subroutine derived_alloc_init_fwd_ad

  subroutine derived_alloc_init_rev_ad(n, m)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer :: j

    if (allocated(obj_ad)) then
      do j = 1, m
        if (allocated(obj_ad(j)%arr_ad)) then
          deallocate(obj_ad(j)%arr_ad)
        end if
      end do
    end if
    if (allocated(obj_ad)) then
      deallocate(obj_ad)
    end if

    return
  end subroutine derived_alloc_init_rev_ad

  subroutine derived_alloc_init_fwd_rev_ad(n, m)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer :: j

    if (.not. allocated(obj_ad)) then
      allocate(obj_ad(m))
    end if
    do j = 1, m
      if (.not. allocated(obj_ad(j)%arr_ad)) then
        allocate(obj_ad(j)%arr_ad(n))
        obj_ad(j)%arr_ad(:) = 0.0
      end if
    end do

    return
  end subroutine derived_alloc_init_fwd_rev_ad

  subroutine derived_alloc_finalize_fwd_ad(m)
    integer, intent(in)  :: m
    integer :: j

    do j = 1, m
      deallocate(obj_ad(j)%arr_ad)
    end do
    deallocate(obj_ad)

    return
  end subroutine derived_alloc_finalize_fwd_ad

  subroutine derived_alloc_finalize_rev_ad(m)
    integer, intent(in)  :: m

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
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    integer :: i
    integer :: j
    real :: save_obj_arr_ad(m,n)

    do j = 1, m
      call fautodiff_data_storage_pop(obj(j)%arr)
    end do
    do j = 1, m
      do i = 1, n
        save_obj_arr_ad(j,i) = obj(j)%arr(i)
        obj(j)%arr(i) = obj(j)%arr(i) * x + j
      end do
    end do

    x_ad = 0.0

    do j = m, 1, -1
      do i = n, 1, -1
        obj_ad(j)%arr_ad(i) = res_ad * x + obj_ad(j)%arr_ad(i) ! res = res + obj(j)%arr(i) * x
        x_ad = res_ad * obj(j)%arr(i) + x_ad ! res = res + obj(j)%arr(i) * x
      end do
    end do
    res_ad = 0.0 ! res = 0.0
    do j = m, 1, -1
      do i = n, 1, -1
        obj(j)%arr(i) = save_obj_arr_ad(j,i)
        x_ad = obj_ad(j)%arr_ad(i) * obj(j)%arr(i) + x_ad ! obj(j)%arr(i) = obj(j)%arr(i) * x + j
        obj_ad(j)%arr_ad(i) = obj_ad(j)%arr_ad(i) * x + obj_ad(j)%arr_ad(i) ! obj(j)%arr(i) = obj(j)%arr(i) * x + j
      end do
    end do

    return
  end subroutine derived_alloc_run_rev_ad

  subroutine derived_alloc_run_fwd_rev_ad()
    integer :: n_ad

    do n_ad = 1, size(obj)
      call fautodiff_data_storage_push(obj(n_ad)%arr)
    end do

    return
  end subroutine derived_alloc_run_fwd_rev_ad

end module derived_alloc_ad
