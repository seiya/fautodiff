module pointer_arrays_ad
  use pointer_arrays
  use fautodiff_stack
  implicit none

  real, pointer :: mod_p_ad(:)
  real, allocatable, target :: all_p_ad(:,:)
  real, pointer :: sub1_p_ad(:)
  real, pointer :: sub2_p_ad(:)

contains

  subroutine pointer_allocate_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    real, pointer :: p_ad(:)
    integer :: i
    real, pointer :: p(:)

    allocate(p(n))
    allocate(p_ad(n))
    if (.not. associated(mod_p_ad)) then
      allocate(mod_p_ad(n))
    end if
    do i = 1, n
      p_ad(i) = x_ad ! p(i) = x
      p(i) = x
      mod_p_ad(i) = x_ad ! mod_p(i) = x
      mod_p(i) = x
    end do
    res_ad = 0.0 ! res = 0.0
    res = 0.0
    do i = 1, n
      res_ad = res_ad + p_ad(i) + mod_p_ad(i) ! res = res + p(i) + mod_p(i)
      res = res + p(i) + mod_p(i)
    end do
    deallocate(p_ad)
    deallocate(p)
    deallocate(mod_p_ad)

    return
  end subroutine pointer_allocate_fwd_ad

  subroutine pointer_allocate_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: res_ad
    real, pointer :: p_ad(:)
    integer :: i

    if (.not. associated(mod_p_ad)) then
      allocate(mod_p_ad, mold=mod_p)
    end if
    allocate(p_ad(n))
    do i = n, 1, - 1
      p_ad(i) = res_ad ! res = res + p(i) + mod_p(i)
      mod_p_ad(i) = res_ad ! res = res + p(i) + mod_p(i)
    end do
    res_ad = 0.0 ! res = 0.0
    do i = n, 1, - 1
      x_ad = mod_p_ad(i) + x_ad ! mod_p(i) = x
      x_ad = p_ad(i) + x_ad ! p(i) = x
    end do
    if (associated(mod_p_ad)) then
      deallocate(mod_p_ad)
    end if
    deallocate(p_ad)

    return
  end subroutine pointer_allocate_rev_ad

  subroutine pointer_subarray_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    real, pointer :: p_ad(:)
    integer :: i
    real, pointer :: p(:)

    if (.not. associated(mod_p_ad)) then
      allocate(mod_p_ad(n))
    end if
    do i = 1, n
      mod_p_ad(i) = x_ad ! mod_p(i) = x + i
      mod_p(i) = x + i
    end do
    p_ad => mod_p_ad(2:n) ! p => mod_p(2:n)
    p => mod_p(2:n)
    res_ad = 0.0 ! res = 0.0
    res = 0.0
    do i = 1, n - 1
      res_ad = res_ad + p_ad(i) ! res = res + p(i)
      res = res + p(i)
    end do
    p => null()
    deallocate(mod_p_ad)

    return
  end subroutine pointer_subarray_fwd_ad

  subroutine pointer_subarray_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: res_ad
    real, pointer :: p_ad(:)
    integer :: i

    if (.not. associated(mod_p_ad)) then
      allocate(mod_p_ad, mold=mod_p)
    end if
    mod_p_ad = 0.0
    p_ad => mod_p_ad(2:n)
    do i = n - 1, 1, - 1
      p_ad(i) = res_ad ! res = res + p(i)
    end do
    res_ad = 0.0 ! res = 0.0
    p_ad => null() ! p => mod_p(2:n)
    do i = n, 1, - 1
      x_ad = mod_p_ad(i) + x_ad ! mod_p(i) = x + i
    end do
    if (associated(mod_p_ad)) then
      deallocate(mod_p_ad)
    end if

    return
  end subroutine pointer_subarray_rev_ad

  subroutine pointer_allsub_init_fwd_ad(n)
    integer, intent(in)  :: n
    integer :: i

    if (.not. allocated(all_p_ad)) then
      allocate(all_p_ad(n,2))
    end if
    all_p_ad = 0.0
    sub1_p_ad => all_p_ad(:,1) ! sub1_p => all_p(:,1)
    sub1_p => all_p(:,1)
    sub2_p_ad => all_p_ad(:,2) ! sub2_p => all_p(:,2)
    sub2_p => all_p(:,2)
    do i = 1, n
      all_p_ad(i,1) = 0.0 ! all_p(i,1) = 0.0
      all_p(i,1) = 0.0
      all_p_ad(i,2) = 0.0 ! all_p(i,2) = i
      all_p(i,2) = i
    end do

    return
  end subroutine pointer_allsub_init_fwd_ad

  subroutine pointer_allsub_init_rev_ad(n)
    integer, intent(in)  :: n

    if (allocated(all_p_ad)) then
      deallocate(all_p_ad)
    end if

    return
  end subroutine pointer_allsub_init_rev_ad

  subroutine pointer_allsub_init_fwd_rev_ad()

    if (.not. allocated(all_p_ad)) then
      allocate(all_p_ad, mold=all_p)
      all_p_ad = 0.0
    end if
    sub1_p_ad => all_p_ad(:,1)
    sub2_p_ad => all_p_ad(:,2)

    return
  end subroutine pointer_allsub_init_fwd_rev_ad

  subroutine pointer_allsub_main_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(in)  :: x_ad(n)
    real, intent(out) :: res
    real, intent(out) :: res_ad
    integer :: i

    do i = 1, n
      sub1_p_ad(i) = sub1_p_ad(i) + x_ad(i) ! sub1_p(i) = sub1_p(i) + x(i)
      sub1_p(i) = sub1_p(i) + x(i)
      sub2_p(i) = sub2_p(i) + i
    end do
    res_ad = 0.0 ! res = 0.0
    res = 0.0
    do i = 1, n
      res_ad = res_ad + sub1_p_ad(i) * sub2_p(i) + sub2_p_ad(i) * sub1_p(i) ! res = res + sub1_p(i) * sub2_p(i)
      res = res + sub1_p(i) * sub2_p(i)
    end do

    return
  end subroutine pointer_allsub_main_fwd_ad

  subroutine pointer_allsub_main_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(inout) :: x_ad(n)
    real, intent(inout) :: res_ad
    integer :: i

    call fautodiff_stack_p%pop(sub2_p)
    call fautodiff_stack_p%pop(sub1_p)
    do i = 1, n
      sub1_p(i) = sub1_p(i) + x(i)
      sub2_p(i) = sub2_p(i) + i
    end do

    do i = n, 1, - 1
      sub1_p_ad(i) = res_ad * sub2_p(i) + sub1_p_ad(i) ! res = res + sub1_p(i) * sub2_p(i)
      sub2_p_ad(i) = res_ad * sub1_p(i) + sub2_p_ad(i) ! res = res + sub1_p(i) * sub2_p(i)
    end do
    res_ad = 0.0 ! res = 0.0
    do i = n, 1, - 1
      x_ad(i) = sub1_p_ad(i) + x_ad(i) ! sub1_p(i) = sub1_p(i) + x(i)
    end do

    return
  end subroutine pointer_allsub_main_rev_ad

  subroutine pointer_allsub_main_fwd_rev_ad()

    call fautodiff_stack_p%push(sub1_p)
    call fautodiff_stack_p%push(sub2_p)

    return
  end subroutine pointer_allsub_main_fwd_rev_ad

  subroutine pointer_swap_fwd_ad(n, x, x_ad, y, y_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in), target  :: x(n)
    real, intent(in), target  :: x_ad(n)
    real, intent(in), target  :: y(n)
    real, intent(in), target  :: y_ad(n)
    real, intent(out) :: res
    real, intent(out) :: res_ad
    real, pointer :: swap_ad(:)
    real, pointer :: work1_ad(:)
    real, pointer :: work2_ad(:)
    real, pointer :: work1(:)
    real, pointer :: work2(:)
    integer :: i
    real, pointer :: swap(:)
    integer :: j

    work1_ad => x_ad ! work1 => x
    work1 => x
    work2_ad => y_ad ! work2 => y
    work2 => y
    res_ad = 0.0 ! res = 0.0
    res = 0.0
    do j = 1, 3
      do i = 1, n
        res_ad = res_ad + work1_ad(i) ! res = res + work1(i)
        res = res + work1(i)
      end do
      swap_ad => work1_ad ! swap => work1
      swap => work1
      work1_ad => work2_ad ! work1 => work2
      work1 => work2
      work2_ad => swap_ad ! work2 => swap
      work2 => swap
    end do

    return
  end subroutine pointer_swap_fwd_ad

  subroutine pointer_swap_rev_ad(n, x, x_ad, y, y_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in), target  :: x(n)
    real, intent(inout), target :: x_ad(n)
    real, intent(in), target  :: y(n)
    real, intent(inout), target :: y_ad(n)
    real, intent(inout) :: res_ad
    real, pointer :: swap_ad(:)
    real, pointer :: work1_ad(:)
    real, pointer :: work2_ad(:)
    integer :: j
    integer :: i

    work1_ad => x_ad ! work1 => x
    work2_ad => y_ad ! work2 => y
    do j = 1, 3
      call fautodiff_stack_p%push(work1_ad)
      swap_ad => work1_ad ! swap => work1
      work1_ad => work2_ad ! work1 => work2
      work2_ad => swap_ad ! work2 => swap
    end do

    do j = 3, 1, - 1
      call fautodiff_stack_p%pop(work1_ad)
      do i = n, 1, - 1
        work1_ad(i) = res_ad + work1_ad(i) ! res = res + work1(i)
      end do
    end do
    res_ad = 0.0 ! res = 0.0
    work1_ad => null() ! work1 => x

    return
  end subroutine pointer_swap_rev_ad

end module pointer_arrays_ad
