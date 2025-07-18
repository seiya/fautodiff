module pointer_arrays_ad
  use pointer_arrays
  use fautodiff_data_storage
  implicit none

  real, pointer :: mod_p_ad(:)

contains

  subroutine pointer_example_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    real, pointer :: p_ad(:)
    integer :: i
    real, pointer :: p(:)

    p_ad(:) = 0.0

    allocate(p(n))
    if (.not. associated(p_ad)) then
      allocate(p_ad(n))
    end if
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
    if (associated(p_ad)) then
      deallocate(p_ad)
    end if
    if (associated(p)) then
      deallocate(p)
    end if
    if (associated(mod_p_ad)) then
      deallocate(mod_p_ad)
    end if

    return
  end subroutine pointer_example_fwd_ad

  subroutine pointer_example_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    real, pointer :: p_ad(:)
    integer :: i

    allocate(p(n))

    p_ad(:) = 0.0
    x_ad = 0.0

    if (.not. associated(mod_p_ad)) then
      allocate(mod_p_ad(n))
    end if
    if (.not. associated(p_ad)) then
      allocate(p_ad(n))
    end if
    do i = n, 1, - 1
      p_ad(i) = res_ad ! res = res + p(i) + mod_p(i)
      mod_p_ad(i) = res_ad ! res = res + p(i) + mod_p(i)
    end do
    res_ad = 0.0 ! res = 0.0
    do i = n, 1, - 1
      x_ad = mod_p_ad(i) + x_ad ! mod_p(i) = x
      mod_p_ad(i) = 0.0 ! mod_p(i) = x
      x_ad = p_ad(i) + x_ad ! p(i) = x
      p_ad(i) = 0.0 ! p(i) = x
    end do
    if (associated(mod_p_ad)) then
      deallocate(mod_p_ad)
    end if
    if (associated(p_ad)) then
      deallocate(p_ad)
    end if
    deallocate(p)

    return
  end subroutine pointer_example_rev_ad

  subroutine pointer_example_fwd_rev_ad(n, x, res)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: res

    call fautodiff_data_storage_push(mod_p)
    call pointer_example(n, x, res)

    return
  end subroutine pointer_example_fwd_rev_ad

end module pointer_arrays_ad
