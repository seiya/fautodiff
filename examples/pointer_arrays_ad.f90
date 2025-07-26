module pointer_arrays_ad
  use pointer_arrays
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
  end subroutine pointer_example_fwd_ad

  subroutine pointer_example_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    real, pointer :: p_ad(:)
    integer :: i

    x_ad = 0.0

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
  end subroutine pointer_example_rev_ad

end module pointer_arrays_ad
