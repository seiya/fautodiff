module derived_alloc_ad
  use derived_alloc
  implicit none
contains
  subroutine derived_alloc_example_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    type(derived_t) :: obj
    real, allocatable :: arr_ad(:)
    integer :: i

    allocate(obj%arr(n))
    allocate(arr_ad(n))
    do i = 1, n
      arr_ad(i) = x_ad * i ! obj%arr(i) = i * x
      obj%arr(i) = i * x
    end do
    res_ad = 0.0 ! res = 0.0
    res = 0.0
    do i = 1, n
      res_ad = res_ad + arr_ad(i) * x + x_ad * obj%arr(i) ! res = res + obj%arr(i) * x
      res = res + obj%arr(i) * x
    end do
    deallocate(arr_ad)
    deallocate(obj%arr)

    return
  end subroutine derived_alloc_example_fwd_ad

  subroutine derived_alloc_example_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    type(derived_t) :: obj
    real, allocatable :: arr_ad(:)
    integer :: i

    allocate(obj%arr(n))
    do i = 1, n
      obj%arr(i) = i * x
    end do

    x_ad = 0.0

    allocate(arr_ad(n))
    do i = n, 1, -1
      arr_ad(i) = res_ad * x ! res = res + obj%arr(i) * x
      x_ad = res_ad * obj%arr(i) + x_ad ! res = res + obj%arr(i) * x
    end do
    res_ad = 0.0 ! res = 0.0
    do i = n, 1, -1
      x_ad = arr_ad(i) * i + x_ad ! obj%arr(i) = i * x
    end do
    deallocate(arr_ad)
    deallocate(obj%arr)

    return
  end subroutine derived_alloc_example_rev_ad

end module derived_alloc_ad
