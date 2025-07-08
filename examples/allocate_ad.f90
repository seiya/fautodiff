module allocate_example_ad
  use allocate_example
  implicit none

contains

  subroutine allocate_and_sum_fwd_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
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
    do i = 1, n
      res_ad = res_ad + arr_ad(i) * x + x_ad * arr(i) ! res = res + arr(i) * x
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

end module allocate_example_ad
