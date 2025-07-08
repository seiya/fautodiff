module allocate_example
  implicit none
  real, allocatable :: mod_arr(:)
  real, allocatable :: mod_arr_diff(:)
  !$FAD DIFF_MODULE_VARS: mod_arr_diff
contains

  subroutine allocate_and_sum(n, x, res)
    integer, intent(in) :: n
    real, intent(in) :: x
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
    deallocate(arr)

    return
  end subroutine allocate_and_sum

end module allocate_example
