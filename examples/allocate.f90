module allocate_example
  implicit none
contains

  subroutine allocate_and_sum(n, x, res)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: res
    real, allocatable :: arr(:)

    allocate(arr(n))
    res = x * 2.0
    deallocate(arr)

    return
  end subroutine allocate_and_sum

end module allocate_example
