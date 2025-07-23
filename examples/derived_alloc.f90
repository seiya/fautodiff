module derived_alloc
  implicit none
  type :: derived_t
    real, allocatable :: arr(:)
  end type derived_t
contains
  subroutine derived_alloc_example(n, x, res)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: res
    type(derived_t) :: obj
    integer :: i

    allocate(obj%arr(n))
    do i = 1, n
      obj%arr(i) = i * x
    end do
    res = 0.0
    do i = 1, n
      res = res + obj%arr(i) * x
    end do
    deallocate(obj%arr)

    return
  end subroutine derived_alloc_example

end module derived_alloc
