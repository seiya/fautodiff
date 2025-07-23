module derived_alloc
  implicit none

  type :: derived_t
    real, allocatable :: arr(:)
  end type derived_t

  type(derived_t), allocatable :: obj(:)

contains

  subroutine derived_alloc_init(n, m)
    integer, intent(in) :: n, m
    integer :: j

    allocate(obj(m))
    do j = 1, m
      allocate(obj(j)%arr(n))
      obj(j)%arr(:) = 1.0
    end do

    return
  end subroutine derived_alloc_init

  subroutine derived_alloc_finalize(m)
    integer, intent(in) :: m
    integer :: j

    do j = 1, m
      deallocate(obj(j)%arr)
    end do
    deallocate(obj)

    return
  end subroutine derived_alloc_finalize

  subroutine derived_alloc_run(n, m, x, res)
    integer, intent(in) :: n, m
    real, intent(in) :: x
    real, intent(out) :: res
    integer :: i, j

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

end module derived_alloc
