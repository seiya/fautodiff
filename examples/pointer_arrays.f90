module pointer_arrays
  implicit none
  real, pointer :: mod_p(:)
contains
  subroutine pointer_example(n, x, res)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: res
    real, pointer :: p(:)
    integer :: i

    allocate(p(n))
    allocate(mod_p(n))
    do i = 1, n
      p(i) = x
      mod_p(i) = x
    end do
    res = 0.0
    do i = 1, n
      res = res + p(i) + mod_p(i)
    end do
    deallocate(p)
    deallocate(mod_p)

    return
  end subroutine pointer_example
end module pointer_arrays
