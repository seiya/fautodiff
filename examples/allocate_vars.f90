module allocate_vars
  implicit none

  real, allocatable :: mod_arr(:)
  real, allocatable :: mod_arr_diff(:)
  !$FAD CONSTANT_VARS: mod_arr
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

  subroutine save_alloc(n, x, y, z)
    integer, intent(in) :: n
    real, intent(in) :: x(n), y
    real, intent(out) :: z
    real, allocatable :: htmp(:)
    integer :: i

    allocate(htmp(n))
    htmp = x
    z = 0.0
    do i = 1, n
      z = z + htmp(i) * y
    end do
    htmp = x**2
    do i = 1, n
      z = z + htmp(i) * y
    end do
    deallocate(htmp)

    return
  end subroutine save_alloc

  subroutine module_vars_init(n, x)
    integer, intent(in) :: n
    real, intent(in) :: x
    integer :: i

    allocate(mod_arr(n))
    allocate(mod_arr_diff(n))

    do i = 1, n
      mod_arr(i) = i * 2.0
      mod_arr_diff(i) = i * x
    end do

    return
  end subroutine module_vars_init

  subroutine module_vars_main(n, x)
    integer, intent(in) :: n
    real, intent(out) :: x
    integer :: i

    x = 0.0
    do i = 1, n
      mod_arr_diff(i) = mod_arr_diff(i) * (2.0 + i)
      x = x + mod_arr(i) * mod_arr_diff(i)
    end do

    return
  end subroutine module_vars_main

  subroutine module_vars_finalize(n, x)
    integer, intent(in) :: n
    real, intent(out) :: x
    integer :: i

    x = 0.0
    do i = 1, n
      x = x + mod_arr(i) * mod_arr_diff(i)
    end do

    deallocate(mod_arr)
    deallocate(mod_arr_diff)

    return
  end subroutine module_vars_finalize

end module allocate_vars
