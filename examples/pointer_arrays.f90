module pointer_arrays
  implicit none

  real, pointer :: mod_p(:)
  real, allocatable, target :: all_p(:,:)
  real, pointer :: sub1_p(:)
  real, pointer :: sub2_p(:)

contains

  subroutine pointer_allocate(n, x, res)
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
  end subroutine pointer_allocate

  subroutine pointer_subarray(n, x, res)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(out) :: res
    real, pointer :: p(:)
    integer :: i

    allocate(mod_p(n))
    do i = 1, n
      mod_p(i) = x + i
    end do
    p => mod_p(2:n)
    res = 0.0
    do i = 1, n - 1
      res = res + p(i)
    end do
    deallocate(mod_p)

    return
  end subroutine pointer_subarray

  subroutine pointer_allsub_init(n)
    integer, intent(in) :: n
    integer :: i

    allocate(all_p(n,2))
    sub1_p => all_p(:,1)
    sub2_p => all_p(:,2)

    do i = 1, n
      all_p(i,1) = 0.0
      all_p(i,2) = i
    end do

    return
  end subroutine pointer_allsub_init

  subroutine pointer_allsub_main(n, x, res)
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    real, intent(out) :: res
    integer :: i

    do i = 1, n
      sub1_p(i) = sub1_p(i) + x(i)
      sub2_p(i) = sub2_p(i) + i
    end do
    res = 0.0
    do i = 1, n
      res = res + sub1_p(i) * sub2_p(i)
    end do

    return
  end subroutine pointer_allsub_main

  subroutine pointer_swap(n, x, y, res)
    integer, intent(in) :: n
    real, intent(in), target :: x(n), y(n)
    real, intent(out) :: res
    real, pointer :: work1(:)
    real, pointer :: work2(:)
    real, pointer :: swap(:)
    integer :: i, j

    work1 => x
    work2 => y
    res = 0.0
    do j = 1, 4
      do i = 1, n
        res = res + work1(i)
      end do
      swap => work1
      work1 => work2
      work2 => swap
    end do

    return
  end subroutine pointer_swap

end module pointer_arrays
