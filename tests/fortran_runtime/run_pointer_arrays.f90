program run_pointer_arrays
  use pointer_arrays
  use pointer_arrays_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_pointer_allocate = 1
  integer, parameter :: I_pointer_subarray = 2
  integer, parameter :: I_pointer_allsub = 3
  integer, parameter :: I_pointer_swap = 4

  integer :: length, status
  character(:), allocatable :: arg
  integer :: i_test

  i_test = I_all
  if (command_argument_count() > 0) then
     call get_command_argument(1, length=length, status=status)
     if (status == 0) then
        allocate(character(len=length) :: arg)
        call get_command_argument(1, arg, status=status)
        if (status == 0) then
           select case(arg)
           case ("pointer_allocate")
              i_test = I_pointer_allocate
           case ("pointer_subarray")
              i_test = I_pointer_subarray
           case ("pointer_allsub")
              i_test = I_pointer_allsub
           case ("pointer_swap")
              i_test = I_pointer_swap
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_pointer_allocate .or. i_test == I_all) then
     call test_pointer_allocate
  end if
  if (i_test == I_pointer_subarray .or. i_test == I_all) then
     call test_pointer_subarray
  end if
  if (i_test == I_pointer_allsub .or. i_test == I_all) then
     call test_pointer_allsub
  end if
  if (i_test == I_pointer_swap .or. i_test == I_all) then
     call test_pointer_swap
  end if

  stop
contains

  subroutine test_pointer_allocate
    real, parameter :: tol = 4.0e-4
    integer, parameter :: n = 5
    real :: x, res, res_eps
    real :: x_ad, res_ad
    real :: fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 2.0
    call pointer_allocate(n, x, res)
    call pointer_allocate(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    allocate(mod_p(n))
    call pointer_allocate_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_pointer_allocate_fwd failed', res_ad, fd
       error stop 1
    end if

    inner1 = res_ad**2
    call pointer_allocate_rev_ad(n, x, x_ad, res_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_pointer_allocate_rev failed', inner1, inner2
       error stop 1
    end if

    deallocate(mod_p)

    return
  end subroutine test_pointer_allocate

  subroutine test_pointer_subarray
    real, parameter :: tol = 1.0e-4
    integer, parameter :: n = 5
    real :: x, res, res_eps
    real :: x_ad, res_ad
    real :: fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = 2.0
    call pointer_subarray(n, x, res)
    call pointer_subarray(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    allocate(mod_p(n))
    call pointer_subarray_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_pointer_subarray_fwd failed', res_ad, fd
       error stop 1
    end if

    inner1 = res_ad**2
    call pointer_subarray_rev_ad(n, x, x_ad, res_ad)
    inner2 = x_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_pointer_subarray_rev failed', inner1, inner2
       error stop 1
    end if

    deallocate(mod_p)

    return
  end subroutine test_pointer_subarray

  subroutine test_pointer_allsub
    real, parameter :: tol = 3.0e-4
    integer, parameter :: n = 5
    real :: x(n), res, res_eps
    real :: x_ad(n), res_ad
    real :: fd, eps
    real :: inner1, inner2
    integer :: i

    allocate(mod_p(n))

    eps = 1.0e-3
    do i = 1, n
      x(i) = i
    end do
    call pointer_allsub_init(n)
    call pointer_allsub_main(n, x, res)
    deallocate(all_p)
    call pointer_allsub_init(n)
    call pointer_allsub_main(n, x + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad(:) = 1.0
    call pointer_allsub_init_fwd_ad(n)
    call pointer_allsub_main_fwd_ad(n, x, x_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_pointer_allsub_fwd failed', res_ad, fd
       error stop 1
    end if

    inner1 = res_ad**2 + sum(sub1_p_ad(:)**2) + sum(sub2_p_ad(:)**2)
    deallocate(all_p)
    call pointer_allsub_init(n)
    call pointer_allsub_main_fwd_rev_ad()
    call pointer_allsub_main_rev_ad(n, x, x_ad, res_ad)
    inner2 = sum(x_ad)
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_pointer_allsub_rev failed', inner1, inner2
       error stop 1
    end if

    call pointer_allsub_init_rev_ad(n)
    deallocate(all_p)

    return
  end subroutine test_pointer_allsub

  subroutine test_pointer_swap
    real, parameter :: tol = 3.0e-4
    integer, parameter :: n = 5
    real :: x(n), y(n), res, res_eps
    real :: x_ad(n), y_ad(n), res_ad
    real :: fd, eps
    real :: inner1, inner2
    integer :: i

    eps = 1.0e-3
    do i = 1, n
      x(i) = i
      y(i) = i * 2.0
    end do
    call pointer_swap(n, x, y, res)
    call pointer_swap(n, x + eps, y + eps, res_eps)
    fd = (res_eps - res) / eps
    x_ad = 1.0
    y_ad = 1.0
    call pointer_swap_fwd_ad(n, x, x_ad, y, y_ad, res, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_pointer_swap_fwd failed', res_ad, fd
       error stop 1
    end if

    inner1 = res_ad**2
    call pointer_swap_rev_ad(n, x, x_ad, y, y_ad, res_ad)
    inner2 = sum(x_ad) + sum(y_ad)
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_pointer_swap_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_pointer_swap

end program run_pointer_arrays
