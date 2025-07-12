program run_arrays
  use arrays
  use arrays_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_elementwise_add = 1
  integer, parameter :: I_scale_array = 2
  integer, parameter :: I_multidimension = 3
  integer, parameter :: I_dot_product = 4
  integer, parameter :: I_indirect = 5
  integer, parameter :: I_stencil = 6

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
           case ("elementwise_add")
              i_test = I_elementwise_add
           case ("scale_array")
              i_test = I_scale_array
           case ("multidimension")
              i_test = I_multidimension
           case ("dot_product")
              i_test = I_dot_product
           case ("indirect")
              i_test = I_indirect
           case ("stencil")
              i_test = I_stencil
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_elementwise_add .or. i_test == I_all) then
     call test_elementwise_add
  end if
  if (i_test == I_scale_array .or. i_test == I_all) then
     call test_scale_array
  end if
  if (i_test == I_multidimension .or. i_test == I_all) then
     call test_multidimension
  end if
  if (i_test == I_dot_product .or. i_test == I_all) then
     call test_dot_product
  end if
  if (i_test == I_indirect .or. i_test == I_all) then
     call test_indirect
  end if
  if (i_test == I_stencil .or. i_test == I_all) then
     call test_stencil
  end if

  stop
contains

  subroutine test_elementwise_add
    integer, parameter :: n = 3
    real :: a(n), b(n), c(n)
    real :: a_ad(n), b_ad(n), c_ad(n)
    real :: c_eps(n), fd(n), eps
    real :: inner1, inner2

    eps = 1.0e-3
    a = (/1.0, 2.0, 3.0/)
    b = (/4.0, 5.0, 6.0/)
    call elementwise_add(n, a, b, c)
    call elementwise_add(n, a + eps, b + eps, c_eps)
    fd(:) = (c_eps(:) - c(:)) / eps
    a_ad(:) = 1.0
    b_ad(:) = 1.0
    call elementwise_add_fwd_ad(n, a, a_ad, b, b_ad, c, c_ad)
    if (any(abs((c_ad(:) - fd(:)) / fd(:)) > tol)) then
       print *, 'test_elementwise_add_fwd failed', c_ad(1), fd
       error stop 1
    end if

    inner1 = sum(c_ad(:)**2)
    call elementwise_add_rev_ad(n, a, a_ad, b, b_ad, c_ad)
    inner2 = sum(a_ad) + sum(b_ad)
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_elementwise_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_elementwise_add

  subroutine test_scale_array
    integer, parameter :: n = 3
    real :: a(n)
    real :: a_ad(n)
    real :: a_eps(n), fd(n), eps
    real :: inner1, inner2

    eps = 1.0e-3
    a = (/1.0, 2.0, 3.0/)
    a_ad(:) = 1.0
    call scale_array(n, a)
    a_eps = (/1.0, 2.0, 3.0/) + eps
    call scale_array(n, a_eps)
    fd(:) = (a_eps(:) - a(:)) / eps
    call scale_array_fwd_ad(n, a, a_ad)
    if (any(abs((a_ad(:) - fd(:)) / fd(:)) > tol)) then
       print *, 'test_scale_array_fwd failed'
       print *, maxval(abs((a_ad(:) - fd(:)) / fd(:)))
       print *, a_ad(:)
       print *, fd(:)
       error stop 1
    end if

    inner1 = sum(a_ad(:)**2)
    a = (/1.0, 2.0, 3.0/)
    call scale_array_rev_ad(n, a, a_ad)
    inner2 = sum(a_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_scale_array_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_scale_array

  subroutine test_multidimension
    real, parameter :: tol = 3e-4
    integer, parameter :: n = 2, m = 2
    real :: a(n,m), b(n,m), c, d(n,m)
    real :: a_ad(n,m), b_ad(n,m), c_ad, d_ad(n,m)
    real :: fd(n,m), eps, d_eps(n,m)
    real :: inner1, inner2

    eps = 1.0e-3
    a = reshape((/1.0, 2.0, 3.0, 4.0/), (/n, m/))
    b = reshape((/5.0, 6.0, 7.0, 8.0/), (/n, m/))
    c = 1.5
    call multidimension(n, m, a, b, c, d)
    call multidimension(n, m, a + eps, b + eps, c + eps, d_eps)
    fd(:,:) = (d_eps(:,:) - d(:,:)) / eps
    a_ad(:,:) = 1.0
    b_ad(:,:) = 1.0
    c_ad = 1.0
    call multidimension_fwd_ad(n, m, a, a_ad, b, b_ad, c, c_ad, d, d_ad)
    if (any(abs((d_ad(:,:) - fd(:,:)) / fd(:,:)) > tol)) then
       print *, 'test_multidimension_fwd failed'
       print *, maxval(abs((d_ad(:,:) - fd(:,:)) / fd(:,:)))
       print *, d_ad(:,:)
       print *, fd(:,:)
       error stop 1
    end if

    inner1 = sum(d_ad(:,:)**2)
    call multidimension_rev_ad(n, m, a, a_ad, b, b_ad, c, c_ad, d_ad)
    inner2 = sum(a_ad(:,:)) + sum(b_ad(:,:)) + c_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_multidimension_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_multidimension

  subroutine test_dot_product
    real, parameter :: tol = 2e-4
    integer, parameter :: n = 3
    real :: a(n), b(n), res
    real :: a_ad(n), b_ad(n), res_ad
    real :: res_eps, fd, eps
    real :: inner1, inner2

    eps = 1.0e-3
    a = (/1.0, 2.0, 3.0/)
    b = (/4.0, 5.0, 6.0/)
    res = dot_product(n, a, b)
    res_eps = dot_product(n, a + eps, b + eps)
    fd = (res_eps - res) / eps
    a_ad(:) = 1.0
    b_ad(:) = 1.0
    call dot_product_fwd_ad(n, a, a_ad, b, b_ad, res_ad)
    if (abs((res_ad - fd) / fd) > tol) then
       print *, 'test_dot_product_fwd failed', res_ad, fd
       error stop 1
    end if

    inner1 = res_ad**2
    call dot_product_rev_ad(n, a, a_ad, b, b_ad, res_ad)
    inner2 = sum(a_ad(:)) + sum(b_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_dot_product_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_dot_product

  subroutine test_indirect
    real, parameter :: tol = 6e-4
    integer, parameter :: n = 3
    real :: a(n), b(n), c(n)
    real :: a_ad(n), b_ad(n), c_ad(n)
    integer :: idx(n)
    real :: b_eps(n), c_eps(n), fd_b(n), fd_c(n), eps
    real :: inner1, inner2

    eps = 1.0e-3
    a = (/1.0, 2.0, 3.0/)
    a_ad(:) = 1.0
    idx = (/3, 1, 2/)
    call indirect(n, a, b, c, idx)
    call indirect(n, a + eps, b_eps, c_eps, idx)
    fd_b(:) = (b_eps(:) - b(:)) / eps
    fd_c(:) = (c_eps(:) - c(:)) / eps
    call indirect_fwd_ad(n, a, a_ad, b, b_ad, c, c_ad, idx)
    if (any(abs((b_ad(:) - fd_b(:)) / fd_b(:)) > tol) .or. &
        any(abs((c_ad(:) - fd_c(:)) / fd_c(:)) > tol)) then
       print *, 'test_indirect_fwd failed'
       print *, maxval(abs((b_ad(:) - fd_b(:)) / fd_b(:)))
       print *, maxval(abs((c_ad(:) - fd_c(:)) / fd_c(:)))
       print *, b_ad
       print *, fd_b
       print *, c_ad
       print *, fd_c
       error stop 1
    end if

    inner1 = sum(b_ad(:)**2) + sum(c_ad(:)**2)
    call indirect_rev_ad(n, a, a_ad, b_ad, c_ad, idx)
    inner2 = sum(a_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_indirect_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_indirect

  subroutine test_stencil
    real, parameter :: tol = 2e-4
    integer, parameter :: n = 3
    real :: a(n), b(n)
    real :: a_ad(n), b_ad(n)
    real :: b_eps(n), fd(n), eps
    real :: inner1, inner2

    eps = 1.0e-3
    a = (/1.0, 2.0, 3.0/)
    a_ad(:) = 1.0
    call stencil(n, a, b)
    call stencil(n, a + eps, b_eps)
    fd(:) = (b_eps(:) - b(:)) / eps
    call stencil_fwd_ad(n, a, a_ad, b, b_ad)
    if (any(abs((b_ad(:) - fd(:)) / fd(:)) > tol)) then
       print *, 'test_stencil_fwd failed'
       print *, maxval(abs((b_ad(:) - fd(:)) / fd(:)))
       print *, b_ad(:)
       print *, fd(:)
       error stop 1
    end if

    inner1 = sum(b_ad(:)**2)
    call stencil_rev_ad(n, a, a_ad, b_ad)
    inner2 = sum(a_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_stencil_rev failed', inner1, inner2
       error stop 1
    end if


    return
  end subroutine test_stencil

end program run_arrays
