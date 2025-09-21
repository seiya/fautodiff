module omp_loops_ad
  use omp_loops
  implicit none

contains

  subroutine sum_loop_fwd_ad(n, x, x_ad, y, y_ad, s, s_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(in)  :: x_ad(n)
    real, intent(out) :: y(n)
    real, intent(out) :: y_ad(n)
    real, intent(out) :: s
    real, intent(out) :: s_ad
    integer :: i

    s_ad = 0.0 ! s = 0.0
    s = 0.0
    !$omp parallel do reduction(+:s, s_ad)
    do i = 1, n
      y_ad(i) = x_ad(i) ! y(i) = x(i)
      y(i) = x(i)
      s_ad = s_ad + y_ad(i) ! s = s + y(i)
      s = s + y(i)
    end do
    !$omp end parallel do

    return
  end subroutine sum_loop_fwd_ad

  subroutine sum_loop_rev_ad(n, x_ad, y_ad, s_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: x_ad(n)
    real, intent(inout) :: y_ad(n)
    real, intent(inout) :: s_ad
    integer :: i

    !$omp parallel do
    do i = n, 1, - 1
      y_ad(i) = s_ad + y_ad(i) ! s = s + y(i)
      x_ad(i) = y_ad(i) + x_ad(i) ! y(i) = x(i)
    end do
    !$omp end parallel do
    s_ad = 0.0 ! s = 0.0
    y_ad = 0.0 ! y = 0.0

    return
  end subroutine sum_loop_rev_ad

  subroutine stencil_loop_fwd_ad(n, x, x_ad, y, y_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(in)  :: x_ad(n)
    real, intent(out) :: y(n)
    real, intent(out) :: y_ad(n)
    integer :: i
    integer :: in
    integer :: ip

    !$omp parallel do private(in, ip)
    do i = 1, n
      in = i - 1
      ip = i + 1
      if (i == 1) then
        in = n
      else if (i == n) then
        ip = 1
      end if
      y_ad(i) = x_ad(i) * 2.0 / 4.0 + x_ad(in) / 4.0 + x_ad(ip) / 4.0 ! y(i) = (2.0 * x(i) + x(in) + x(ip)) / 4.0
      y(i) = (2.0 * x(i) + x(in) + x(ip)) / 4.0
    end do
    !$omp end parallel do

    return
  end subroutine stencil_loop_fwd_ad

  subroutine stencil_loop_rev_ad(n, x_ad, y_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: x_ad(n)
    real, intent(inout) :: y_ad(n)
    integer :: i
    integer :: in
    integer :: ip

    !$omp parallel do private(in, ip)
    do i = n, 1, - 1
      in = i - 1
      ip = i + 1
      if (i == 1) then
        in = n
      else if (i == n) then
        ip = 1
      end if
      x_ad(i) = y_ad(in) / 4.0 + y_ad(ip) / 4.0 + y_ad(i) * 2.0 / 4.0 + x_ad(i)
      ! y(i) = (2.0 * x(i) + x(in) + x(ip)) / 4.0; y(i) = (2.0 * x(i) + x(in) + x(ip)) / 4.0; y(i) = (2.0 * x(i) + x(in) + x(ip)) /
      ! 4.0
    end do
    !$omp end parallel do
    y_ad = 0.0 ! y(i) = (2.0 * x(i) + x(in) + x(ip)) / 4.0

    return
  end subroutine stencil_loop_rev_ad

  subroutine omp_ws_alloc_fwd_ad(x, x_ad, y, y_ad)
    real, intent(inout), allocatable :: x(:)
    real, intent(inout), allocatable :: x_ad(:)
    real, intent(out) :: y(size(x))
    real, intent(out) :: y_ad(size(x))

    !$omp parallel
    !$omp workshare
    x_ad = x_ad * 2.0 * x ! x = x**2
    x = x**2
    y_ad = x_ad ! y = x
    y = x
    !$omp end workshare
    !$omp end parallel

    return
  end subroutine omp_ws_alloc_fwd_ad

  subroutine omp_ws_alloc_rev_ad(x, x_ad, y_ad)
    real, intent(inout), allocatable :: x(:)
    real, intent(inout), allocatable :: x_ad(:)
    real, intent(inout) :: y_ad(size(x))
    real, allocatable :: x_save_53_ad(:)

    allocate(x_save_53_ad, mold=x)
    !$omp parallel
    !$omp workshare
    x_save_53_ad(:) = x(:)
    !$omp end workshare
    !$omp end parallel

    !$omp parallel
    !$omp workshare
    x_ad = y_ad + x_ad ! y = x
    y_ad = 0.0 ! y = x
    x(:) = x_save_53_ad(:)
    x_ad = x_ad * 2.0 * x ! x = x**2
    !$omp end workshare
    !$omp end parallel
    if (allocated(x_save_53_ad)) then
      deallocate(x_save_53_ad)
    end if

    return
  end subroutine omp_ws_alloc_rev_ad

  subroutine omp_ws_if_fwd_ad(x, x_ad, y, y_ad, f)
    real, intent(in)  :: x(:)
    real, intent(in)  :: x_ad(:)
    real, intent(out) :: y(:)
    real, intent(out) :: y_ad(:)
    logical, intent(in)  :: f

    !$omp parallel
    !$omp workshare
    y_ad(:) = x_ad(:) ! y(:) = x(:)
    y(:) = x(:)
    !$omp end workshare
    if (f) then
      !$omp workshare
      y_ad(:) = y_ad(:) + x_ad(:) * 2.0 * x(:) ! y(:) = y(:) + x(:)**2
      y(:) = y(:) + x(:)**2
      !$omp end workshare
    end if
    !$omp end parallel

    return
  end subroutine omp_ws_if_fwd_ad

  subroutine omp_ws_if_rev_ad(x, x_ad, y_ad, f)
    real, intent(in)  :: x(:)
    real, intent(inout) :: x_ad(:)
    real, intent(inout) :: y_ad(:)
    logical, intent(in)  :: f

    !$omp parallel
    if (f) then
      !$omp workshare
      x_ad(:) = y_ad(:) * 2.0 * x(:) + x_ad(:) ! y(:) = y(:) + x(:)**2
      !$omp end workshare
    end if
    !$omp workshare
    x_ad(:) = y_ad(:) + x_ad(:) ! y(:) = x(:)
    y_ad(:) = 0.0 ! y(:) = x(:)
    !$omp end workshare
    !$omp end parallel

    return
  end subroutine omp_ws_if_rev_ad

end module omp_loops_ad
