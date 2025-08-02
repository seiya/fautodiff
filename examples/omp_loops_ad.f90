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

  subroutine sum_loop_rev_ad(n, x, x_ad, y_ad, s_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(out) :: x_ad(n)
    real, intent(inout) :: y_ad(n)
    real, intent(inout) :: s_ad
    integer :: i

    !$omp parallel do
    do i = n, 1, - 1
      y_ad(i) = s_ad + y_ad(i) ! s = s + y(i)
      x_ad(i) = y_ad(i) ! y(i) = x(i)
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

  subroutine stencil_loop_rev_ad(n, x, x_ad, y_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(out) :: x_ad(n)
    real, intent(inout) :: y_ad(n)
    integer :: i
    integer :: in
    integer :: ip

    x_ad(:) = 0.0

    do i = n, 1, - 1
      in = i - 1
      ip = i + 1
      if (i == 1) then
        in = n
      else if (i == n) then
        ip = 1
      end if
      x_ad(i) = y_ad(i) * 2.0 / 4.0 + x_ad(i) ! y(i) = (2.0 * x(i) + x(in) + x(ip)) / 4.0
      x_ad(in) = y_ad(i) / 4.0 + x_ad(in) ! y(i) = (2.0 * x(i) + x(in) + x(ip)) / 4.0
      x_ad(ip) = y_ad(i) / 4.0 + x_ad(ip) ! y(i) = (2.0 * x(i) + x(in) + x(ip)) / 4.0
      y_ad(i) = 0.0 ! y(i) = (2.0 * x(i) + x(in) + x(ip)) / 4.0
    end do

    return
  end subroutine stencil_loop_rev_ad

end module omp_loops_ad
