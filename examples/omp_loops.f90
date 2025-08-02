module omp_loops
  implicit none

contains

  subroutine sum_loop(n, x, y, s)
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    real, intent(out) :: y(n)
    real, intent(out) :: s
    integer :: i

    y = 0.0
    s = 0.0
    !$omp parallel do reduction(+:s)
    do i = 1, n
      y(i) = x(i)
      s = s + y(i)
    end do
    !$omp end parallel do

    return
  end subroutine sum_loop

  subroutine stencil_loop(n, x, y)
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    real, intent(out) :: y(n)
    integer :: i, in, ip

   !$omp parallel do private(in, ip)
    do i = 1, n
      in = i - 1
      ip = i + 1
      if (i == 1) then
        in = n
      else if (i == n) then
        ip = 1
      end if
      y(i) = (2.0 * x(i) + x(in) + x(ip)) / 4.0
    end do

    return
  end subroutine stencil_loop
end module omp_loops
