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

  subroutine stencil_loop(n, m, x, y)
    integer, intent(in) :: n, m
    real, intent(in) :: x(n, m)
    real, intent(out) :: y(n, m)
    integer :: i, in, ip
    integer :: j, jn, jp

   !$omp parallel do private(in, ip, jn, jp)
    do j = 1, m
      jn = j - 1
      jp = j + 1
      if (j == 1) then
        jn = m
      else if (j == m) then
        jp = 1
      end if
      do i = 1, n
        in = i - 1
        ip = i + 1
        if (i == 1) then
          in = n
        else if (i == n) then
          ip = 1
        end if
        y(i,j) = (4.0 * x(i,j) + x(in,j) + x(ip,j) + x(i,jn) + x(i,jp)) / 8.0
      end do
    end do

    return
  end subroutine stencil_loop

  subroutine stencil_loop_mod(is, ie, x, y)
    integer, intent(in) :: is, ie
    real, intent(in) :: x(is:ie)
    real, intent(out) :: y(is:ie)
    real :: xn
    real :: xp
    real :: work
    integer :: i
    integer :: in
    integer :: ip
    integer :: len

    len = ie - is + 1

   !$omp parallel do private(in, ip, xn, xp, work)
    do i = is, ie
      in = modulo(i - is - 1, len) + is
      ip = modulo(i - is + 1, len) + is
      xn = x(in)
      xp = x(ip)
      if (x(i) > 0.0) then
        work = xn
      else
        work = xp
      end if
      if (work >= 0.0) then
        y(i) = work * (2.0 * x(i) + x(in)) / 4.0
      else
        y(i) = work * (2.0 * x(i) + x(ip)) / 4.0
      end if
    end do

    return
  end subroutine stencil_loop_mod

  subroutine stencil_loop_with_halo(is, ie, istart, iend, h, u, dhdt)
    integer, intent(in) :: is, ie
    integer, intent(in) :: istart, iend
    real, intent(in) :: h(is:ie)
    real, intent(in) :: u(is:ie)
    real, intent(out) :: dhdt(is:ie)
    real :: flux(2)
    integer :: i

   !$omp parallel do private(flux)
    do i = istart, iend
      flux(1) = u(i) * (- h(i+2) + 2.0 * h(i+1) - 2.0 * h(i) + h(i-1)) / 6.0
      flux(2) = u(i-1) * (- h(i+1) + 2.0 * h(i) - 2.0 * h(i-1) + h(i-2)) / 6.0
      dhdt(i) = - (flux(1) - flux(2))
    end do

    return
  end subroutine stencil_loop_with_halo

  subroutine indirect_access_loop(n, m, idx, x, y)
    integer, intent(in) :: n, m
    integer, intent(in) :: idx(n)
    real, intent(in) :: x(m)
    real, intent(out) :: y(n)
    integer :: i

    !$omp parallel do
    do i = 1, n
      y(i) = x(idx(i))
    end do
    !$omp end parallel do

    return
  end subroutine indirect_access_loop

  subroutine omp_ws_alloc(x, y)
    real, allocatable, intent(inout) :: x(:)
    real, intent(out) :: y(size(x))

    !$omp parallel
    !$omp workshare
    x = x**2
    y = x
    !$omp end workshare
    !$omp end parallel

    return
  end subroutine omp_ws_alloc

  subroutine omp_ws_if(x, y, f)
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    logical, intent(in) :: f

    !$omp parallel
    !$omp workshare
    y(:) = x(:)
    !$omp end workshare
    if (f) then
      !$omp workshare
      y(:) = y(:) + x(:)**2
      !$omp end workshare
    end if
    !$omp end parallel

    return
  end subroutine omp_ws_if

end module omp_loops
