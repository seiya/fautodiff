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

  subroutine stencil_loop_fwd_ad(n, m, x, x_ad, y, y_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x(n,m)
    real, intent(in)  :: x_ad(n,m)
    real, intent(out) :: y(n,m)
    real, intent(out) :: y_ad(n,m)
    integer :: j
    integer :: jn
    integer :: jp
    integer :: i
    integer :: in
    integer :: ip

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
        y_ad(i,j) = x_ad(i,j) * 4.0 / 8.0 + x_ad(in,j) / 8.0 + x_ad(ip,j) / 8.0 + x_ad(i,jn) / 8.0 + x_ad(i,jp) / 8.0
          ! y(i,j) = (4.0 * x(i,j) + x(in,j) + x(ip,j) + x(i,jn) + x(i,jp)) / 8.0
        y(i,j) = (4.0 * x(i,j) + x(in,j) + x(ip,j) + x(i,jn) + x(i,jp)) / 8.0
      end do
    end do
    !$omp end parallel do

    return
  end subroutine stencil_loop_fwd_ad

  subroutine stencil_loop_rev_ad(n, m, x_ad, y_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(inout) :: x_ad(n,m)
    real, intent(inout) :: y_ad(n,m)
    integer :: j
    integer :: jn
    integer :: jp
    integer :: i
    integer :: in
    integer :: ip

    !$omp parallel do private(in, ip, jn, jp)
    do j = m, 1, - 1
      jn = j - 1
      jp = j + 1
      if (j == 1) then
        jn = m
      else if (j == m) then
        jp = 1
      end if
      do i = n, 1, - 1
        in = i - 1
        ip = i + 1
        if (i == 1) then
          in = n
        else if (i == n) then
          ip = 1
        end if
        x_ad(i,j) = y_ad(i,j) * 4.0 / 8.0 + x_ad(i,j) ! y(i,j) = (4.0 * x(i,j) + x(in,j) + x(ip,j) + x(i,jn) + x(i,jp)) / 8.0
        x_ad(in,j) = y_ad(i,j) / 8.0 + x_ad(in,j) ! y(i,j) = (4.0 * x(i,j) + x(in,j) + x(ip,j) + x(i,jn) + x(i,jp)) / 8.0
        x_ad(ip,j) = y_ad(i,j) / 8.0 + x_ad(ip,j) ! y(i,j) = (4.0 * x(i,j) + x(in,j) + x(ip,j) + x(i,jn) + x(i,jp)) / 8.0
        x_ad(i,j) = y_ad(i,jp) / 8.0 + y_ad(i,jn) / 8.0 + x_ad(i,j)
          ! y(i,j) = (4.0 * x(i,j) + x(in,j) + x(ip,j) + x(i,jn) + x(i,jp)) / 8.0
      end do
    end do
    !$omp end parallel do
    !$omp parallel do
    do j = m, 1, - 1
      do i = n, 1, - 1
        y_ad(i,j) = 0.0 ! y(i,j) = (4.0 * x(i,j) + x(in,j) + x(ip,j) + x(i,jn) + x(i,jp)) / 8.0
      end do
    end do
    !$omp end parallel do

    return
  end subroutine stencil_loop_rev_ad

  subroutine stencil_loop_mod_fwd_ad(is, ie, x, x_ad, y, y_ad)
    integer, intent(in)  :: is
    integer, intent(in)  :: ie
    real, intent(in)  :: x(is:ie)
    real, intent(in)  :: x_ad(is:ie)
    real, intent(out) :: y(is:ie)
    real, intent(out) :: y_ad(is:ie)
    real :: work_ad
    integer :: len
    integer :: i
    integer :: in
    integer :: ip
    real :: work

    len = ie - is + 1
    !$omp parallel do private(in, ip, work, work_ad)
    do i = is, ie
      in = modulo(i - is - 1, len) + is
      ip = modulo(i - is + 1, len) + is
      if (x(i) > 0.0) then
        work_ad = x_ad(in) ! work = x(in)
        work = x(in)
      else
        work_ad = x_ad(ip) ! work = x(ip)
        work = x(ip)
      end if
      if (work >= 0.0) then
        y_ad(i) = work_ad * (2.0 * x(i) + x(in)) / 4.0 + x_ad(i) * work * 2.0 / 4.0 + x_ad(in) * work / 4.0
          ! y(i) = work * (2.0 * x(i) + x(in)) / 4.0
        y(i) = work * (2.0 * x(i) + x(in)) / 4.0
      else
        y_ad(i) = work_ad * (2.0 * x(i) + x(ip)) / 4.0 + x_ad(i) * work * 2.0 / 4.0 + x_ad(ip) * work / 4.0
          ! y(i) = work * (2.0 * x(i) + x(ip)) / 4.0
        y(i) = work * (2.0 * x(i) + x(ip)) / 4.0
      end if
    end do
    !$omp end parallel do

    return
  end subroutine stencil_loop_mod_fwd_ad

  subroutine stencil_loop_mod_rev_ad(is, ie, x, x_ad, y_ad)
    integer, intent(in)  :: is
    integer, intent(in)  :: ie
    real, intent(in)  :: x(is:ie)
    real, intent(inout) :: x_ad(is:ie)
    real, intent(inout) :: y_ad(is:ie)
    integer :: in_n1_ad
    integer :: ip_p1_ad
    real :: work_ad_n1_ad
    real :: work_ad_p1_ad
    real :: work_n1_ad
    real :: work_p1_ad
    integer :: len
    integer :: i
    integer :: in
    integer :: ip
    real :: work

    len = ie - is + 1

    !$omp parallel do private(in, ip, work, in_n1_ad, ip_p1_ad, work_n1_ad, work_p1_ad, work_ad_n1_ad, work_ad_p1_ad)
    do i = ie, is, - 1
      in_n1_ad = modulo(i - is - 2, len) + is
      in = modulo(i - is - 1, len) + is
      ip = modulo(i - is + 1, len) + is
      ip_p1_ad = modulo(i - is + 2, len) + is
      if (x(in) > 0.0) then
        work_n1_ad = x(in_n1_ad)
      else
        work_n1_ad = x(i)
      end if
      if (x(i) > 0.0) then
        work = x(in)
      else
        work = x(ip)
      end if
      if (x(ip) > 0.0) then
        work_p1_ad = x(i)
      else
        work_p1_ad = x(ip_p1_ad)
      end if
      if (work_n1_ad >= 0.0) then
        work_ad_n1_ad = y_ad(in) * (2.0 * x(in) + x(in_n1_ad)) / 4.0 ! y(i) = work * (2.0 * x(i) + x(in)) / 4.0
      else
        work_ad_n1_ad = y_ad(in) * (2.0 * x(in) + x(i)) / 4.0 ! y(i) = work * (2.0 * x(i) + x(ip)) / 4.0
        x_ad(i) = y_ad(in) * work_n1_ad / 4.0 + x_ad(i) ! y(i) = work * (2.0 * x(i) + x(ip)) / 4.0
      end if
      if (work >= 0.0) then
        x_ad(i) = y_ad(i) * work * 2.0 / 4.0 + x_ad(i) ! y(i) = work * (2.0 * x(i) + x(in)) / 4.0
      else
        x_ad(i) = y_ad(i) * work * 2.0 / 4.0 + x_ad(i) ! y(i) = work * (2.0 * x(i) + x(ip)) / 4.0
      end if
      if (work_p1_ad >= 0.0) then
        work_ad_p1_ad = y_ad(ip) * (2.0 * x(ip) + x(i)) / 4.0 ! y(i) = work * (2.0 * x(i) + x(in)) / 4.0
        x_ad(i) = y_ad(ip) * work_p1_ad / 4.0 + x_ad(i) ! y(i) = work * (2.0 * x(i) + x(in)) / 4.0
      else
        work_ad_p1_ad = y_ad(ip) * (2.0 * x(ip) + x(ip_p1_ad)) / 4.0 ! y(i) = work * (2.0 * x(i) + x(ip)) / 4.0
      end if
      if (x(in) > 0.0) then
      else
        x_ad(i) = work_ad_n1_ad + x_ad(i) ! work = x(ip)
      end if
      if (x(ip) > 0.0) then
        x_ad(i) = work_ad_p1_ad + x_ad(i) ! work = x(in)
      end if
    end do
    !$omp end parallel do
    !$omp parallel do
    do i = ie, is, - 1
      y_ad(i) = 0.0 ! y(i) = work * (2.0 * x(i) + x(in)) / 4.0, y(i) = work * (2.0 * x(i) + x(ip)) / 4.0
    end do
    !$omp end parallel do

    return
  end subroutine stencil_loop_mod_rev_ad

  subroutine stencil_loop_with_halo_fwd_ad(is, ie, istart, iend, h, h_ad, u, u_ad, dhdt, dhdt_ad)
    integer, intent(in)  :: is
    integer, intent(in)  :: ie
    integer, intent(in)  :: istart
    integer, intent(in)  :: iend
    real, intent(in)  :: h(is:ie)
    real, intent(in)  :: h_ad(is:ie)
    real, intent(in)  :: u(is:ie)
    real, intent(in)  :: u_ad(is:ie)
    real, intent(out) :: dhdt(is:ie)
    real, intent(out) :: dhdt_ad(is:ie)
    real :: flux_ad(2)
    integer :: i
    real :: flux(2)

    !$omp parallel do private(flux, flux_ad)
    do i = istart, iend
      flux_ad(1) = u_ad(i) * (- h(i + 2) + 2.0 * h(i + 1) - 2.0 * h(i) + h(i - 1)) / 6.0 - h_ad(i + 2) * u(i) / 6.0 &
                   + h_ad(i + 1) * u(i) * 2.0 / 6.0 - h_ad(i) * u(i) * 2.0 / 6.0 + h_ad(i - 1) * u(i) / 6.0
        ! flux(1) = u(i) * (- h(i+2) + 2.0 * h(i+1) - 2.0 * h(i) + h(i-1)) / 6.0
      flux(1) = u(i) * (- h(i + 2) + 2.0 * h(i + 1) - 2.0 * h(i) + h(i - 1)) / 6.0
      flux_ad(2) = u_ad(i - 1) * (- h(i + 1) + 2.0 * h(i) - 2.0 * h(i - 1) + h(i - 2)) / 6.0 - h_ad(i + 1) * u(i - 1) / 6.0 &
                   + h_ad(i) * u(i - 1) * 2.0 / 6.0 - h_ad(i - 1) * u(i - 1) * 2.0 / 6.0 + h_ad(i - 2) * u(i - 1) / 6.0
        ! flux(2) = u(i-1) * (- h(i+1) + 2.0 * h(i) - 2.0 * h(i-1) + h(i-2)) / 6.0
      flux(2) = u(i - 1) * (- h(i + 1) + 2.0 * h(i) - 2.0 * h(i - 1) + h(i - 2)) / 6.0
      dhdt_ad(i) = - flux_ad(1) + flux_ad(2) ! dhdt(i) = - (flux(1) - flux(2))
      dhdt(i) = - (flux(1) - flux(2))
    end do
    !$omp end parallel do

    return
  end subroutine stencil_loop_with_halo_fwd_ad

  subroutine stencil_loop_with_halo_rev_ad(is, ie, istart, iend, h, h_ad, u, u_ad, dhdt_ad)
    integer, intent(in)  :: is
    integer, intent(in)  :: ie
    integer, intent(in)  :: istart
    integer, intent(in)  :: iend
    real, intent(in)  :: h(is:ie)
    real, intent(inout) :: h_ad(is:ie)
    real, intent(in)  :: u(is:ie)
    real, intent(inout) :: u_ad(is:ie)
    real, intent(inout) :: dhdt_ad(is:ie)
    real :: flux_ad(2)
    real :: flux_ad_n1_ad(2)
    real :: flux_ad_n2_ad(2)
    real :: flux_ad_p1_ad(2)
    real :: flux_ad_p2_ad(2)
    integer :: i

    !$omp parallel do private(flux_ad, flux_ad_n2_ad, flux_ad_n1_ad, flux_ad_p1_ad, flux_ad_p2_ad)
    do i = iend + 2, istart - 2, - 1
      if (i - 2 >= istart) then
        flux_ad_n2_ad(1) = - dhdt_ad(i - 2) ! dhdt(i) = - (flux(1) - flux(2))
      end if
      if (i - 1 >= istart .and. i - 1 <= iend) then
        flux_ad_n1_ad(1) = - dhdt_ad(i - 1) ! dhdt(i) = - (flux(1) - flux(2))
      end if
      if (i >= istart .and. i <= iend) then
        flux_ad(1) = - dhdt_ad(i) ! dhdt(i) = - (flux(1) - flux(2))
      end if
      if (i + 1 >= istart .and. i + 1 <= iend) then
        flux_ad_p1_ad(1) = - dhdt_ad(i + 1) ! dhdt(i) = - (flux(1) - flux(2))
      end if
      if (i - 1 >= istart .and. i - 1 <= iend) then
        flux_ad_n1_ad(2) = dhdt_ad(i - 1) ! dhdt(i) = - (flux(1) - flux(2))
      end if
      if (i >= istart .and. i <= iend) then
        flux_ad(2) = dhdt_ad(i) ! dhdt(i) = - (flux(1) - flux(2))
      end if
      if (i + 1 >= istart .and. i + 1 <= iend) then
        flux_ad_p1_ad(2) = dhdt_ad(i + 1) ! dhdt(i) = - (flux(1) - flux(2))
      end if
      if (i + 2 <= iend) then
        flux_ad_p2_ad(2) = dhdt_ad(i + 2) ! dhdt(i) = - (flux(1) - flux(2))
      end if
      if (i + 1 >= istart .and. i + 1 <= iend) then
        u_ad(i) = flux_ad_p1_ad(2) * (- h(i + 2) + 2.0 * h(i + 1) - 2.0 * h(i) + h(i - 1)) / 6.0 + u_ad(i)
          ! flux(2) = u(i-1) * (- h(i+1) + 2.0 * h(i) - 2.0 * h(i-1) + h(i-2)) / 6.0
      end if
      if (i - 1 >= istart .and. i - 1 <= iend) then
        h_ad(i) = - flux_ad_n1_ad(2) * u(i - 2) / 6.0 + h_ad(i)
          ! flux(2) = u(i-1) * (- h(i+1) + 2.0 * h(i) - 2.0 * h(i-1) + h(i-2)) / 6.0
      end if
      if (i >= istart .and. i <= iend) then
        h_ad(i) = flux_ad(2) * u(i - 1) * 2.0 / 6.0 + h_ad(i)
          ! flux(2) = u(i-1) * (- h(i+1) + 2.0 * h(i) - 2.0 * h(i-1) + h(i-2)) / 6.0
      end if
      if (i + 1 >= istart .and. i + 1 <= iend) then
        h_ad(i) = - flux_ad_p1_ad(2) * u(i) * 2.0 / 6.0 + h_ad(i)
          ! flux(2) = u(i-1) * (- h(i+1) + 2.0 * h(i) - 2.0 * h(i-1) + h(i-2)) / 6.0
      end if
      if (i + 2 <= iend) then
        h_ad(i) = flux_ad_p2_ad(2) * u(i + 1) / 6.0 + h_ad(i)
          ! flux(2) = u(i-1) * (- h(i+1) + 2.0 * h(i) - 2.0 * h(i-1) + h(i-2)) / 6.0
      end if
      if (i >= istart .and. i <= iend) then
        u_ad(i) = flux_ad(1) * (- h(i + 2) + 2.0 * h(i + 1) - 2.0 * h(i) + h(i - 1)) / 6.0 + u_ad(i)
          ! flux(1) = u(i) * (- h(i+2) + 2.0 * h(i+1) - 2.0 * h(i) + h(i-1)) / 6.0
      end if
      if (i - 2 >= istart) then
        h_ad(i) = - flux_ad_n2_ad(1) * u(i - 2) / 6.0 + h_ad(i)
          ! flux(1) = u(i) * (- h(i+2) + 2.0 * h(i+1) - 2.0 * h(i) + h(i-1)) / 6.0
      end if
      if (i - 1 >= istart .and. i - 1 <= iend) then
        h_ad(i) = flux_ad_n1_ad(1) * u(i - 1) * 2.0 / 6.0 + h_ad(i)
          ! flux(1) = u(i) * (- h(i+2) + 2.0 * h(i+1) - 2.0 * h(i) + h(i-1)) / 6.0
      end if
      if (i >= istart .and. i <= iend) then
        h_ad(i) = - flux_ad(1) * u(i) * 2.0 / 6.0 + h_ad(i) ! flux(1) = u(i) * (- h(i+2) + 2.0 * h(i+1) - 2.0 * h(i) + h(i-1)) / 6.0
      end if
      if (i + 1 >= istart .and. i + 1 <= iend) then
        h_ad(i) = flux_ad_p1_ad(1) * u(i + 1) / 6.0 + h_ad(i)
          ! flux(1) = u(i) * (- h(i+2) + 2.0 * h(i+1) - 2.0 * h(i) + h(i-1)) / 6.0
      end if
    end do
    !$omp end parallel do
    !$omp parallel do
    do i = iend, istart, - 1
      dhdt_ad(i) = 0.0 ! dhdt(i) = - (flux(1) - flux(2))
    end do
    !$omp end parallel do

    return
  end subroutine stencil_loop_with_halo_rev_ad

  subroutine indirect_access_loop_fwd_ad(n, m, idx, x, x_ad, y, y_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: idx(n)
    real, intent(in)  :: x(m)
    real, intent(in)  :: x_ad(m)
    real, intent(out) :: y(n)
    real, intent(out) :: y_ad(n)
    integer :: i

    !$omp parallel do
    do i = 1, n
      y_ad(i) = x_ad(idx(i)) ! y(i) = x(idx(i))
      y(i) = x(idx(i))
    end do
    !$omp end parallel do

    return
  end subroutine indirect_access_loop_fwd_ad

  subroutine indirect_access_loop_rev_ad(n, m, idx, x_ad, y_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    integer, intent(in)  :: idx(n)
    real, intent(inout) :: x_ad(m)
    real, intent(inout) :: y_ad(n)
    integer :: i

    do i = n, 1, - 1
      x_ad(idx(i)) = y_ad(i) + x_ad(idx(i)) ! y(i) = x(idx(i))
      y_ad(i) = 0.0 ! y(i) = x(idx(i))
    end do

    return
  end subroutine indirect_access_loop_rev_ad

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
    real, allocatable :: x_save_134_ad(:)

    allocate(x_save_134_ad, mold=x)
    !$omp parallel
    !$omp workshare
    x_save_134_ad(:) = x(:)
    !$omp end workshare
    !$omp end parallel

    !$omp parallel
    !$omp workshare
    x_ad = y_ad + x_ad ! y = x
    y_ad = 0.0 ! y = x
    x(:) = x_save_134_ad(:)
    x_ad = x_ad * 2.0 * x ! x = x**2
    !$omp end workshare
    !$omp end parallel
    if (allocated(x_save_134_ad)) then
      deallocate(x_save_134_ad)
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
