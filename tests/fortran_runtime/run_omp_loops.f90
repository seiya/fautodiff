program run_omp_loops
  use omp_loops
  use omp_loops_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_sum_loop = 1
  integer, parameter :: I_stencil_loop = 2
  integer, parameter :: I_stencil_loop_mod = 3
  integer, parameter :: I_stencil_loop_with_halo = 4
  integer, parameter :: I_indirect_access_loop = 5
  integer, parameter :: I_omp_ws_if = 6
  integer, parameter :: I_omp_ws_alloc = 7

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
           case ("sum_loop")
              i_test = I_sum_loop
           case ("stencil_loop")
              i_test = I_stencil_loop
           case ("stencil_loop_mod")
              i_test = I_stencil_loop_mod
           case ("stencil_loop_with_halo")
              i_test = I_stencil_loop_with_halo
           case ("indirect_access_loop")
              i_test = I_indirect_access_loop
           case ("omp_ws_if")
              i_test = I_omp_ws_if
           case ("omp_ws_alloc")
              i_test = I_omp_ws_alloc
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_sum_loop .or. i_test == I_all) then
     call test_sum_loop
  end if
  if (i_test == I_stencil_loop .or. i_test == I_all) then
     call test_stencil_loop
  end if
  if (i_test == I_stencil_loop_mod .or. i_test == I_all) then
     call test_stencil_loop_mod
  end if
  if (i_test == I_stencil_loop_with_halo .or. i_test == I_all) then
     call test_stencil_loop_with_halo
  end if
  if (i_test == I_indirect_access_loop .or. i_test == I_all) then
     call test_indirect_access_loop
  end if
  if (i_test == I_omp_ws_if .or. i_test == I_all) then
     call test_omp_ws_if
  end if
  if (i_test == I_omp_ws_alloc .or. i_test == I_all) then
     call test_omp_ws_alloc
  end if

  stop
contains

  subroutine test_sum_loop
    integer, parameter :: n = 3
    real :: x(n), y(n), s
    real :: x_ad(n), y_ad(n), s_ad
    real :: y_eps(n), s_eps, fd_y(n), fd_s, eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = (/1.0, 2.0, 3.0/)
    call sum_loop(n, x, y, s)
    call sum_loop(n, x + eps, y_eps, s_eps)
    fd_y(:) = (y_eps(:) - y(:)) / eps
    fd_s = (s_eps - s) / eps
    x_ad(:) = 1.0
    call sum_loop_fwd_ad(n, x, x_ad, y, y_ad, s, s_ad)
    if (any(abs((y_ad(:) - fd_y(:)) / max(abs(fd_y(:)), tol)) > tol) .or. &
        abs((s_ad - fd_s) / max(abs(fd_s), tol)) > tol) then
       print *, 'test_sum_loop_fwd failed'
       error stop 1
    end if

    inner1 = sum(y_ad(:)**2) + s_ad**2
    x_ad(:) = 0.0
    call sum_loop_rev_ad(n, x_ad, y_ad, s_ad)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / max(abs(inner1), tol)) > tol) then
      print *, 'test_sum_loop_rev failed', inner1, inner2
      error stop 1
    end if

    return
  end subroutine test_sum_loop

  subroutine test_stencil_loop
    real, parameter :: tol_stencil = 2.0e-4
    integer, parameter :: n = 3
    integer, parameter :: m = 4
    real :: x(n, m), y(n, m)
    real :: x_eps(n, m)
    real :: x_ad(n, m), y_ad(n, m)
    real :: y_eps(n, m), fd_y(n, m), eps
    real :: inner1, inner2
    integer :: i, j

    eps = 1.0e-3
    do j = 1, m
       do i = 1, n
          x(i, j) = 0.5 * real(i) + 0.25 * real(j)
       end do
    end do
    call stencil_loop(n, m, x, y)
    x_eps(:,:) = x(:,:) + eps
    call stencil_loop(n, m, x_eps, y_eps)
    fd_y(:,:) = (y_eps(:,:) - y(:,:)) / eps
    x_ad(:,:) = 1.0
    call stencil_loop_fwd_ad(n, m, x, x_ad, y, y_ad)
    if (any(abs((y_ad(:,:) - fd_y(:,:)) / max(abs(fd_y(:,:)), tol_stencil)) > tol_stencil)) then
       print *, 'test_stencil_loop_fwd failed'
       error stop 1
    end if

    inner1 = sum(y_ad(:,:)**2)
    x_ad(:,:) = 0.0
    call stencil_loop_rev_ad(n, m, x_ad, y_ad)
    inner2 = sum(x_ad(:,:))
    if (abs((inner2 - inner1) / max(abs(inner1), tol_stencil)) > tol_stencil) then
       print *, 'test_stencil_loop_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_stencil_loop

  subroutine test_stencil_loop_mod
    real, parameter :: tol_stencil = 4.1e-4
    integer, parameter :: n = 3
    integer, parameter :: is = 1
    integer, parameter :: ie = n
    real :: x(is:ie), y(is:ie)
    real :: x_eps(is:ie)
    real :: x_ad(is:ie), y_ad(is:ie)
    real :: y_eps(is:ie), fd_y(is:ie), eps
    real :: inner1, inner2

    eps = 1.0e-3
    x = (/1.0, 2.0, 3.0/)
    call stencil_loop_mod(is, ie, x, y)
    x_eps(:) = x(:) + eps
    call stencil_loop_mod(is, ie, x_eps, y_eps)
    fd_y(:) = (y_eps(:) - y(:)) / eps
    x_ad(:) = 1.0
    call stencil_loop_mod_fwd_ad(is, ie, x, x_ad, y, y_ad)
    if (any(abs((y_ad(:) - fd_y(:)) / fd_y(:)) > tol_stencil)) then
       print *, 'test_stencil_loop_mod_fwd failed', maxval(abs((y_ad(:) - fd_y(:)) / fd_y(:)))
       error stop 1
    end if

    inner1 = sum(y_ad(:)**2)
    x_ad(:) = 0.0
    call stencil_loop_mod_rev_ad(is, ie, x, x_ad, y_ad)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol_stencil) then
       print *, 'test_stencil_loop_mod_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_stencil_loop_mod

  subroutine test_stencil_loop_with_halo
    real, parameter :: tol_halo = 5.0e-4
    integer, parameter :: is = 0
    integer, parameter :: ie = 6
    integer, parameter :: istart = 2
    integer, parameter :: iend = 4
    real :: h(is:ie), u(is:ie)
    real :: h_eps(is:ie), u_eps(is:ie)
    real :: dhdt(is:ie), dhdt_eps(is:ie), dhdt_eps_minus(is:ie)
    real :: fd_dhdt(is:ie)
    real :: h_ad(is:ie), u_ad(is:ie), dhdt_ad(is:ie)
    real :: dhdt_bar(is:ie)
    real :: h_seed(is:ie), u_seed(is:ie)
    real :: inner1, inner2, eps
    integer :: i

    eps = 5.0e-4
    do i = is, ie
       h(i) = 0.5 + 0.1 * real(i)
       u(i) = 1.0 + 0.05 * real(i)
    end do

    ! Forward/Reverse checks for perturbations in h
    call stencil_loop_with_halo(is, ie, istart, iend, h, u, dhdt)
    do i = is, ie
       h_seed(i) = 10.0 * real(i - is + 1)
    end do
    h_eps(:) = h(:) + eps * h_seed(:)
    call stencil_loop_with_halo(is, ie, istart, iend, h_eps, u, dhdt_eps)
    h_eps(:) = h(:) - eps * h_seed(:)
    call stencil_loop_with_halo(is, ie, istart, iend, h_eps, u, dhdt_eps_minus)
    fd_dhdt(:) = 0.0
    fd_dhdt(istart:iend) = (dhdt_eps(istart:iend) - dhdt_eps_minus(istart:iend)) / (2.0 * eps)

    h_ad(:) = h_seed(:)
    u_ad(:) = 0.0
    call stencil_loop_with_halo_fwd_ad(is, ie, istart, iend, h, h_ad, u, u_ad, dhdt, dhdt_ad)
    if (any(abs((dhdt_ad(istart:iend) - fd_dhdt(istart:iend)) / &
         & max(1.0e-12, abs(fd_dhdt(istart:iend)))) > tol_halo)) then
       print *, 'test_stencil_loop_with_halo_fwd (h) failed'
       error stop 1
    end if

    dhdt_bar(:) = dhdt_ad(:)
    inner1 = sum(dhdt_ad(istart:iend) * dhdt_bar(istart:iend))
    h_ad(:) = 0.0
    u_ad(:) = 0.0
    dhdt_ad(:) = dhdt_bar(:)
    call stencil_loop_with_halo_rev_ad(is, ie, istart, iend, h, h_ad, u, u_ad, dhdt_ad)
    inner2 = sum(h_seed(:) * h_ad(:))
    if (abs((inner2 - inner1) / max(1.0e-12, abs(inner1))) > tol_halo) then
       print *, 'test_stencil_loop_with_halo_rev (h) failed', inner1, inner2
       error stop 1
    end if

    ! Forward/Reverse checks for perturbations in u
    call stencil_loop_with_halo(is, ie, istart, iend, h, u, dhdt)
    do i = is, ie
       u_seed(i) = 5.0 * real(i - is + 1)
    end do
    u_eps(:) = u(:) + eps * u_seed(:)
    call stencil_loop_with_halo(is, ie, istart, iend, h, u_eps, dhdt_eps)
    u_eps(:) = u(:) - eps * u_seed(:)
    call stencil_loop_with_halo(is, ie, istart, iend, h, u_eps, dhdt_eps_minus)
    fd_dhdt(:) = 0.0
    fd_dhdt(istart:iend) = (dhdt_eps(istart:iend) - dhdt_eps_minus(istart:iend)) / (2.0 * eps)

    h_ad(:) = 0.0
    u_ad(:) = u_seed(:)
    call stencil_loop_with_halo_fwd_ad(is, ie, istart, iend, h, h_ad, u, u_ad, dhdt, dhdt_ad)
    if (any(abs((dhdt_ad(istart:iend) - fd_dhdt(istart:iend)) / &
         & max(1.0e-12, abs(fd_dhdt(istart:iend)))) > tol_halo)) then
       print *, 'test_stencil_loop_with_halo_fwd (u) failed'
       error stop 1
    end if

    dhdt_bar(:) = dhdt_ad(:)
    inner1 = sum(dhdt_ad(istart:iend) * dhdt_bar(istart:iend))
    h_ad(:) = 0.0
    u_ad(:) = 0.0
    dhdt_ad(:) = dhdt_bar(:)
    call stencil_loop_with_halo_rev_ad(is, ie, istart, iend, h, h_ad, u, u_ad, dhdt_ad)
    inner2 = sum(u_seed(:) * u_ad(:))
    if (abs((inner2 - inner1) / max(1.0e-12, abs(inner1))) > tol_halo) then
       print *, 'test_stencil_loop_with_halo_rev (u) failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_stencil_loop_with_halo

  subroutine test_indirect_access_loop
    real, parameter :: tol_indirect = 1.0e-4
    integer, parameter :: n = 3
    integer, parameter :: m = 5
    integer :: idx(n)
    real :: x(m), x_eps_plus(m), x_eps_minus(m)
    real :: y(n), y_eps(n), y_eps_minus(n), fd_y(n)
    real :: x_ad(m), y_ad(n)
    real :: inner1, inner2, eps

    eps = 1.0e-3
    x = (/1.0, -0.5, 3.5, 4.0, -1.0/)
    idx = (/4, 3, 5/)

    call indirect_access_loop(n, m, idx, x, y)

    x_eps_plus(:) = x(:) + eps
    call indirect_access_loop(n, m, idx, x_eps_plus, y_eps)
    x_eps_minus(:) = x(:) - eps
    call indirect_access_loop(n, m, idx, x_eps_minus, y_eps_minus)
    fd_y(:) = (y_eps(:) - y_eps_minus(:)) / (2.0 * eps)

    x_ad(:) = 1.0
    call indirect_access_loop_fwd_ad(n, m, idx, x, x_ad, y, y_ad)
    if (any(abs((y_ad(:) - fd_y(:)) / max(1.0e-12, abs(fd_y(:)))) > tol_indirect)) then
       print *, 'test_indirect_access_loop_fwd failed'
       error stop 1
    end if

    inner1 = sum(y_ad(:)**2)
    x_ad(:) = 0.0
    call indirect_access_loop_rev_ad(n, m, idx, x_ad, y_ad)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / max(1.0e-12, abs(inner1))) > tol_indirect) then
       print *, 'test_indirect_access_loop_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_indirect_access_loop

  subroutine test_omp_ws_if
    integer, parameter :: n = 3
    real, parameter :: tol_ws = 5.0e-4
    real :: x(n), y(n)
    real :: x_ad(n), y_ad(n)
    real :: y_eps(n), fd_y(n), eps
    real :: inner1, inner2
    logical :: f

    eps = 1.0e-3
    x = (/1.0, 2.0, 3.0/)

    ! Case 1: f = .false. => y = x
    f = .false.
    call omp_ws_if(x, y, f)
    call omp_ws_if(x + eps, y_eps, f)
    fd_y(:) = (y_eps(:) - y(:)) / eps
    x_ad(:) = 1.0
    call omp_ws_if_fwd_ad(x, x_ad, y, y_ad, f)
    if (any(abs((y_ad(:) - fd_y(:)) / max(1.0e-12, abs(fd_y(:)))) > tol_ws)) then
       print *, 'test_omp_ws_if_fwd (f=false) failed'
       error stop 1
    end if

    inner1 = sum(y_ad(:)**2)
    x_ad(:) = 0.0
    call omp_ws_if_rev_ad(x, x_ad, y_ad, f)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / max(1.0e-12, inner1)) > tol_ws) then
      print *, 'test_omp_ws_if_rev (f=false) failed', inner1, inner2
      error stop 1
    end if

    ! Case 2: f = .true. => y = x + x**2
    f = .true.
    call omp_ws_if(x, y, f)
    call omp_ws_if(x + eps, y_eps, f)
    fd_y(:) = (y_eps(:) - y(:)) / eps
    x_ad(:) = 1.0
    call omp_ws_if_fwd_ad(x, x_ad, y, y_ad, f)
    if (any(abs((y_ad(:) - fd_y(:)) / max(1.0e-12, abs(fd_y(:)))) > tol_ws)) then
       print *, 'test_omp_ws_if_fwd (f=true) failed'
       error stop 1
    end if

    inner1 = sum(y_ad(:)**2)
    x_ad(:) = 0.0
    call omp_ws_if_rev_ad(x, x_ad, y_ad, f)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / max(1.0e-12, inner1)) > tol_ws) then
      print *, 'test_omp_ws_if_rev (f=true) failed', inner1, inner2
      error stop 1
    end if

    return
  end subroutine test_omp_ws_if

  subroutine test_omp_ws_alloc
    real, parameter :: eps = 1.0e-3
    integer, parameter :: n = 4
    real, parameter :: tol_alloc = 1.0e-3
    real :: fd(n)
    real :: inner1, inner2
    real, allocatable :: x(:), x_seed(:), x_ad(:), x_out_ad(:)
    real, allocatable :: y(:), y_eps(:), y_ad(:)
    integer :: i

    allocate(x_seed(n))
    do i = 1, n
       x_seed(i) = real(i)
    end do

    allocate(x(n), y(n))
    x = x_seed
    call omp_ws_alloc(x, y)

    allocate(y_eps(n))
    y_eps = 0.0
    x = x_seed + eps
    call omp_ws_alloc(x, y_eps)
    fd(:) = (y_eps(:) - y(:)) / eps

    x = x_seed
    allocate(x_ad(n), y_ad(n), x_out_ad(n))
    x_ad = 1.0
    call omp_ws_alloc_fwd_ad(x, x_ad, y, y_ad)
    if (maxval(abs((y_ad(:) - fd(:)) / max(abs(fd(:)), tol_alloc))) > tol_alloc) then
       print *, 'test_omp_ws_alloc_fwd failed'
       print *, maxval(abs((y_ad(:) - fd(:)) / max(abs(fd(:)), tol_alloc)))
       error stop 1
    end if

    x_out_ad(:) = x_ad(:)
    inner1 = sum(y_ad(:)**2) + sum(x_out_ad(:)**2)
    x(:) = x_seed(:)
    x_ad(:) = x_out_ad(:)
    call omp_ws_alloc_rev_ad(x, x_ad, y_ad)
    inner2 = sum(x_ad(:))
    if (abs((inner2 - inner1) / max(abs(inner1), tol_alloc)) > tol_alloc) then
       print *, 'test_omp_ws_alloc_rev failed', inner1, inner2
       error stop 1
    end if

    deallocate(x)
    deallocate(x_seed)
    deallocate(x_ad)
    deallocate(x_out_ad)
    deallocate(y)
    deallocate(y_eps)
    deallocate(y_ad)

    return
  end subroutine test_omp_ws_alloc

end program run_omp_loops
