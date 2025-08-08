program run_self_reference
  use self_reference
  use self_reference_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_slice = 1
  integer, parameter :: I_slice_ptr = 2
  integer, parameter :: I_slice_expr = 3

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
           case ("slice")
              i_test = I_slice
           case ("slice_ptr")
              i_test = I_slice_ptr
           case ("slice_expr")
              i_test = I_slice_expr
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_slice .or. i_test == I_all) then
     call test_slice
  end if
  if (i_test == I_slice_ptr .or. i_test == I_all) then
     call test_slice_ptr
  end if
  if (i_test == I_slice_expr .or. i_test == I_all) then
     call test_slice_expr
  end if

  print *, 'OK'
  stop

contains

  subroutine test_slice
    integer, parameter :: n_tot = 5
    real :: u(n_tot), u_eps(n_tot), u_ad(n_tot)
    real :: fd(n_tot), eps, inner1, inner2

    eps = 1.0e-3
    u = (/1.0, 2.0, 3.0, 4.0, 5.0/)
    call self_ref_slice(u, 3, 5, 1, 3)
    u_eps = (/1.0, 2.0, 3.0, 4.0, 5.0/) + eps
    call self_ref_slice(u_eps, 3, 5, 1, 3)
    fd(:) = (u_eps(:) - u(:)) / eps
    u = (/1.0, 2.0, 3.0, 4.0, 5.0/)
    u_ad(:) = 1.0
    call self_ref_slice_fwd_ad(u, u_ad, 3, 5, 1, 3)
    if (any(abs((u_ad(:) - fd(:)) / fd(:)) > tol)) then
       print *, 'test_slice_fwd failed'
       error stop 1
    end if

    inner1 = sum(u_ad(:)**2)
    call self_ref_slice_rev_ad(u, u_ad, 3, 5, 1, 3)
    inner2 = sum(u_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_slice_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_slice

  subroutine test_slice_ptr
    integer, parameter :: n_tot = 5
    real, parameter :: tol_ptr = 5.0e-4
    real :: u(n_tot), v(n_tot)
    real :: u_eps(n_tot), v_eps(n_tot)
    real :: u_ad(n_tot), v_ad(n_tot)
    real :: fd_u(n_tot), fd_v(n_tot)
    real :: eps, inner1, inner2

    eps = 1.0e-3
    u = (/1.0, 2.0, 3.0, 4.0, 5.0/)
    v = (/6.0, 7.0, 8.0, 9.0, 10.0/)
    call self_ref_slice_ptr(u, v, 3, 5, 1, 3)
    u_eps = (/1.0, 2.0, 3.0, 4.0, 5.0/) + eps
    v_eps = (/6.0, 7.0, 8.0, 9.0, 10.0/) + eps
    call self_ref_slice_ptr(u_eps, v_eps, 3, 5, 1, 3)
    fd_u(:) = (u_eps(:) - u(:)) / eps
    fd_v(:) = (v_eps(:) - v(:)) / eps
    u = (/1.0, 2.0, 3.0, 4.0, 5.0/)
    v = (/6.0, 7.0, 8.0, 9.0, 10.0/)
    u_ad(:) = 1.0
    v_ad(:) = 1.0
    call self_ref_slice_ptr_fwd_ad(u, u_ad, v, v_ad, 3, 5, 1, 3)
    if (any(abs((u_ad(:) - fd_u(:)) / fd_u(:)) > tol_ptr) .or. &
        any(abs((v_ad(:) - fd_v(:)) / fd_v(:)) > tol_ptr)) then
       print *, 'test_slice_ptr_fwd failed'
       error stop 1
    end if

    inner1 = sum(u_ad(:)**2) + sum(v_ad(:)**2)
    call self_ref_slice_ptr_rev_ad(u, u_ad, v, v_ad, 3, 5, 1, 3)
    inner2 = sum(u_ad(:)) + sum(v_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol_ptr) then
       print *, 'test_slice_ptr_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_slice_ptr

  subroutine test_slice_expr
    integer, parameter :: n = 3, m = 3
    real, parameter :: tol_expr = 1.0e-2
    real :: u(n,m), v(n,m), w(n,m)
    real :: u_eps(n,m), v_eps(n,m), w_eps(n,m)
    real :: u_ad(n,m), v_ad(n,m), w_ad(n,m)
    real :: fd_u(n,m), fd_w(n,m)
    real :: eps, inner1, inner2

    eps = 1.0e-3
    u = reshape((/1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0/), (/n,m/))
    v = reshape((/9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0/), (/n,m/))
    w = reshape((/1.0,1.0,1.0,2.0,2.0,2.0,3.0,3.0,3.0/), (/n,m/))
    call self_ref_slice_expr(u, v, w, 2, 2, 1, 1, 2, 2)
    u_eps = reshape((/1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0/), (/n,m/)) + eps
    v_eps = reshape((/9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0/), (/n,m/)) + eps
    w_eps = reshape((/1.0,1.0,1.0,2.0,2.0,2.0,3.0,3.0,3.0/), (/n,m/)) + eps
    call self_ref_slice_expr(u_eps, v_eps, w_eps, 2, 2, 1, 1, 2, 2)
    fd_u(:,:) = (u_eps(:,:) - u(:,:)) / eps
    fd_w(:,:) = (w_eps(:,:) - w(:,:)) / eps
    u = reshape((/1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0/), (/n,m/))
    v = reshape((/9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0/), (/n,m/))
    w = reshape((/1.0,1.0,1.0,2.0,2.0,2.0,3.0,3.0,3.0/), (/n,m/))
    u_ad(:,:) = 1.0
    v_ad(:,:) = 1.0
    w_ad(:,:) = 1.0
    call self_ref_slice_expr_fwd_ad(u, u_ad, v, v_ad, w, w_ad, 2, 2, 1, 1, 2, 2)
    if (any(abs((u_ad(:,:) - fd_u(:,:)) / fd_u(:,:)) > tol_expr) .or. &
        any(abs((w_ad(:,:) - fd_w(:,:)) / fd_w(:,:)) > tol_expr)) then
       print *, 'test_slice_expr_fwd failed'
       error stop 1
    end if

    inner1 = sum(u_ad(:,:)**2) + sum(w_ad(:,:)**2)
    v_ad(:,:) = 0.0
    call self_ref_slice_expr_rev_ad(u, u_ad, v, v_ad, w, w_ad, 2, 2, 1, 1, 2, 2)
    inner2 = sum(u_ad(:,:)) + sum(v_ad(:,:)) + sum(w_ad(:,:))
    if (abs((inner2 - inner1) / inner1) > tol_expr) then
       print *, 'test_slice_expr_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_slice_expr

end program run_self_reference
