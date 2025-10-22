program run_generic_interface
  use generic_interface_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_call_add_real_8 = 1
  integer, parameter :: I_call_add_real_selected_real_kind = 2
  integer, parameter :: I_call_add_real_kind = 3
  integer, parameter :: I_call_add_real_real64 = 4

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
           case ("call_add_real_8")
              i_test = I_call_add_real_8
           case ("call_add_real_selected_real_kind")
              i_test = I_call_add_real_selected_real_kind
           case ("call_add_real_kind")
              i_test = I_call_add_real_kind
           case ("call_add_real_real64")
              i_test = I_call_add_real_real64
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_call_add_real_8 .or. i_test == I_all) then
     call test_call_add_real_8
  end if
  if (i_test == I_call_add_real_selected_real_kind .or. i_test == I_all) then
     call test_call_add_real_selected_real_kind
  end if
  if (i_test == I_call_add_real_kind .or. i_test == I_all) then
     call test_call_add_real_kind
  end if
  if (i_test == I_call_add_real_real64 .or. i_test == I_all) then
     call test_call_add_real_real64
  end if

  stop

contains

  subroutine test_call_add_real_8
    real(8) :: x, y, z
    real(8) :: x_ad, y_ad, z_ad
    real(8) :: z_eps, fd, eps
    real(8) :: inner1, inner2

    eps = 1.0d-6
    x = 2.0d0
    y = 3.0d0
    call call_add_real_8(x, y, z)
    call call_add_real_8(x + eps, y + eps, z_eps)
    fd = (z_eps - z) / eps
    x_ad = 1.0d0
    y_ad = 1.0d0
    call call_add_real_8_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_call_add_real_8_fwd failed', z_ad, fd
       error stop 1
    end if

    inner1 = z_ad**2
    x_ad = 0.0d0
    y_ad = 0.0d0
    call call_add_real_8_rev_ad(x_ad, y_ad, z_ad)
    inner2 = x_ad + y_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_call_add_real_8_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_call_add_real_8

  subroutine test_call_add_real_selected_real_kind
    integer, parameter :: RP = selected_real_kind(15, 307)
    real(kind=RP) :: x, y, z
    real(kind=RP) :: x_ad, y_ad, z_ad
    real(kind=RP) :: z_eps, fd, eps
    real(kind=RP) :: inner1, inner2

    eps = 1.0d-6
    x = 2.0d0
    y = 3.0d0
    call call_add_real_selected_real_kind(x, y, z)
    call call_add_real_selected_real_kind(x + eps, y + eps, z_eps)
    fd = (z_eps - z) / eps
    x_ad = 1.0d0
    y_ad = 1.0d0
    call call_add_real_selected_real_kind_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_call_add_real_selected_real_kind_fwd failed', z_ad, fd
       error stop 1
    end if

    inner1 = z_ad**2
    x_ad = 0.0d0
    y_ad = 0.0d0
    call call_add_real_selected_real_kind_rev_ad(x_ad, y_ad, z_ad)
    inner2 = x_ad + y_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_call_add_real_selected_real_kind_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_call_add_real_selected_real_kind

  subroutine test_call_add_real_kind
    integer, parameter :: RP = kind(1.0d0)
    real(kind=RP) :: x, y, z
    real(kind=RP) :: x_ad, y_ad, z_ad
    real(kind=RP) :: z_eps, fd, eps
    real(kind=RP) :: inner1, inner2

    eps = 1.0d-6
    x = 2.0d0
    y = 3.0d0
    call call_add_real_kind(x, y, z)
    call call_add_real_kind(x + eps, y + eps, z_eps)
    fd = (z_eps - z) / eps
    x_ad = 1.0d0
    y_ad = 1.0d0
    call call_add_real_kind_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_call_add_real_kind_fwd failed', z_ad, fd
       error stop 1
    end if

    inner1 = z_ad**2
    x_ad = 0.0d0
    y_ad = 0.0d0
    call call_add_real_kind_rev_ad(x_ad, y_ad, z_ad)
    inner2 = x_ad + y_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_call_add_real_kind_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_call_add_real_kind

  subroutine test_call_add_real_real64
    real(kind=real64) :: x, y, z
    real(kind=real64) :: x_ad, y_ad, z_ad
    real(kind=real64) :: z_eps, fd, eps
    real(kind=real64) :: inner1, inner2

    eps = 1.0d-6
    x = 2.0d0
    y = 3.0d0
    call call_add_real_real64(x, y, z)
    call call_add_real_real64(x + eps, y + eps, z_eps)
    fd = (z_eps - z) / eps
    x_ad = 1.0d0
    y_ad = 1.0d0
    call call_add_real_real64_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    if (abs((z_ad - fd) / fd) > tol) then
       print *, 'test_call_add_real_real64_fwd failed', z_ad, fd
       error stop 1
    end if

    inner1 = z_ad**2
    x_ad = 0.0d0
    y_ad = 0.0d0
    call call_add_real_real64_rev_ad(x_ad, y_ad, z_ad)
    inner2 = x_ad + y_ad
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_call_add_real_real64_rev failed', inner1, inner2
       error stop 1
    end if

    return
  end subroutine test_call_add_real_real64

end program run_generic_interface
