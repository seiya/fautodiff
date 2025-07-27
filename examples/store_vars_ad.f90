module store_vars_ad
  use store_vars
  use fautodiff_stack
  implicit none

contains

  subroutine do_with_recurrent_scalar_fwd_ad(n, x, x_ad, z, z_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(in)  :: x_ad(n)
    real, intent(out) :: z(n)
    real, intent(out) :: z_ad(n)
    real :: work_ad
    real :: work
    integer :: i

    work = 1.0
    work_ad = x_ad(1) * work ! work = x(1) * work
    work = x(1) * work
    z_ad(:) = x_ad(:) * work + work_ad * x(:) ! z(:) = x(:) * work
    z(:) = x(:) * work
    do i = 1, n
      work_ad = x_ad(i) * work + work_ad * x(i) ! work = x(i) * work
      work = x(i) * work
      z_ad(i) = work_ad * 2.0 * work + z_ad(i) ! z(i) = work**2 + z(i)
      z(i) = work**2 + z(i)
    end do

    return
  end subroutine do_with_recurrent_scalar_fwd_ad

  subroutine do_with_recurrent_scalar_rev_ad(n, x, x_ad, z_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(out) :: x_ad(n)
    real, intent(inout) :: z_ad(n)
    real :: work_ad
    real :: work
    integer :: i
    real :: work_save_16_ad
    real :: work_save_21_ad
    real :: work_save_19_ad

    work = 1.0
    work_save_16_ad = work
    work = x(1) * work
    work_save_21_ad = work
    do i = 1, n
      call fautodiff_stack_r4%push(work)
      work = x(i) * work
    end do

    work_ad = 0.0

    do i = n, 1, - 1
      call fautodiff_stack_r4%pop(work)
      work_save_19_ad = work
      work = x(i) * work
      work_ad = z_ad(i) * 2.0 * work + work_ad ! z(i) = work**2 + z(i)
      work = work_save_19_ad
      x_ad(i) = work_ad * work ! work = x(i) * work
      work_ad = work_ad * x(i) ! work = x(i) * work
    end do
    work = work_save_21_ad
    x_ad(:) = z_ad(:) * work + x_ad(:) ! z(:) = x(:) * work
    work_ad = sum(z_ad(:) * x(:)) + work_ad ! z(:) = x(:) * work
    z_ad(:) = 0.0 ! z(:) = x(:) * work
    work = work_save_16_ad
    x_ad(1) = work_ad * work + x_ad(1) ! work = x(1) * work

    return
  end subroutine do_with_recurrent_scalar_rev_ad

  subroutine do_while_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad
    real, intent(out) :: z
    real, intent(out) :: z_ad
    real :: a_ad
    real :: a

    y_ad = 0.0 ! y = 0.0
    y = 0.0
    z_ad = 0.0 ! z = 1.0
    z = 1.0
    a_ad = x_ad * y ! a = y * x
    a = y * x
    do while (y < 10.0)
      a_ad = a_ad + x_ad ! a = a + x
      a = a + x
      y_ad = y_ad + a_ad ! y = y + a
      y = y + a
      a = a + 1.0
      z_ad = z_ad * a + a_ad * z ! z = z * a
      z = z * a
    end do
    y_ad = z_ad * y + y_ad * z ! y = z * y
    y = z * y

    return
  end subroutine do_while_fwd_ad

  subroutine do_while_rev_ad(x, x_ad, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: y_ad
    real, intent(inout) :: z_ad
    real :: a_ad
    real :: y
    real :: z
    real :: a
    real :: y_save_39_ad

    y = 0.0
    z = 1.0
    a = y * x
    call fautodiff_stack_l%push(.false.)
    y_save_39_ad = y
    do while (y < 10.0)
      call fautodiff_stack_l%push(.true.)
      call fautodiff_stack_r4%push(z)
      call fautodiff_stack_r4%push(a)
      a = a + x
      y = y + a
      a = a + 1.0
      z = z * a
    end do

    a_ad = 0.0
    x_ad = 0.0

    z_ad = y_ad * y + z_ad ! y = z * y
    y_ad = y_ad * z ! y = z * y
    do while (fautodiff_stack_l%get())
      call fautodiff_stack_r4%pop(a)
      call fautodiff_stack_r4%pop(z)
      a = a + x
      a = a + 1.0
      a_ad = z_ad * z + a_ad ! z = z * a
      z_ad = z_ad * a ! z = z * a
      a_ad = y_ad + a_ad ! y = y + a
      x_ad = a_ad + x_ad ! a = a + x
    end do
    y = y_save_39_ad
    x_ad = a_ad * y + x_ad ! a = y * x
    z_ad = 0.0 ! z = 1.0
    y_ad = 0.0 ! y = 0.0

    return
  end subroutine do_while_rev_ad

end module store_vars_ad
