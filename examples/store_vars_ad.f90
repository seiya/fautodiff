module store_vars_ad
  use store_vars
  use fautodiff_data_storage
  implicit none

contains

  subroutine do_with_recurrent_scalar_fwd_ad(n, x, x_ad, z_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(in)  :: x_ad(n)
    real, intent(out) :: z_ad(n)
    real :: work_ad
    real :: work
    integer :: i

    work_ad = 0.0 ! work = 1.0
    work = 1.0
    do i = 1, n
      work_ad = x_ad(i) * work + work_ad * x(i) ! work = x(i) * work
      work = x(i) * work
      z_ad(i) = work_ad ! z(i) = work
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
    real :: work_save_21_ad

    work = 1.0
    do i = 1, n
      call fautodiff_data_storage_push(work)
      work = x(i) * work
    end do

    work_ad = 0.0

    do i = n, 1, - 1
      call fautodiff_data_storage_pop(work)
      work_save_21_ad = work
      work_ad = z_ad(i) + work_ad ! z(i) = work
      z_ad(i) = 0.0 ! z(i) = work
      work = work_save_21_ad
      x_ad(i) = work_ad * work ! work = x(i) * work
      work_ad = work_ad * x(i) ! work = x(i) * work
    end do

    return
  end subroutine do_with_recurrent_scalar_rev_ad

  subroutine do_while_fwd_ad(x, x_ad, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y_ad
    real, intent(out) :: z_ad
    real :: a_ad
    real :: y
    real :: z
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
    real :: y_save_45_ad

    y = 0.0
    z = 1.0
    a = y * x
    call fautodiff_data_storage_push(.false.)
    y_save_45_ad = y
    do while (y < 10.0)
      call fautodiff_data_storage_push(.true.)
      call fautodiff_data_storage_push(z)
      call fautodiff_data_storage_push(a)
      a = a + x
      y = y + a
      a = a + 1.0
      z = z * a
    end do

    a_ad = 0.0
    x_ad = 0.0

    z_ad = y_ad * y + z_ad ! y = z * y
    y_ad = y_ad * z ! y = z * y
    do while (fautodiff_data_storage_get())
      call fautodiff_data_storage_pop(a)
      call fautodiff_data_storage_pop(z)
      a = a + x
      a = a + 1.0
      a_ad = z_ad * z + a_ad ! z = z * a
      z_ad = z_ad * a ! z = z * a
      a_ad = y_ad + a_ad ! y = y + a
      x_ad = a_ad + x_ad ! a = a + x
    end do
    y = y_save_45_ad
    x_ad = a_ad * y + x_ad ! a = y * x
    z_ad = 0.0 ! z = 1.0
    y_ad = 0.0 ! y = 0.0

    return
  end subroutine do_while_rev_ad

end module store_vars_ad
