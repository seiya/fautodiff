module store_vars_ad
  use store_vars
  use fautodiff_data_storage
  implicit none

contains

  subroutine do_with_recurrent_scalar_rev_ad(n, x, x_ad, z_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x(n)
    real, intent(out) :: x_ad(n)
    real, intent(inout) :: z_ad(n)
    real :: work_ad
    real :: work
    integer :: i
    real :: work_save_15_ad

    work = 1.0
    do i = 1, n
      call fautodiff_data_storage_push(work)
      work = x(i) * work
    end do

    work_ad = 0.0

    do i = n, 1, - 1
      call fautodiff_data_storage_pop(work)
      work_save_15_ad = work
      work_ad = z_ad(i) + work_ad ! z(i) = work
      z_ad(i) = 0.0 ! z(i) = work
      work = work_save_15_ad
      x_ad(i) = work_ad * work ! work = x(i) * work
      work_ad = work_ad * x(i) ! work = x(i) * work
    end do

    return
  end subroutine do_with_recurrent_scalar_rev_ad

end module store_vars_ad
