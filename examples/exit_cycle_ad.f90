module exit_cycle_ad
  use exit_cycle
  use fautodiff_data_storage
  implicit none

contains

  subroutine do_exit_cycle_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    integer :: i

    res_ad = x_ad * 2.0 * x ! res = x**2
    res = x**2
    do i = 1, n
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
      if (i == 2) then
        cycle
      end if
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
      if (i == 4) then
        exit
      end if
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
      if (res > 4.0) then
        res_ad = res_ad * x + x_ad * res ! res = res * x
        res = res * x
        cycle
      end if
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
      if (res > 2.0) then
        res_ad = res_ad * x + x_ad * res ! res = res * x
        res = res * x
        exit
      end if
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
    end do
    res_ad = res_ad * x + x_ad * res ! res = res * x
    res = res * x

    return
  end subroutine do_exit_cycle_fwd_ad

  subroutine do_exit_cycle_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    real :: res
    integer :: exit_do_start_35_ad
    integer :: i
    logical :: exit_flag_22_ad
    logical :: exit_flag_32_ad
    logical :: cycle_flag_18_ad
    logical :: cycle_flag_27_ad
    real :: res_save_16_ad
    real :: res_save_20_ad
    real :: res_save_24_ad
    real :: res_save_28_ad
    real :: res_save_29_ad
    real :: res_save_33_ad

    res = x**2
    exit_do_start_35_ad = n
    do i = 1, n
      call fautodiff_data_storage_push(res)
      res = res * x
      if (i == 2) then
        cycle
      end if
      res = res * x
      if (i == 4) then
        exit_do_start_35_ad = i
        exit
      end if
      res = res * x
      if (res > 4.0) then
        res = res * x
        cycle
      end if
      res = res * x
      if (res > 2.0) then
        res = res * x
        exit_do_start_35_ad = i
        exit
      end if
      res = res * x
    end do

    x_ad = res_ad * res ! res = res * x
    res_ad = res_ad * x ! res = res * x
    exit_flag_22_ad = .true.
    exit_flag_32_ad = .true.
    do i = exit_do_start_35_ad, 1, - 1
      cycle_flag_18_ad = .true.
      cycle_flag_27_ad = .true.
      call fautodiff_data_storage_pop(res)
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        res_save_16_ad = res
        res = res * x
        if (i == 2) then
          cycle_flag_18_ad = .false.
        end if
      end if
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        res_save_20_ad = res
        res = res * x
        if (i == 4) then
          exit_flag_22_ad = .false.
        end if
      end if
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        res_save_24_ad = res
        res = res * x
        res_save_28_ad = res
        if (res > 4.0) then
          res = res * x
          cycle_flag_27_ad = .false.
        end if
      end if
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        res_save_29_ad = res
        res = res * x
        res_save_33_ad = res
        if (res > 2.0) then
          res = res * x
          exit_flag_32_ad = .false.
        end if
      end if
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. cycle_flag_27_ad) then
        res = res_save_33_ad
        if (res > 2.0) then
          exit_flag_32_ad = .true. ! exit
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        res = res_save_29_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. exit_flag_32_ad) then
        res = res_save_28_ad
        if (res > 4.0) then
          cycle_flag_27_ad = .true. ! cycle
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        res = res_save_24_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_18_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        if (i == 4) then
          exit_flag_22_ad = .true. ! exit
        end if
      end if
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        res = res_save_20_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (exit_flag_22_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        if (i == 2) then
          cycle_flag_18_ad = .true. ! cycle
        end if
      end if
      if (cycle_flag_18_ad .and. exit_flag_22_ad .and. cycle_flag_27_ad .and. exit_flag_32_ad) then
        res = res_save_16_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
    end do
    x_ad = res_ad * 2.0 * x + x_ad ! res = x**2
    res_ad = 0.0 ! res = x**2

    return
  end subroutine do_exit_cycle_rev_ad

  subroutine while_exit_cycle_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    integer :: i

    res_ad = x_ad * 2.0 * x ! res = x**2
    res = x**2
    i = 1
    do while (i <= n)
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
      if (i == 2) then
        i = i + 1
        cycle
      end if
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
      if (i == 4) then
        exit
      end if
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
      if (res > 4.0) then
        res_ad = res_ad * x + x_ad * res ! res = res * x
        res = res * x
        i = i + 1
        cycle
      end if
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
      if (res > 2.0) then
        res_ad = res_ad * x + x_ad * res ! res = res * x
        res = res * x
        exit
      end if
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
      i = i + 1
    end do
    res_ad = res_ad * x + x_ad * res ! res = res * x
    res = res * x

    return
  end subroutine while_exit_cycle_fwd_ad

  subroutine while_exit_cycle_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    real :: res
    integer :: i
    logical :: exit_flag_56_ad
    logical :: exit_flag_67_ad
    logical :: cycle_flag_52_ad
    logical :: cycle_flag_62_ad
    real :: res_save_49_ad
    integer :: i_save_53_ad
    real :: res_save_54_ad
    real :: res_save_58_ad
    real :: res_save_63_ad
    integer :: i_save_63_ad
    real :: res_save_64_ad
    real :: res_save_68_ad
    real :: res_save_69_ad
    integer :: i_save_70_ad

    res = x**2
    i = 1
    call fautodiff_data_storage_push(.false.)
    do while (i <= n)
      call fautodiff_data_storage_push(.true.)
      call fautodiff_data_storage_push(i)
      call fautodiff_data_storage_push(res)
      res = res * x
      if (i == 2) then
        i = i + 1
        cycle
      end if
      res = res * x
      if (i == 4) then
        exit
      end if
      res = res * x
      if (res > 4.0) then
        res = res * x
        i = i + 1
        cycle
      end if
      res = res * x
      if (res > 2.0) then
        res = res * x
        exit
      end if
      res = res * x
      i = i + 1
    end do

    x_ad = res_ad * res ! res = res * x
    res_ad = res_ad * x ! res = res * x
    exit_flag_56_ad = .true.
    exit_flag_67_ad = .true.
    do while (fautodiff_data_storage_get())
      cycle_flag_52_ad = .true.
      cycle_flag_62_ad = .true.
      call fautodiff_data_storage_pop(res)
      call fautodiff_data_storage_pop(i)
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        res_save_49_ad = res
        res = res * x
        i_save_53_ad = i
        if (i == 2) then
          i = i + 1
          cycle_flag_52_ad = .false.
        end if
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        res_save_54_ad = res
        res = res * x
        if (i == 4) then
          exit_flag_56_ad = .false.
        end if
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        res_save_58_ad = res
        res = res * x
        i_save_63_ad = i
        res_save_63_ad = res
        if (res > 4.0) then
          res = res * x
          i = i + 1
          cycle_flag_62_ad = .false.
        end if
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        res_save_64_ad = res
        res = res * x
        res_save_68_ad = res
        if (res > 2.0) then
          res = res * x
          exit_flag_67_ad = .false.
        end if
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        res_save_69_ad = res
        i_save_70_ad = i
        i = i + 1
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        i = i_save_70_ad
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        res = res_save_69_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad) then
        res = res_save_68_ad
        if (res > 2.0) then
          exit_flag_67_ad = .true. ! exit
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        res = res_save_64_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. exit_flag_67_ad) then
        res = res_save_63_ad
        if (res > 4.0) then
          cycle_flag_62_ad = .true. ! cycle
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
        i = i_save_63_ad
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        res = res_save_58_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_52_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        if (i == 4) then
          exit_flag_56_ad = .true. ! exit
        end if
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        res = res_save_54_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        i = i_save_53_ad
        if (i == 2) then
          cycle_flag_52_ad = .true. ! cycle
        end if
      end if
      if (cycle_flag_52_ad .and. exit_flag_56_ad .and. cycle_flag_62_ad .and. exit_flag_67_ad) then
        res = res_save_49_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
    end do
    x_ad = res_ad * 2.0 * x + x_ad ! res = x**2
    res_ad = 0.0 ! res = x**2

    return
  end subroutine while_exit_cycle_rev_ad

end module exit_cycle_ad
