module exit_cycle_ad
  use exit_cycle
  use fautodiff_data_storage
  implicit none

contains

  subroutine loop_exit_cycle_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    integer :: i

    res_ad = x_ad * 2.0 * x
    res = x**2
    do i = 1, n
      res_ad = res_ad * x + x_ad * res
      res = res * x
      if (i == 2) cycle
      res_ad = res_ad * x + x_ad * res
      res = res * x
      if (i == 4) exit
      res_ad = res_ad * x + x_ad * res
      res = res * x
      if (res > 2.0) then
        res_ad = res_ad * x + x_ad * res
        res = res * x
        cycle
      end if
      res_ad = res_ad * x + x_ad * res
      res = res * x
      if (res > 4.0) then
        res_ad = res_ad * x + x_ad * res
        res = res * x
        exit
      end if
      res_ad = res_ad * x + x_ad * res
      res = res * x
    end do
    res_ad = res_ad * x + x_ad * res
    res = res * x

    return
  end subroutine loop_exit_cycle_fwd_ad

  subroutine loop_exit_cycle_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    integer :: i
    real :: res
    integer :: exit_index_1_ad
    logical :: exit_flag_2_ad
    logical :: exit_flag_3_ad
    logical :: cycle_flag_4_ad
    logical :: cycle_flag_5_ad
    real :: res_save_3_ad
    real :: res_save_4_ad
    real :: res_save_5_ad
    real :: res_save_6_ad
    real :: res_save_7_ad
    real :: res_save_8_ad
    real :: res_save_9_ad
    real :: res_save_10_ad
    real :: res_save_11_ad

    res = x**2
    exit_index_1_ad = n
    do i = 1, n
      call fautodiff_data_storage_push(res)
      res = res * x
      if (i == 2) then
        cycle
      end if
      res = res * x
      if (i == 4) then
        exit_index_1_ad = i
        exit
      end if
      res = res * x
      if (res > 2.0) then
        res = res * x
        cycle
      end if
      res = res * x
      if (res > 4.0) then
        res = res * x
        exit_index_1_ad = i
        exit
      end if
      res = res * x
    end do
    res = res * x

    x_ad = res_ad * res ! res = res * x
    res_ad = res_ad * x ! res = res * x
    exit_flag_2_ad = .true.
    exit_flag_3_ad = .true.
    do i = exit_index_1_ad, 1, - 1
      cycle_flag_5_ad = .true.
      cycle_flag_4_ad = .true.
      call fautodiff_data_storage_pop(res)
      if (exit_flag_3_ad .and. exit_flag_2_ad) then
        res_save_9_ad = res
        res = res * x
      end if
      if (exit_flag_3_ad .and. exit_flag_2_ad) then
        if (i == 2) then
          cycle_flag_4_ad = .false.
        end if
      end if
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad) then
        res_save_8_ad = res
        res = res * x
      end if
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        if (i == 4) then
          exit_flag_2_ad = .false.
        end if
      end if
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        res_save_7_ad = res
        res = res * x
      end if
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        res_save_11_ad = res
        if (res > 2.0) then
          res = res * x
          cycle_flag_5_ad = .false.
        end if
      end if
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        res_save_5_ad = res
        res = res * x
      end if
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        res_save_10_ad = res
        if (res > 4.0) then
          res = res * x
          exit_flag_3_ad = .false.
        end if
      end if
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        res_save_3_ad = res
      end if

      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        res = res_save_3_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      res = res_save_10_ad
      if (res > 4.0) then
        if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
          res_save_4_ad = res
        end if
        exit_flag_3_ad = .true. ! exit
        if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
          res = res_save_4_ad
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      res = res_save_10_ad
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        res = res_save_5_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      res = res_save_11_ad
      if (res > 2.0) then
        if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
          res_save_6_ad = res
        end if
        cycle_flag_5_ad = .true. ! cycle
        if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
          res = res_save_6_ad
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      res = res_save_11_ad
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        res = res_save_7_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (i == 4) then
        exit_flag_2_ad = .true. ! exit
      end if
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        res = res_save_8_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (i == 2) then
        cycle_flag_4_ad = .true.
      end if
      if (exit_flag_3_ad .and. exit_flag_2_ad .and. cycle_flag_4_ad .and. cycle_flag_5_ad) then
        res = res_save_9_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
    end do
    x_ad = res_ad * 2.0 * x + x_ad ! res = x**2
    res_ad = 0.0 ! res = x**2

    return
  end subroutine loop_exit_cycle_rev_ad

end module exit_cycle_ad
