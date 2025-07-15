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
  end subroutine loop_exit_cycle_fwd_ad

  subroutine loop_exit_cycle_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    real :: res
    integer :: exit_do_start_38_ad
    integer :: i
    logical :: exit_flag_25_ad
    logical :: exit_flag_35_ad
    logical :: cycle_flag_21_ad
    logical :: cycle_flag_30_ad
    real :: res_save_19_ad
    real :: res_save_23_ad
    real :: res_save_27_ad
    real :: res_save_31_ad
    real :: res_save_32_ad
    real :: res_save_36_ad

    res = x**2
    exit_do_start_38_ad = n
    do i = 1, n
      call fautodiff_data_storage_push(res)
      res = res * x
      if (i == 2) then
        cycle
      end if
      res = res * x
      if (i == 4) then
        exit_do_start_38_ad = i
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
        exit_do_start_38_ad = i
        exit
      end if
      res = res * x
    end do

    x_ad = res_ad * res ! res = res * x
    res_ad = res_ad * x ! res = res * x
    exit_flag_25_ad = .true.
    exit_flag_35_ad = .true.
    do i = exit_do_start_38_ad, 1, - 1
      cycle_flag_21_ad = .true.
      cycle_flag_30_ad = .true.
      call fautodiff_data_storage_pop(res)
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        res_save_19_ad = res
        res = res * x
        if (i == 2) then
          cycle_flag_21_ad = .false.
        end if
      end if
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        res_save_23_ad = res
        res = res * x
        if (i == 4) then
          exit_flag_25_ad = .false.
        end if
      end if
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        res_save_27_ad = res
        res = res * x
        res_save_31_ad = res
        if (res > 4.0) then
          res = res * x
          cycle_flag_30_ad = .false.
        end if
      end if
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        res_save_32_ad = res
        res = res * x
        res_save_36_ad = res
        if (res > 2.0) then
          res = res * x
          exit_flag_35_ad = .false.
        end if
      end if
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. cycle_flag_30_ad) then
        res = res_save_36_ad
        if (res > 2.0) then
          exit_flag_35_ad = .true. ! exit
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        res = res_save_32_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. exit_flag_35_ad) then
        res = res_save_31_ad
        if (res > 4.0) then
          cycle_flag_30_ad = .true. ! cycle
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        res = res_save_27_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_21_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        if (i == 4) then
          exit_flag_25_ad = .true. ! exit
        end if
      end if
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        res = res_save_23_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (exit_flag_25_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        if (i == 2) then
          cycle_flag_21_ad = .true. ! cycle
        end if
      end if
      if (cycle_flag_21_ad .and. exit_flag_25_ad .and. cycle_flag_30_ad .and. exit_flag_35_ad) then
        res = res_save_19_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
    end do
    x_ad = res_ad * 2.0 * x + x_ad ! res = x**2
    res_ad = 0.0 ! res = x**2

    return
  end subroutine loop_exit_cycle_rev_ad

  subroutine loop_exit_cycle_while_fwd_ad(n, x, x_ad, res, res_ad)
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
  end subroutine loop_exit_cycle_while_fwd_ad

  subroutine loop_exit_cycle_while_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    real :: res
    integer :: i
    logical :: exit_flag_63_ad
    logical :: exit_flag_74_ad
    logical :: cycle_flag_59_ad
    logical :: cycle_flag_69_ad
    real :: res_save_56_ad
    integer :: i_save_60_ad
    real :: res_save_61_ad
    real :: res_save_65_ad
    real :: res_save_70_ad
    integer :: i_save_70_ad
    real :: res_save_71_ad
    real :: res_save_75_ad
    real :: res_save_76_ad
    integer :: i_save_77_ad

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
    exit_flag_63_ad = .true.
    exit_flag_74_ad = .true.
    do while (fautodiff_data_storage_get())
      call fautodiff_data_storage_pop(res)
      call fautodiff_data_storage_pop(i)
      cycle_flag_59_ad = .true.
      cycle_flag_69_ad = .true.
      res_save_56_ad = res
      res = res * x
      i_save_60_ad = i
      if (i == 2) then
        i = i + 1
        cycle
      end if
      res_save_61_ad = res
      res = res * x
      if (i == 4) then
        exit
      end if
      res_save_65_ad = res
      res = res * x
      i_save_70_ad = i
      res_save_70_ad = res
      if (res > 4.0) then
        res = res * x
        i = i + 1
        cycle
      end if
      res_save_71_ad = res
      res = res * x
      res_save_75_ad = res
      if (res > 2.0) then
        res = res * x
        exit
      end if
      res_save_76_ad = res
      res = res * x
      i_save_77_ad = i
      i = i + 1
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        res_save_56_ad = res
        res = res * x
        i_save_60_ad = i
        if (i == 2) then
          i = i + 1
          cycle_flag_59_ad = .false.
        end if
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        res_save_61_ad = res
        res = res * x
        if (i == 4) then
          exit_flag_63_ad = .false.
        end if
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        res_save_65_ad = res
        res = res * x
        i_save_70_ad = i
        res_save_70_ad = res
        if (res > 4.0) then
          res = res * x
          i = i + 1
          cycle_flag_69_ad = .false.
        end if
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        res_save_71_ad = res
        res = res * x
        res_save_75_ad = res
        if (res > 2.0) then
          res = res * x
          exit_flag_74_ad = .false.
        end if
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        res_save_76_ad = res
        i_save_77_ad = i
        i = i + 1
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        i = i_save_77_ad
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        res = res_save_76_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad) then
        res = res_save_75_ad
        if (res > 2.0) then
          exit_flag_74_ad = .true. ! exit
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        res = res_save_71_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. exit_flag_74_ad) then
        res = res_save_70_ad
        if (res > 4.0) then
          cycle_flag_69_ad = .true. ! cycle
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
        i = i_save_70_ad
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        res = res_save_65_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_59_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        if (i == 4) then
          exit_flag_63_ad = .true. ! exit
        end if
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        res = res_save_61_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        i = i_save_60_ad
        if (i == 2) then
          cycle_flag_59_ad = .true. ! cycle
        end if
      end if
      if (cycle_flag_59_ad .and. exit_flag_63_ad .and. cycle_flag_69_ad .and. exit_flag_74_ad) then
        res = res_save_56_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
    end do
    x_ad = res_ad * 2.0 * x + x_ad ! res = x**2
    res_ad = 0.0 ! res = x**2

    return
  end subroutine loop_exit_cycle_while_rev_ad

end module exit_cycle_ad
