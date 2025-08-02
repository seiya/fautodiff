module exit_cycle_ad
  use exit_cycle
  use fautodiff_stack
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
    integer :: exit_do_start_32_ad
    integer :: i
    logical :: exit_flag_19_ad
    logical :: exit_flag_29_ad
    logical :: cycle_flag_15_ad
    logical :: cycle_flag_24_ad
    real :: res_save_13_ad
    real :: res_save_17_ad
    real :: res_save_21_ad
    real :: res_save_25_ad
    real :: res_save_26_ad
    real :: res_save_30_ad

    res = x**2
    exit_do_start_32_ad = n
    do i = 1, n
      call fautodiff_stack_push_r(res)
      res = res * x
      if (i == 2) then
        cycle
      end if
      res = res * x
      if (i == 4) then
        exit_do_start_32_ad = i
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
        exit_do_start_32_ad = i
        exit
      end if
      res = res * x
    end do

    x_ad = res_ad * res ! res = res * x
    res_ad = res_ad * x ! res = res * x
    exit_flag_19_ad = .true.
    exit_flag_29_ad = .true.
    label_32_0_ad: do i = exit_do_start_32_ad, 1, - 1
      cycle_flag_15_ad = .true.
      cycle_flag_24_ad = .true.
      call fautodiff_stack_pop_r(res)
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        res_save_13_ad = res
        res = res * x
        if (i == 2) then
          cycle_flag_15_ad = .false.
        end if
      end if
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        res_save_17_ad = res
        res = res * x
        if (i == 4) then
          exit_flag_19_ad = .false.
        end if
      end if
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        res_save_21_ad = res
        res = res * x
        res_save_25_ad = res
        if (res > 4.0) then
          res = res * x
          cycle_flag_24_ad = .false.
        end if
      end if
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        res_save_26_ad = res
        res = res * x
        res_save_30_ad = res
        if (res > 2.0) then
          res = res * x
          exit_flag_29_ad = .false.
        end if
      end if
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. cycle_flag_24_ad) then
        res = res_save_30_ad
        if (res > 2.0) then
          exit_flag_29_ad = .true. ! exit
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        res = res_save_26_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. exit_flag_29_ad) then
        res = res_save_25_ad
        if (res > 4.0) then
          cycle_flag_24_ad = .true. ! cycle
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        res = res_save_21_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_15_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        if (i == 4) then
          exit_flag_19_ad = .true. ! exit
        end if
      end if
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        res = res_save_17_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (exit_flag_19_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        if (i == 2) then
          cycle_flag_15_ad = .true. ! cycle
        end if
      end if
      if (cycle_flag_15_ad .and. exit_flag_19_ad .and. cycle_flag_24_ad .and. exit_flag_29_ad) then
        res = res_save_13_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
    end do label_32_0_ad
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
    logical :: exit_flag_54_ad
    logical :: exit_flag_65_ad
    logical :: cycle_flag_50_ad
    logical :: cycle_flag_60_ad
    real :: res_save_47_ad
    integer :: i_save_51_ad
    real :: res_save_52_ad
    real :: res_save_56_ad
    real :: res_save_61_ad
    integer :: i_save_61_ad
    real :: res_save_62_ad
    real :: res_save_66_ad

    res = x**2
    i = 1
    call fautodiff_stack_l%push(.false.)
    do while (i <= n)
      call fautodiff_stack_l%push(.true.)
      call fautodiff_stack_i%push(i)
      call fautodiff_stack_push_r(res)
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
    exit_flag_54_ad = .true.
    exit_flag_65_ad = .true.
    label_69_0_ad: do while (fautodiff_stack_l%get())
      cycle_flag_50_ad = .true.
      cycle_flag_60_ad = .true.
      call fautodiff_stack_pop_r(res)
      call fautodiff_stack_i%pop(i)
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        res_save_47_ad = res
        res = res * x
        i_save_51_ad = i
        if (i == 2) then
          i = i + 1
          cycle_flag_50_ad = .false.
        end if
      end if
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        res_save_52_ad = res
        res = res * x
        if (i == 4) then
          exit_flag_54_ad = .false.
        end if
      end if
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        res_save_56_ad = res
        res = res * x
        i_save_61_ad = i
        res_save_61_ad = res
        if (res > 4.0) then
          res = res * x
          i = i + 1
          cycle_flag_60_ad = .false.
        end if
      end if
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        res_save_62_ad = res
        res = res * x
        res_save_66_ad = res
        if (res > 2.0) then
          res = res * x
          exit_flag_65_ad = .false.
        end if
      end if
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. cycle_flag_60_ad) then
        res = res_save_66_ad
        if (res > 2.0) then
          exit_flag_65_ad = .true. ! exit
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        res = res_save_62_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. exit_flag_65_ad) then
        res = res_save_61_ad
        if (res > 4.0) then
          cycle_flag_60_ad = .true. ! cycle
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
        i = i_save_61_ad
      end if
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        res = res_save_56_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_50_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        if (i == 4) then
          exit_flag_54_ad = .true. ! exit
        end if
      end if
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        res = res_save_52_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (exit_flag_54_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        i = i_save_51_ad
        if (i == 2) then
          cycle_flag_50_ad = .true. ! cycle
        end if
      end if
      if (cycle_flag_50_ad .and. exit_flag_54_ad .and. cycle_flag_60_ad .and. exit_flag_65_ad) then
        res = res_save_47_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
    end do label_69_0_ad
    x_ad = res_ad * 2.0 * x + x_ad ! res = x**2
    res_ad = 0.0 ! res = x**2

    return
  end subroutine while_exit_cycle_rev_ad

  subroutine exit_cycle_with_labels_fwd_ad(n, x, x_ad, res, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: res
    real, intent(out) :: res_ad
    integer :: i
    integer :: k
    integer :: j

    res_ad = x_ad ! res = x
    res = x
    i = 0
    outer: do while (i <= n)
      i = i + 1
      res = res + 1.0
      middle: do j = 1, n
        res = res + 10.0
        if (res > 5000.0) then
          exit outer
        end if
        inner: do k = 1, n
          if (res > 4000.0) then
            exit outer
          end if
          res = res + 100.0
          if (res > 2400.0) then
            exit middle
          end if
          res = res + 100.0
          if (res > 2200.0) then
            cycle middle
          end if
          res = res + 100.0
          if (res > 1000.0) then
            cycle outer
          end if
          res_ad = res_ad * x + x_ad * res ! res = res * x
          res = res * x
        end do inner
      end do middle
      res_ad = res_ad * x + x_ad * res ! res = res * x
      res = res * x
    end do outer

    return
  end subroutine exit_cycle_with_labels_fwd_ad

  subroutine exit_cycle_with_labels_rev_ad(n, x, x_ad, res_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(inout) :: res_ad
    real :: res
    integer :: i
    integer :: k
    integer :: j
    logical :: exit_flag_90_ad
    logical :: exit_flag_94_ad
    logical :: cycle_flag_106_ad
    integer :: exit_do_start_110_ad
    logical :: exit_flag_98_ad
    logical :: cycle_flag_102_ad
    integer :: exit_do_start_109_ad
    real :: res_save_109_ad
    real :: res_save_96_ad
    real :: res_save_100_ad
    real :: res_save_104_ad

    res = x
    i = 0
    call fautodiff_stack_l%push(.false.)
    outer: do while (i <= n)
      call fautodiff_stack_l%push(.true.)
      call fautodiff_stack_push_r(res)
      i = i + 1
      res = res + 1.0
      middle: do j = 1, n
        res = res + 10.0
        if (res > 5000.0) then
          exit outer
        end if
        inner: do k = 1, n
          if (res > 4000.0) then
            exit outer
          end if
          res = res + 100.0
          if (res > 2400.0) then
            exit middle
          end if
          res = res + 100.0
          if (res > 2200.0) then
            cycle middle
          end if
          res = res + 100.0
          if (res > 1000.0) then
            cycle outer
          end if
          res = res * x
        end do inner
      end do middle
      res = res * x
    end do outer

    x_ad = 0.0

    exit_flag_90_ad = .true.
    exit_flag_94_ad = .true.
    label_112_0_ad: do while (fautodiff_stack_l%get())
      cycle_flag_106_ad = .true.
      call fautodiff_stack_pop_r(res)
      if (exit_flag_90_ad .and. exit_flag_94_ad .and. cycle_flag_106_ad) then
        res = res + 1.0
        exit_do_start_110_ad = n
        label_110_1_ad: do j = 1, n
          call fautodiff_stack_push_r(res)
          res = res + 10.0
          if (res > 5000.0) then
            exit_do_start_110_ad = j
            exit_flag_90_ad = .false.
            exit label_110_1_ad
          end if
          label_109_2_ad: do k = 1, n
            if (res > 4000.0) then
              exit_do_start_110_ad = j
              exit_flag_94_ad = .false.
              exit label_110_1_ad
            end if
            res = res + 100.0
            if (res > 2400.0) then
              exit_do_start_110_ad = j
              exit label_110_1_ad
            end if
            res = res + 100.0
            if (res > 2200.0) then
              cycle label_110_1_ad
            end if
            res = res + 100.0
            if (res > 1000.0) then
              cycle_flag_106_ad = .false.
              cycle label_110_1_ad
            end if
            res = res * x
          end do label_109_2_ad
        end do label_110_1_ad
      end if
      if (exit_flag_90_ad .and. exit_flag_94_ad .and. cycle_flag_106_ad) then
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      exit_flag_90_ad = .true.
      exit_flag_94_ad = .true.
      exit_flag_98_ad = .true.
      label_110_0_ad: do j = exit_do_start_110_ad, 1, - 1
        cycle_flag_102_ad = .true.
        cycle_flag_106_ad = .true.
        call fautodiff_stack_pop_r(res)
        if (exit_flag_90_ad .and. exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
          res = res + 10.0
          if (res > 5000.0) then
            exit_flag_90_ad = .false.
          end if
        end if
        if (exit_flag_90_ad .and. exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
          res_save_109_ad = res
          exit_do_start_109_ad = n
          label_109_1_ad: do k = 1, n
            call fautodiff_stack_push_r(res)
            if (res > 4000.0) then
              exit_do_start_109_ad = k
              exit_flag_94_ad = .false.
              exit label_109_1_ad
            end if
            res = res + 100.0
            if (res > 2400.0) then
              exit_do_start_109_ad = k
              exit_flag_98_ad = .false.
              exit label_109_1_ad
            end if
            res = res + 100.0
            if (res > 2200.0) then
              cycle_flag_102_ad = .false.
              cycle label_109_1_ad
            end if
            res = res + 100.0
            if (res > 1000.0) then
              cycle_flag_106_ad = .false.
              cycle label_109_1_ad
            end if
            res = res * x
          end do label_109_1_ad
        end if
        if (exit_flag_90_ad) then
          exit_flag_94_ad = .true.
          exit_flag_98_ad = .true.
          label_109_0_ad: do k = exit_do_start_109_ad, 1, - 1
            cycle_flag_102_ad = .true.
            cycle_flag_106_ad = .true.
            call fautodiff_stack_pop_r(res)
            if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
              if (res > 4000.0) then
                exit_flag_94_ad = .false.
              end if
            end if
            if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
              res_save_96_ad = res
              res = res + 100.0
              if (res > 2400.0) then
                exit_flag_98_ad = .false.
              end if
            end if
            if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
              res_save_100_ad = res
              res = res + 100.0
              if (res > 2200.0) then
                cycle_flag_102_ad = .false.
              end if
            end if
            if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
              res_save_104_ad = res
              res = res + 100.0
              if (res > 1000.0) then
                cycle_flag_106_ad = .false.
              end if
            end if
            if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
              x_ad = res_ad * res + x_ad ! res = res * x
              res_ad = res_ad * x ! res = res * x
            end if
            if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad) then
              if (res > 1000.0) then
                cycle_flag_106_ad = .true. ! cycle outer
              end if
            end if
            if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
              res = res_save_104_ad
            end if
            if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_106_ad) then
              if (res > 2200.0) then
                cycle_flag_102_ad = .true. ! cycle middle
              end if
            end if
            if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
              res = res_save_100_ad
            end if
            if (exit_flag_94_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
              if (res > 2400.0) then
                exit_flag_98_ad = .true. ! exit middle
              end if
            end if
            if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
              res = res_save_96_ad
            end if
            if (exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
              if (res > 4000.0) then
                exit_flag_94_ad = .true. ! exit outer
              end if
            end if
          end do label_109_0_ad
          res = res_save_109_ad
        end if
        if (exit_flag_94_ad .and. exit_flag_98_ad .and. cycle_flag_102_ad .and. cycle_flag_106_ad) then
          if (res > 5000.0) then
            exit_flag_90_ad = .true. ! exit outer
          end if
        end if
      end do label_110_0_ad
    end do label_112_0_ad
    x_ad = res_ad + x_ad ! res = x
    res_ad = 0.0 ! res = x

    return
  end subroutine exit_cycle_with_labels_rev_ad

end module exit_cycle_ad
