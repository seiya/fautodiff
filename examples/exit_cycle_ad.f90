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
      call fautodiff_stack_r4%push(res)
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
    label_35_0_ad: do i = exit_do_start_35_ad, 1, - 1
      cycle_flag_18_ad = .true.
      cycle_flag_27_ad = .true.
      call fautodiff_stack_r4%pop(res)
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
    end do label_35_0_ad
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

    res = x**2
    i = 1
    call fautodiff_stack_l%push(.false.)
    do while (i <= n)
      call fautodiff_stack_l%push(.true.)
      call fautodiff_stack_i%push(i)
      call fautodiff_stack_r4%push(res)
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
    label_71_0_ad: do while (fautodiff_stack_l%get())
      cycle_flag_52_ad = .true.
      cycle_flag_62_ad = .true.
      call fautodiff_stack_r4%pop(res)
      call fautodiff_stack_i%pop(i)
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
    end do label_71_0_ad
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
    logical :: exit_flag_91_ad
    logical :: exit_flag_95_ad
    logical :: cycle_flag_107_ad
    integer :: exit_do_start_111_ad
    logical :: exit_flag_99_ad
    logical :: cycle_flag_103_ad
    integer :: exit_do_start_110_ad
    real :: res_save_110_ad
    real :: res_save_97_ad
    real :: res_save_101_ad
    real :: res_save_105_ad

    res = x
    i = 0
    call fautodiff_stack_l%push(.false.)
    outer: do while (i <= n)
      call fautodiff_stack_l%push(.true.)
      call fautodiff_stack_r4%push(res)
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

    exit_flag_91_ad = .true.
    exit_flag_95_ad = .true.
    label_113_0_ad: do while (fautodiff_stack_l%get())
      cycle_flag_107_ad = .true.
      call fautodiff_stack_r4%pop(res)
      if (exit_flag_91_ad .and. exit_flag_95_ad .and. cycle_flag_107_ad) then
        res = res + 1.0
        exit_do_start_111_ad = n
        label_111_1_ad: do j = 1, n
          call fautodiff_stack_r4%push(res)
          res = res + 10.0
          if (res > 5000.0) then
            exit_do_start_111_ad = j
            exit_flag_91_ad = .false.
            exit label_111_1_ad
          end if
          label_110_2_ad: do k = 1, n
            if (res > 4000.0) then
              exit_do_start_111_ad = j
              exit_flag_95_ad = .false.
              exit label_111_1_ad
            end if
            res = res + 100.0
            if (res > 2400.0) then
              exit_do_start_111_ad = j
              exit label_111_1_ad
            end if
            res = res + 100.0
            if (res > 2200.0) then
              cycle label_111_1_ad
            end if
            res = res + 100.0
            if (res > 1000.0) then
              cycle_flag_107_ad = .false.
              cycle label_111_1_ad
            end if
            res = res * x
          end do label_110_2_ad
        end do label_111_1_ad
      end if
      if (exit_flag_91_ad .and. exit_flag_95_ad .and. cycle_flag_107_ad) then
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      exit_flag_91_ad = .true.
      exit_flag_95_ad = .true.
      exit_flag_99_ad = .true.
      label_111_0_ad: do j = exit_do_start_111_ad, 1, - 1
        cycle_flag_103_ad = .true.
        cycle_flag_107_ad = .true.
        call fautodiff_stack_r4%pop(res)
        if (exit_flag_91_ad .and. exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
          res = res + 10.0
          if (res > 5000.0) then
            exit_flag_91_ad = .false.
          end if
        end if
        if (exit_flag_91_ad .and. exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
          res_save_110_ad = res
          exit_do_start_110_ad = n
          label_110_1_ad: do k = 1, n
            call fautodiff_stack_r4%push(res)
            if (res > 4000.0) then
              exit_do_start_110_ad = k
              exit_flag_95_ad = .false.
              exit label_110_1_ad
            end if
            res = res + 100.0
            if (res > 2400.0) then
              exit_do_start_110_ad = k
              exit_flag_99_ad = .false.
              exit label_110_1_ad
            end if
            res = res + 100.0
            if (res > 2200.0) then
              cycle_flag_103_ad = .false.
              cycle label_110_1_ad
            end if
            res = res + 100.0
            if (res > 1000.0) then
              cycle_flag_107_ad = .false.
              cycle label_110_1_ad
            end if
            res = res * x
          end do label_110_1_ad
        end if
        if (exit_flag_91_ad) then
          exit_flag_95_ad = .true.
          exit_flag_99_ad = .true.
          label_110_0_ad: do k = exit_do_start_110_ad, 1, - 1
            cycle_flag_103_ad = .true.
            cycle_flag_107_ad = .true.
            call fautodiff_stack_r4%pop(res)
            if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
              if (res > 4000.0) then
                exit_flag_95_ad = .false.
              end if
            end if
            if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
              res_save_97_ad = res
              res = res + 100.0
              if (res > 2400.0) then
                exit_flag_99_ad = .false.
              end if
            end if
            if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
              res_save_101_ad = res
              res = res + 100.0
              if (res > 2200.0) then
                cycle_flag_103_ad = .false.
              end if
            end if
            if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
              res_save_105_ad = res
              res = res + 100.0
              if (res > 1000.0) then
                cycle_flag_107_ad = .false.
              end if
            end if
            if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
              x_ad = res_ad * res + x_ad ! res = res * x
              res_ad = res_ad * x ! res = res * x
            end if
            if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad) then
              if (res > 1000.0) then
                cycle_flag_107_ad = .true. ! cycle outer
              end if
            end if
            if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
              res = res_save_105_ad
            end if
            if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_107_ad) then
              if (res > 2200.0) then
                cycle_flag_103_ad = .true. ! cycle middle
              end if
            end if
            if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
              res = res_save_101_ad
            end if
            if (exit_flag_95_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
              if (res > 2400.0) then
                exit_flag_99_ad = .true. ! exit middle
              end if
            end if
            if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
              res = res_save_97_ad
            end if
            if (exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
              if (res > 4000.0) then
                exit_flag_95_ad = .true. ! exit outer
              end if
            end if
          end do label_110_0_ad
          res = res_save_110_ad
        end if
        if (exit_flag_95_ad .and. exit_flag_99_ad .and. cycle_flag_103_ad .and. cycle_flag_107_ad) then
          if (res > 5000.0) then
            exit_flag_91_ad = .true. ! exit outer
          end if
        end if
      end do label_111_0_ad
    end do label_113_0_ad
    x_ad = res_ad + x_ad ! res = x
    res_ad = 0.0 ! res = x

    return
  end subroutine exit_cycle_with_labels_rev_ad

end module exit_cycle_ad
