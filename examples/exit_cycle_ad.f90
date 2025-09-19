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
    real, intent(inout) :: x_ad
    real, intent(inout) :: res_ad
    real :: res
    integer :: exit_do_start_31_ad
    integer :: i
    logical :: exit_flag_18_ad
    logical :: exit_flag_28_ad
    logical :: cycle_flag_14_ad
    logical :: cycle_flag_23_ad
    real :: res_save_12_ad
    real :: res_save_16_ad
    real :: res_save_20_ad
    real :: res_save_24_ad
    real :: res_save_25_ad
    real :: res_save_29_ad

    res = x**2
    exit_do_start_31_ad = n
    do i = 1, n
      call fautodiff_stack_push_r(res)
      res = res * x
      if (i == 2) then
        cycle
      end if
      res = res * x
      if (i == 4) then
        exit_do_start_31_ad = i
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
        exit_do_start_31_ad = i
        exit
      end if
      res = res * x
    end do

    x_ad = res_ad * res + x_ad ! res = res * x
    res_ad = res_ad * x ! res = res * x
    exit_flag_18_ad = .true.
    exit_flag_28_ad = .true.
    label_31_0_ad: do i = exit_do_start_31_ad, 1, - 1
      cycle_flag_14_ad = .true.
      cycle_flag_23_ad = .true.
      call fautodiff_stack_pop_r(res)
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        res_save_12_ad = res
        res = res * x
        if (i == 2) then
          cycle_flag_14_ad = .false.
        end if
      end if
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        res_save_16_ad = res
        res = res * x
        if (i == 4) then
          exit_flag_18_ad = .false.
        end if
      end if
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        res_save_20_ad = res
        res = res * x
        res_save_24_ad = res
        if (res > 4.0) then
          res = res * x
          cycle_flag_23_ad = .false.
        end if
      end if
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        res_save_25_ad = res
        res = res * x
        res_save_29_ad = res
        if (res > 2.0) then
          res = res * x
          exit_flag_28_ad = .false.
        end if
      end if
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. cycle_flag_23_ad) then
        res = res_save_29_ad
        if (res > 2.0) then
          exit_flag_28_ad = .true. ! exit
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        res = res_save_25_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. exit_flag_28_ad) then
        res = res_save_24_ad
        if (res > 4.0) then
          cycle_flag_23_ad = .true. ! cycle
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        res = res_save_20_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_14_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        if (i == 4) then
          exit_flag_18_ad = .true. ! exit
        end if
      end if
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        res = res_save_16_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (exit_flag_18_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        if (i == 2) then
          cycle_flag_14_ad = .true. ! cycle
        end if
      end if
      if (cycle_flag_14_ad .and. exit_flag_18_ad .and. cycle_flag_23_ad .and. exit_flag_28_ad) then
        res = res_save_12_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
    end do label_31_0_ad
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
    real, intent(inout) :: x_ad
    real, intent(inout) :: res_ad
    real :: res
    integer :: i
    logical :: exit_flag_53_ad
    logical :: exit_flag_64_ad
    logical :: cycle_flag_49_ad
    logical :: cycle_flag_59_ad
    real :: res_save_46_ad
    integer :: i_save_50_ad
    real :: res_save_51_ad
    real :: res_save_55_ad
    real :: res_save_60_ad
    integer :: i_save_60_ad
    real :: res_save_61_ad
    real :: res_save_65_ad

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

    x_ad = res_ad * res + x_ad ! res = res * x
    res_ad = res_ad * x ! res = res * x
    exit_flag_53_ad = .true.
    exit_flag_64_ad = .true.
    label_68_0_ad: do while (fautodiff_stack_l%get())
      cycle_flag_49_ad = .true.
      cycle_flag_59_ad = .true.
      call fautodiff_stack_pop_r(res)
      call fautodiff_stack_i%pop(i)
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        res_save_46_ad = res
        res = res * x
        i_save_50_ad = i
        if (i == 2) then
          i = i + 1
          cycle_flag_49_ad = .false.
        end if
      end if
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        res_save_51_ad = res
        res = res * x
        if (i == 4) then
          exit_flag_53_ad = .false.
        end if
      end if
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        res_save_55_ad = res
        res = res * x
        i_save_60_ad = i
        res_save_60_ad = res
        if (res > 4.0) then
          res = res * x
          i = i + 1
          cycle_flag_59_ad = .false.
        end if
      end if
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        res_save_61_ad = res
        res = res * x
        res_save_65_ad = res
        if (res > 2.0) then
          res = res * x
          exit_flag_64_ad = .false.
        end if
      end if
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. cycle_flag_59_ad) then
        res = res_save_65_ad
        if (res > 2.0) then
          exit_flag_64_ad = .true. ! exit
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
      end if
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        res = res_save_61_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. exit_flag_64_ad) then
        res = res_save_60_ad
        if (res > 4.0) then
          cycle_flag_59_ad = .true. ! cycle
          x_ad = res_ad * res + x_ad ! res = res * x
          res_ad = res_ad * x ! res = res * x
        end if
        i = i_save_60_ad
      end if
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        res = res_save_55_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (cycle_flag_49_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        if (i == 4) then
          exit_flag_53_ad = .true. ! exit
        end if
      end if
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        res = res_save_51_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      if (exit_flag_53_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        i = i_save_50_ad
        if (i == 2) then
          cycle_flag_49_ad = .true. ! cycle
        end if
      end if
      if (cycle_flag_49_ad .and. exit_flag_53_ad .and. cycle_flag_59_ad .and. exit_flag_64_ad) then
        res = res_save_46_ad
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
    end do label_68_0_ad
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
    real, intent(inout) :: x_ad
    real, intent(inout) :: res_ad
    real :: res
    integer :: i
    integer :: k
    integer :: j
    logical :: exit_flag_89_ad
    logical :: exit_flag_93_ad
    logical :: cycle_flag_105_ad
    integer :: exit_do_start_109_ad
    logical :: exit_flag_97_ad
    logical :: cycle_flag_101_ad
    integer :: exit_do_start_108_ad
    real :: res_save_108_ad
    real :: res_save_95_ad
    real :: res_save_99_ad
    real :: res_save_103_ad

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

    exit_flag_89_ad = .true.
    exit_flag_93_ad = .true.
    label_111_0_ad: do while (fautodiff_stack_l%get())
      cycle_flag_105_ad = .true.
      call fautodiff_stack_pop_r(res)
      if (exit_flag_89_ad .and. exit_flag_93_ad .and. cycle_flag_105_ad) then
        res = res + 1.0
        exit_do_start_109_ad = n
        label_109_1_ad: do j = 1, n
          call fautodiff_stack_push_r(res)
          res = res + 10.0
          if (res > 5000.0) then
            exit_do_start_109_ad = j
            exit_flag_89_ad = .false.
            exit label_109_1_ad
          end if
          label_108_2_ad: do k = 1, n
            if (res > 4000.0) then
              exit_do_start_109_ad = j
              exit_flag_93_ad = .false.
              exit label_109_1_ad
            end if
            res = res + 100.0
            if (res > 2400.0) then
              exit_do_start_109_ad = j
              exit label_109_1_ad
            end if
            res = res + 100.0
            if (res > 2200.0) then
              cycle label_109_1_ad
            end if
            res = res + 100.0
            if (res > 1000.0) then
              cycle_flag_105_ad = .false.
              cycle label_109_1_ad
            end if
            res = res * x
          end do label_108_2_ad
        end do label_109_1_ad
      end if
      if (exit_flag_89_ad .and. exit_flag_93_ad .and. cycle_flag_105_ad) then
        x_ad = res_ad * res + x_ad ! res = res * x
        res_ad = res_ad * x ! res = res * x
      end if
      exit_flag_89_ad = .true.
      exit_flag_93_ad = .true.
      exit_flag_97_ad = .true.
      label_109_0_ad: do j = exit_do_start_109_ad, 1, - 1
        cycle_flag_101_ad = .true.
        cycle_flag_105_ad = .true.
        call fautodiff_stack_pop_r(res)
        if (exit_flag_89_ad .and. exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
          res = res + 10.0
          if (res > 5000.0) then
            exit_flag_89_ad = .false.
          end if
        end if
        if (exit_flag_89_ad .and. exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
          res_save_108_ad = res
          exit_do_start_108_ad = n
          label_108_1_ad: do k = 1, n
            call fautodiff_stack_push_r(res)
            if (res > 4000.0) then
              exit_do_start_108_ad = k
              exit_flag_93_ad = .false.
              exit label_108_1_ad
            end if
            res = res + 100.0
            if (res > 2400.0) then
              exit_do_start_108_ad = k
              exit_flag_97_ad = .false.
              exit label_108_1_ad
            end if
            res = res + 100.0
            if (res > 2200.0) then
              cycle_flag_101_ad = .false.
              cycle label_108_1_ad
            end if
            res = res + 100.0
            if (res > 1000.0) then
              cycle_flag_105_ad = .false.
              cycle label_108_1_ad
            end if
            res = res * x
          end do label_108_1_ad
        end if
        if (exit_flag_89_ad) then
          exit_flag_93_ad = .true.
          exit_flag_97_ad = .true.
          label_108_0_ad: do k = exit_do_start_108_ad, 1, - 1
            cycle_flag_101_ad = .true.
            cycle_flag_105_ad = .true.
            call fautodiff_stack_pop_r(res)
            if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
              if (res > 4000.0) then
                exit_flag_93_ad = .false.
              end if
            end if
            if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
              res_save_95_ad = res
              res = res + 100.0
              if (res > 2400.0) then
                exit_flag_97_ad = .false.
              end if
            end if
            if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
              res_save_99_ad = res
              res = res + 100.0
              if (res > 2200.0) then
                cycle_flag_101_ad = .false.
              end if
            end if
            if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
              res_save_103_ad = res
              res = res + 100.0
              if (res > 1000.0) then
                cycle_flag_105_ad = .false.
              end if
            end if
            if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
              x_ad = res_ad * res + x_ad ! res = res * x
              res_ad = res_ad * x ! res = res * x
            end if
            if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad) then
              if (res > 1000.0) then
                cycle_flag_105_ad = .true. ! cycle outer
              end if
            end if
            if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
              res = res_save_103_ad
            end if
            if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_105_ad) then
              if (res > 2200.0) then
                cycle_flag_101_ad = .true. ! cycle middle
              end if
            end if
            if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
              res = res_save_99_ad
            end if
            if (exit_flag_93_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
              if (res > 2400.0) then
                exit_flag_97_ad = .true. ! exit middle
              end if
            end if
            if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
              res = res_save_95_ad
            end if
            if (exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
              if (res > 4000.0) then
                exit_flag_93_ad = .true. ! exit outer
              end if
            end if
          end do label_108_0_ad
          res = res_save_108_ad
        end if
        if (exit_flag_93_ad .and. exit_flag_97_ad .and. cycle_flag_101_ad .and. cycle_flag_105_ad) then
          if (res > 5000.0) then
            exit_flag_89_ad = .true. ! exit outer
          end if
        end if
      end do label_109_0_ad
    end do label_111_0_ad
    x_ad = res_ad + x_ad ! res = x
    res_ad = 0.0 ! res = x

    return
  end subroutine exit_cycle_with_labels_rev_ad

end module exit_cycle_ad
