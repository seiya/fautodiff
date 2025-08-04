module block_construct_ad
  use block_construct
  implicit none

  real :: z_ad = 0.0

contains

  subroutine compute_module_fwd_ad(val, val_ad)
    real, intent(in)  :: val
    real, intent(in)  :: val_ad

    z_ad = val_ad ! z = val + 1.0
    z = val + 1.0

    return
  end subroutine compute_module_fwd_ad

  subroutine compute_module_rev_ad(val, val_ad)
    real, intent(in)  :: val
    real, intent(inout) :: val_ad

    val_ad = z_ad + val_ad ! z = val + 1.0
    z_ad = 0.0 ! z = val + 1.0

    return
  end subroutine compute_module_rev_ad

  subroutine use_block_fwd_ad(x, x_ad, y, y_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(out) :: y
    real, intent(out) :: y_ad
    real :: z_ad
    real :: z

    z_ad = x_ad ! z = x + 1.0
    z = x + 1.0
    block
      real :: z
      real :: z_ad = 0.0

      z_ad = x_ad ! z = x + 2.0
      z = x + 2.0
      y_ad = z_ad ! y = z + 1.0
      y = z + 1.0
    end block
    y_ad = y_ad + z_ad ! y = y + z
    y = y + z

    return
  end subroutine use_block_fwd_ad

  subroutine use_block_rev_ad(x, x_ad, y_ad)
    real, intent(in)  :: x
    real, intent(inout) :: x_ad
    real, intent(inout) :: y_ad
    real :: z_ad

    z_ad = y_ad + z_ad ! y = y + z
    block
      real :: z_ad = 0.0

      z_ad = y_ad ! y = z + 1.0
      y_ad = 0.0 ! y = z + 1.0
      x_ad = z_ad + x_ad ! z = x + 2.0
    end block
    x_ad = z_ad + x_ad ! z = x + 1.0
    z_ad = 0.0 ! z = x + 1.0

    return
  end subroutine use_block_rev_ad

end module block_construct_ad
