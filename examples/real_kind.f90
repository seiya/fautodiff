module real_kind
  implicit none
  integer, parameter :: RP = kind(1.0d0)

contains

  subroutine scale_8(x)
    real(kind=8), intent(inout) :: x

    x = x * 2.0_8

    return
  end subroutine scale_8

  subroutine scale_rp(x)
    real(kind=RP), intent(inout) :: x

    x = x * 2.0_RP

    return
  end subroutine scale_rp

  subroutine scale_dp(x)
    double precision, intent(inout) :: x

    x = x * 2.0d0

    return
  end subroutine scale_dp

end module real_kind
