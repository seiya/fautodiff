module save_vars_ad
  implicit none

contains

  subroutine simple_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: y
    real, intent(out) :: y_ad
    real, intent(inout) :: z_ad
    real :: work_ad
    real :: work
    real :: work_save_13_ad
    real :: work_save_15_ad

    work = x + 1.0
    work_save_13_ad = work
    work = work**2
    work_save_15_ad = work
    work = x**2

    work_ad = z_ad * x ! z = work * x + z
    x_ad = z_ad * work ! z = work * x + z
    work = work_save_15_ad
    x_ad = work_ad * 2.0 * x + x_ad ! work = x**2
    work_ad = 0.0 ! work = x**2
    work_ad = z_ad * x + work_ad ! z = work * x + z
    x_ad = z_ad * work + x_ad ! z = work * x + z
    work = work_save_13_ad
    work_ad = work_ad * 2.0 * work ! work = work**2
    work_ad = z_ad + work_ad ! z = work + y
    y_ad = z_ad ! z = work + y
    z_ad = 0.0 ! z = work + y
    x_ad = work_ad + x_ad ! work = x + 1.0

    return
  end subroutine simple_ad

  subroutine if_example_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: y
    real, intent(out) :: y_ad
    real, intent(inout) :: z_ad
    real :: work_ad
    real :: work
    real :: work_save_37_ad
    real :: work_save_29_ad
    real :: work_save_38_ad

    work = x + 1.0
    work_save_37_ad = work
    if (work > 0.0) then
      work = work**2
    else if (work < 0.0) then
      work = x
      work = work * x
    end if
    work_save_38_ad = work
    work = work * x

    y_ad = 0.0

    work_ad = z_ad * x ! z = work * x + z
    x_ad = z_ad * work ! z = work * x + z
    work = work_save_38_ad
    work_ad = work_ad * x ! work = work * x
    x_ad = work_ad * work + x_ad ! work = work * x
    work = work_save_37_ad
    if (work > 0.0) then
      work_save_29_ad = work
      work = work**2
      work_ad = z_ad * x + work_ad ! z = work * x + z
      x_ad = z_ad * work + x_ad ! z = work * x + z
      work = work_save_29_ad
      work_ad = work_ad * 2.0 * work ! work = work**2
    else if (work < 0.0) then
      work = x
      work_ad = work_ad * x ! work = work * x
      x_ad = work_ad * work + x_ad ! work = work * x
      work_ad = z_ad * y + work_ad ! z = work * y
      y_ad = z_ad * work ! z = work * y
      z_ad = 0.0 ! z = work * y
      x_ad = work_ad + x_ad ! work = x
      work_ad = 0.0 ! work = x
    else
      work_ad = z_ad * x + work_ad ! z = work * x
      x_ad = z_ad * work + x_ad ! z = work * x
      z_ad = 0.0 ! z = work * x
    end if
    work = work_save_37_ad
    work_ad = z_ad * y + work_ad ! z = work * y
    y_ad = z_ad * work + y_ad ! z = work * y
    z_ad = 0.0 ! z = work * y
    x_ad = work_ad + x_ad ! work = x + 1.0

    return
  end subroutine if_example_ad

  subroutine do_with_array_private_ad(n, m, x, x_ad, y, y_ad, z_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x(n,m)
    real, intent(out) :: x_ad(n,m)
    real, intent(in)  :: y(n,m)
    real, intent(out) :: y_ad(n,m)
    real, intent(inout) :: z_ad(n,m)
    real :: scalar_ad
    real :: ary_ad(n,m)
    real :: ary(n,m)
    integer :: i
    integer :: j
    real :: z(n,m)
    real :: scalar
    real :: ary_save_58_ad
    real :: z_save_60_ad
    real :: scalar_save_62_ad

    ary(:,:) = x(:,:)

    do j = m, 1, - 1
      do i = n, 1, - 1
        z(i,j) = ary(i,j) * y(i,j)
        ary_save_58_ad = ary(i,j)
        ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
        scalar = ary(i,j) * z(i,j)
        z_save_60_ad = z(i,j)
        z(i,j) = x(i,j) + scalar
        z(i,j) = y(i,j) * scalar + z(i,j)
        scalar_save_62_ad = scalar
        scalar = z(i,j) * y(i,j)
        z_ad(i,j) = z_ad(i,j) * scalar ! z(i,j) = z(i,j) * scalar
        scalar_ad = z_ad(i,j) * z(i,j) ! z(i,j) = z(i,j) * scalar
        scalar = scalar_save_62_ad
        z_ad(i,j) = scalar_ad * y(i,j) + z_ad(i,j) ! scalar = z(i,j) * y(i,j)
        y_ad(i,j) = scalar_ad * z(i,j) ! scalar = z(i,j) * y(i,j)
        scalar_ad = 0.0 ! scalar = z(i,j) * y(i,j)
        y_ad(i,j) = z_ad(i,j) * scalar + y_ad(i,j) ! z(i,j) = y(i,j) * scalar + z(i,j)
        scalar_ad = z_ad(i,j) * y(i,j) + scalar_ad ! z(i,j) = y(i,j) * scalar + z(i,j)
        z(i,j) = z_save_60_ad
        x_ad(i,j) = z_ad(i,j) ! z(i,j) = x(i,j) + scalar
        scalar_ad = z_ad(i,j) + scalar_ad ! z(i,j) = x(i,j) + scalar
        z_ad(i,j) = 0.0 ! z(i,j) = x(i,j) + scalar
        ary_ad(i,j) = scalar_ad * z(i,j) ! scalar = ary(i,j) * z(i,j)
        z_ad(i,j) = scalar_ad * ary(i,j) + z_ad(i,j) ! scalar = ary(i,j) * z(i,j)
        ary(i,j) = ary_save_58_ad
        x_ad(i,j) = ary_ad(i,j) * ary(i,j) + x_ad(i,j) ! ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
        ary_ad(i,j) = ary_ad(i,j) * x(i,j) ! ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
        z_ad(i,j) = ary_ad(i,j) + z_ad(i,j) ! ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
        ary_ad(i,j) = z_ad(i,j) * y(i,j) + ary_ad(i,j) ! z(i,j) = ary(i,j) * y(i,j)
        y_ad(i,j) = z_ad(i,j) * ary(i,j) + y_ad(i,j) ! z(i,j) = ary(i,j) * y(i,j)
        z_ad(i,j) = 0.0 ! z(i,j) = ary(i,j) * y(i,j)
      end do
    end do
    x_ad(:,:) = ary_ad(:,:) + x_ad(:,:) ! ary(:,:) = x(:,:)

    return
  end subroutine do_with_array_private_ad

  subroutine do_with_array_ad(n, m, x, x_ad, y, y_ad, z_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x(n,m)
    real, intent(out) :: x_ad(n,m)
    real, intent(in)  :: y(n,m)
    real, intent(out) :: y_ad(n,m)
    real, intent(inout) :: z_ad(n,m)
    real :: ary_ad(n,m)
    integer :: i
    integer :: j
    real :: ary(n,m)
    real :: z(n,m)
    real :: ary_save_89_ad(n,m)
    real :: z_save_92_ad
    real :: ary_save_93_ad

    do j = 1, m
      do i = 1, n
        ary(i,j) = x(i,j) + 1.0
      end do
    end do
    ary_save_89_ad(:,:) = ary(:,:)
    do j = 1, m
      do i = 1, n
        z(i,j) = ary(i,j) * x(i,j)
        ary(i,j) = ary(i,j) + z(i,j) * y(i,j)
      end do
    end do

    do j = m, 1, - 1
      do i = n, 1, - 1
        z_save_92_ad = z(i,j)
        ary_save_93_ad = ary(i,j)
        ary_ad(i,j) = z_ad(i,j) ! z(i,j) = z(i,j) + ary(i,j)
        ary(i,j) = ary_save_93_ad
        y_ad(i,j) = ary_ad(i,j) * ary(i,j) ! ary(i,j) = y(i,j) * ary(i,j)
        ary_ad(i,j) = ary_ad(i,j) * y(i,j) ! ary(i,j) = y(i,j) * ary(i,j)
        z(i,j) = z_save_92_ad
        z_ad(i,j) = z_ad(i,j) * x(i,j) ! z(i,j) = z(i,j) * x(i,j) + ary(i,j)
        x_ad(i,j) = z_ad(i,j) * z(i,j) ! z(i,j) = z(i,j) * x(i,j) + ary(i,j)
        ary_ad(i,j) = z_ad(i,j) + ary_ad(i,j) ! z(i,j) = z(i,j) * x(i,j) + ary(i,j)
      end do
    end do
    ary(:,:) = ary_save_89_ad(:,:)
    do j = m, 1, - 1
      do i = n, 1, - 1
        z(i,j) = ary(i,j) * x(i,j)
        z_ad(i,j) = ary_ad(i,j) * y(i,j) + z_ad(i,j) ! ary(i,j) = ary(i,j) + z(i,j) * y(i,j)
        y_ad(i,j) = ary_ad(i,j) * z(i,j) + y_ad(i,j) ! ary(i,j) = ary(i,j) + z(i,j) * y(i,j)
        ary_ad(i,j) = z_ad(i,j) * x(i,j) + ary_ad(i,j) ! z(i,j) = ary(i,j) * x(i,j)
        x_ad(i,j) = z_ad(i,j) * ary(i,j) + x_ad(i,j) ! z(i,j) = ary(i,j) * x(i,j)
        z_ad(i,j) = 0.0 ! z(i,j) = ary(i,j) * x(i,j)
      end do
    end do
    do j = m, 1, - 1
      do i = n, 1, - 1
        x_ad(i,j) = ary_ad(i,j) + x_ad(i,j) ! ary(i,j) = x(i,j) + 1.0
      end do
    end do

    return
  end subroutine do_with_array_ad

  subroutine do_with_local_array_ad(n, m, x, x_ad, y, y_ad, z_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x(n,m)
    real, intent(out) :: x_ad(n,m)
    real, intent(in)  :: y(n,m)
    real, intent(out) :: y_ad(n,m)
    real, intent(inout) :: z_ad(n,m)
    real :: work1(2,n,m)
    real :: work1_ad(2,n,m)
    real :: work2(2,m)
    real :: work2_ad(2,m)
    real :: work3(2)
    real :: work3_ad(2)
    real :: z(n,m)
    real :: work1_save_1_ad(2,n,m)
    integer :: i
    integer :: j
    integer :: k

    do j = m, 1, - 1
      do i = n, 1, - 1
        z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        do k = 1, 2
          work3(k) = work1(k,i,j)
          work1(k,i,j) = x(i,j) * work3(k)
        end do
        work1_save_1_ad(:,i,j) = work1(:,i,j)
        z_ad(i,j) = z_ad(i,j) * (work3(1) + work3(2)) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        work3_ad(1) = z_ad(i,j) * z(i,j) + work3_ad(1) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        work3_ad(2) = z_ad(i,j) * z(i,j) + work3_ad(2) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        work1_ad(1,i,j) = z_ad(i,j) * y(i,j) + work1_ad(1,i,j) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        y_ad(i,j) = z_ad(i,j) * work1(1,i,j) + y_ad(i,j) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        work1_ad(2,i,j) = z_ad(i,j) * x(i,j) + work1_ad(2,i,j) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        x_ad(i,j) = z_ad(i,j) * work1(2,i,j) + x_ad(i,j) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        do k = 2, 1, - 1
          x_ad(i,j) = work1_ad(k,i,j) + x_ad(i,j) ! work1(k,i,j) = x(i,j) * work3(k)
          work3_ad(k) = work1_ad(k,i,j) * x(i,j) + work3_ad(k) ! work1(k,i,j) = x(i,j) * work3(k)
          work1_ad(k,i,j) = 0.0 ! work1(k,i,j) = x(i,j) * work3(k)
          work1_ad(k,i,j) = work3_ad(k) + work1_ad(k,i,j) ! work3(k) = work1(k,i,j)
          work3_ad(k) = 0.0
        end do
        work1(:,i,j) = work1_save_1_ad(:,i,j)
        work1_ad(1,i,j) = z_ad(i,j) * y(i,j) + work1_ad(1,i,j) ! z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        y_ad(i,j) = z_ad(i,j) * work1(1,i,j) + y_ad(i,j) ! z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        work1_ad(2,i,j) = z_ad(i,j) * x(i,j) + work1_ad(2,i,j) ! z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        x_ad(i,j) = z_ad(i,j) * work1(2,i,j) + x_ad(i,j) ! z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        z_ad(i,j) = 0.0
      end do
    end do

    do j = m, 1, - 1
      do i = 1, n
        work3(1) = x(i,j) + y(i,j)
        work3(2) = x(i,j) - y(i,j)
        work2(1,i) = work3(1) + work3(2) * x(i,j)
        work2(2,i) = work3(1) * work3(2) + y(i,j)
      end do

      do i = n, 1, - 1
        work2_ad(2,i) = work1_ad(2,i,j) * y(i,j) ! work1(2,i,j) = work2(2,i) * y(i,j)
        y_ad(i,j) = work1_ad(2,i,j) * work2(2,i) + y_ad(i,j) ! work1(2,i,j) = work2(2,i) * y(i,j)
        work2_ad(1,i) = work1_ad(1,i,j) * x(i,j) + work2_ad(1,i) ! work1(1,i,j) = work2(1,i) * x(i,j)
        x_ad(i,j) = work1_ad(1,i,j) * work2(1,i) + x_ad(i,j) ! work1(1,i,j) = work2(1,i) * x(i,j)
      end do

      do i = n, 1, - 1
        work3(1) = x(i,j) + y(i,j)
        work3(2) = x(i,j) - y(i,j)
        work3_ad(1) = work2_ad(2,i) * work3(2) + work3_ad(1) ! work2(2,i) = work3(1) * work3(2) + y(i,j)
        work3_ad(2) = work2_ad(2,i) * work3(1) + work3_ad(2) ! work2(2,i) = work3(1) * work3(2) + y(i,j)
        y_ad(i,j) = work2_ad(2,i) + y_ad(i,j) ! work2(2,i) = work3(1) * work3(2) + y(i,j)
        work3_ad(1) = work2_ad(1,i) + work3_ad(1) ! work2(1,i) = work3(1) + work3(2) * x(i,j)
        work3_ad(2) = work2_ad(1,i) * x(i,j) + work3_ad(2) ! work2(1,i) = work3(1) + work3(2) * x(i,j)
        x_ad(i,j) = work2_ad(1,i) * work3(2) + x_ad(i,j) ! work2(1,i) = work3(1) + work3(2) * x(i,j)
        x_ad(i,j) = work3_ad(2) + x_ad(i,j) ! work3(2) = x(i,j) - y(i,j)
        y_ad(i,j) = - work3_ad(2) + y_ad(i,j) ! work3(2) = x(i,j) - y(i,j)
        work3_ad(2) = 0.0
        x_ad(i,j) = work3_ad(1) + x_ad(i,j) ! work3(1) = x(i,j) + y(i,j)
        y_ad(i,j) = work3_ad(1) + y_ad(i,j) ! work3(1) = x(i,j) + y(i,j)
        work3_ad(1) = 0.0
      end do
    end do

    return
  end subroutine do_with_local_array_ad

  subroutine do_with_recurrent_scalar_ad()

    return
  end subroutine do_with_recurrent_scalar_ad

  subroutine do_with_stencil_array_ad()

    return
  end subroutine do_with_stencil_array_ad

end module save_vars_ad
