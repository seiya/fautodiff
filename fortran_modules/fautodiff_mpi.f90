module fautodiff_mpi
  use mpi
  implicit none
  private
  public :: fautodiff_mpi_bcast_fwd_ad
  public :: fautodiff_mpi_bcast_rev_ad
  public :: fautodiff_mpi_reduce_fwd_ad
  public :: fautodiff_mpi_reduce_rev_ad
  public :: fautodiff_mpi_allreduce_fwd_ad
  public :: fautodiff_mpi_allreduce_rev_ad
  public :: fautodiff_mpi_recv_fwd_ad
  public :: fautodiff_mpi_recv_rev_ad
  public :: fautodiff_mpi_send_fwd_ad
  public :: fautodiff_mpi_send_rev_ad

  interface fautodiff_mpi_bcast_fwd_ad
     module procedure mpi_bcast_fwd_ad_r4
     module procedure mpi_bcast_fwd_ad_r8
  end interface
  interface fautodiff_mpi_bcast_rev_ad
     module procedure mpi_bcast_rev_ad_r4
     module procedure mpi_bcast_rev_ad_r8
  end interface
  interface fautodiff_mpi_reduce_fwd_ad
     module procedure mpi_reduce_fwd_ad_r4
     module procedure mpi_reduce_fwd_ad_r8
  end interface
  interface fautodiff_mpi_reduce_rev_ad
     module procedure mpi_reduce_rev_ad_r4
     module procedure mpi_reduce_rev_ad_r8
  end interface
  interface fautodiff_mpi_allreduce_fwd_ad
     module procedure mpi_allreduce_fwd_ad_r4
     module procedure mpi_allreduce_fwd_ad_r8
  end interface
  interface fautodiff_mpi_allreduce_rev_ad
     module procedure mpi_allreduce_rev_ad_r4
     module procedure mpi_allreduce_rev_ad_r8
  end interface
  interface fautodiff_mpi_recv_fwd_ad
     module procedure mpi_recv_fwd_ad_r4
     module procedure mpi_recv_fwd_ad_r8
  end interface
  interface fautodiff_mpi_recv_rev_ad
     module procedure mpi_recv_rev_ad_r4
     module procedure mpi_recv_rev_ad_r8
  end interface
  interface fautodiff_mpi_send_fwd_ad
     module procedure mpi_send_fwd_ad_r4
     module procedure mpi_send_fwd_ad_r8
  end interface
  interface fautodiff_mpi_send_rev_ad
     module procedure mpi_send_rev_ad_r4
     module procedure mpi_send_rev_ad_r8
  end interface

contains


  subroutine mpi_bcast_fwd_ad_r4(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real, intent(inout) :: buffer(*)
    real, intent(inout) :: buffer_ad(*)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    call MPI_Bcast(buffer, count, datatype, root, comm, ierr)
    call MPI_Bcast(buffer_ad, count, datatype, root, comm, ierr)
  end subroutine mpi_bcast_fwd_ad_r4

  subroutine mpi_bcast_rev_ad_r4(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real, intent(inout) :: buffer(*)
    real, intent(inout) :: buffer_ad(*)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    call MPI_Reduce(buffer_ad, tmp, count, datatype, MPI_SUM, root, comm, ierr)
    if (rank == root) then
      buffer_ad(:count) = tmp(:count)
    else
      buffer_ad(:count) = 0.0
    end if
  end subroutine mpi_bcast_rev_ad_r4

  subroutine mpi_bcast_fwd_ad_r8(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real(8), intent(inout) :: buffer(*)
    real(8), intent(inout) :: buffer_ad(*)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    call MPI_Bcast(buffer, count, datatype, root, comm, ierr)
    call MPI_Bcast(buffer_ad, count, datatype, root, comm, ierr)
  end subroutine mpi_bcast_fwd_ad_r8

  subroutine mpi_bcast_rev_ad_r8(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real(8), intent(inout) :: buffer(*)
    real(8), intent(inout) :: buffer_ad(*)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    call MPI_Reduce(buffer_ad, tmp, count, datatype, MPI_SUM, root, comm, ierr)
    if (rank == root) then
      buffer_ad(:count) = tmp(:count)
    else
      buffer_ad(:count) = 0.0_8
    end if
  end subroutine mpi_bcast_rev_ad_r8

  subroutine mpi_reduce_fwd_ad_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real, intent(in) :: sendbuf(*)
    real, intent(in) :: sendbuf_ad(*)
    real, intent(out) :: recvbuf(*)
    real, intent(out) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    call MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierr)
    call MPI_Reduce(sendbuf_ad, recvbuf_ad, count, datatype, op, root, comm, ierr)
  end subroutine mpi_reduce_fwd_ad_r4

  subroutine mpi_reduce_rev_ad_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real, intent(in) :: sendbuf(*)
    real, intent(inout) :: sendbuf_ad(*)
    real, intent(in) :: recvbuf(*)
    real, intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    if (rank == root) tmp(:) = recvbuf_ad(:)
    call MPI_Bcast(tmp, count, datatype, root, comm, ierr)
    sendbuf_ad(:count) = sendbuf_ad(:count) + tmp(:count)
    if (rank == root) recvbuf_ad(:count) = 0.0
  end subroutine mpi_reduce_rev_ad_r4

  subroutine mpi_reduce_fwd_ad_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real(8), intent(in) :: sendbuf(*)
    real(8), intent(in) :: sendbuf_ad(*)
    real(8), intent(out) :: recvbuf(*)
    real(8), intent(out) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    call MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierr)
    call MPI_Reduce(sendbuf_ad, recvbuf_ad, count, datatype, op, root, comm, ierr)
  end subroutine mpi_reduce_fwd_ad_r8

  subroutine mpi_reduce_rev_ad_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real(8), intent(in) :: sendbuf(*)
    real(8), intent(inout) :: sendbuf_ad(*)
    real(8), intent(in) :: recvbuf(*)
    real(8), intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    if (rank == root) tmp(:) = recvbuf_ad(:)
    call MPI_Bcast(tmp, count, datatype, root, comm, ierr)
    sendbuf_ad(:count) = sendbuf_ad(:count) + tmp(:count)
    if (rank == root) recvbuf_ad(:count) = 0.0_8
  end subroutine mpi_reduce_rev_ad_r8

  subroutine mpi_allreduce_fwd_ad_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real, intent(in) :: sendbuf(*)
    real, intent(in) :: sendbuf_ad(*)
    real, intent(out) :: recvbuf(*)
    real, intent(out) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    call MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierr)
    call MPI_Allreduce(sendbuf_ad, recvbuf_ad, count, datatype, op, comm, ierr)
  end subroutine mpi_allreduce_fwd_ad_r4

  subroutine mpi_allreduce_rev_ad_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real, intent(in) :: sendbuf(*)
    real, intent(inout) :: sendbuf_ad(*)
    real, intent(in) :: recvbuf(*)
    real, intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)

    call MPI_Allreduce(recvbuf_ad, tmp, count, datatype, MPI_SUM, comm, ierr)
    sendbuf_ad(:count) = sendbuf_ad(:count) + tmp(:count)
    recvbuf_ad(:count) = 0.0
  end subroutine mpi_allreduce_rev_ad_r4

  subroutine mpi_allreduce_fwd_ad_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real(8), intent(in) :: sendbuf(*)
    real(8), intent(in) :: sendbuf_ad(*)
    real(8), intent(out) :: recvbuf(*)
    real(8), intent(out) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    call MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierr)
    call MPI_Allreduce(sendbuf_ad, recvbuf_ad, count, datatype, op, comm, ierr)
  end subroutine mpi_allreduce_fwd_ad_r8

  subroutine mpi_allreduce_rev_ad_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real(8), intent(in) :: sendbuf(*)
    real(8), intent(inout) :: sendbuf_ad(*)
    real(8), intent(in) :: recvbuf(*)
    real(8), intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)

    call MPI_Allreduce(recvbuf_ad, tmp, count, datatype, MPI_SUM, comm, ierr)
    sendbuf_ad(:count) = sendbuf_ad(:count) + tmp(:count)
    recvbuf_ad(:count) = 0.0_8
  end subroutine mpi_allreduce_rev_ad_r8


  subroutine mpi_recv_fwd_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real, intent(out) :: buf(*)
    real, intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr
    call MPI_Recv(buf, count, datatype, source, tag, comm, status, ierr)
    call MPI_Recv(buf_ad, count, datatype, source, tag, comm, status, ierr)
  end subroutine mpi_recv_fwd_ad_r4

  subroutine mpi_recv_rev_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real, intent(out) :: buf(*)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr
    call MPI_Send(buf_ad, count, datatype, source, tag, comm, ierr)
    buf_ad(:count) = 0.0
    status = 0
  end subroutine mpi_recv_rev_ad_r4

  subroutine mpi_recv_fwd_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real(8), intent(out) :: buf(*)
    real(8), intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr
    call MPI_Recv(buf, count, datatype, source, tag, comm, status, ierr)
    call MPI_Recv(buf_ad, count, datatype, source, tag, comm, status, ierr)
  end subroutine mpi_recv_fwd_ad_r8

  subroutine mpi_recv_rev_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real(8), intent(out) :: buf(*)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr
    call MPI_Send(buf_ad, count, datatype, source, tag, comm, ierr)
    buf_ad(:count) = 0.0_8
    status = 0
  end subroutine mpi_recv_rev_ad_r8

  subroutine mpi_send_fwd_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real, intent(in) :: buf(*)
    real, intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    call MPI_Send(buf, count, datatype, dest, tag, comm, ierr)
    call MPI_Send(buf_ad, count, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_fwd_ad_r4

  subroutine mpi_send_rev_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real, intent(in) :: buf(*)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    call MPI_Recv(tmp, count, datatype, dest, tag, comm, MPI_STATUS_IGNORE, ierr)
    buf_ad(:count) = buf_ad(:count) + tmp(:count)
  end subroutine mpi_send_rev_ad_r4

  subroutine mpi_send_fwd_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real(8), intent(in) :: buf(*)
    real(8), intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    call MPI_Send(buf, count, datatype, dest, tag, comm, ierr)
    call MPI_Send(buf_ad, count, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_fwd_ad_r8

  subroutine mpi_send_rev_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real(8), intent(in) :: buf(*)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    call MPI_Recv(tmp, count, datatype, dest, tag, comm, MPI_STATUS_IGNORE, ierr)
    buf_ad(:count) = buf_ad(:count) + tmp(:count)
  end subroutine mpi_send_rev_ad_r8

end module fautodiff_mpi
