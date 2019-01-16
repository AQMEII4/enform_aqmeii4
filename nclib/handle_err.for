      subroutine handle_err(errcode)
      implicit none
      INCLUDE 'netcdf.inc'
      INTEGER errcode

      print *, 'Error: ', nf_strerror(errcode)
      stop 2
      end
