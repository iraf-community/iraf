      subroutine concat (buf1, buf2, outstr)
      integer buf1(100), buf2(100), outstr(100)
      integer i
      i = 1
      call stcopy (buf1, 1, outstr, i)
      call scopy (buf2, 1, outstr, i)
      return
      end
