      integer function locate (c, pat, offset)
      integer c, pat (128)
      integer offset
      integer i
      i = offset + pat (offset)
23000 if (.not.(i .gt. offset))goto 23002
      if (.not.(c .eq. pat (i)))goto 23003
      locate=(1)
      return
23003 continue
23001 i = i - 1
      goto 23000
23002 continue
      locate=(0)
      return
      end
