      subroutine swvar (lab)
      integer lab, i, labnum, ndigi0
      call outch (115)
      call outch (119)
      labnum = lab
      ndigi0=0
23000 if (.not.(labnum .gt. 0))goto 23002
      ndigi0 = ndigi0 + 1
23001 labnum=labnum/10
      goto 23000
23002 continue
      i=3
23003 if (.not.(i .le. 6 - ndigi0))goto 23005
      call outch (48)
23004 i=i+1
      goto 23003
23005 continue
      call outnum (lab)
      return
      end
c     ndigi0  ndigits
