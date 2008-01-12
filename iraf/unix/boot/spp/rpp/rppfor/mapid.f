      subroutine mapid (name)
      integer name(100)
      integer i
      i=1
23000 if (.not.(name(i) .ne. -2))goto 23002
23001 i=i+1
      goto 23000
23002 continue
      if (.not.(i-1 .gt. 6))goto 23003
      name(6) = name(i-1)
      name(6+1) = -2
23003 continue
      end
