      integer function mntoc (buf, p, defalt)
      integer buf (100), defalt
      integer p
      integer i, tp
      integer equal
      integer c, tmp (128)
      integer text (170)
      data text /  6, 97, 99, 107, -2, 7, 98, 101, 108, -2, 8, 98, 115, 
     *-2, -2, 24, 99, 97, 110, -2, 13, 99, 114, -2, -2, 17, 100, 99, 49,
     * -2, 18, 100, 99, 50, -2, 19, 100, 99, 51, -2, 20, 100, 99, 52, -2
     *, 127, 100, 101, 108, -2, 16, 100, 108, 101, -2, 25, 101, 109, -2,
     * -2, 5, 101, 110, 113, -2, 4, 101, 111, 116, -2, 27, 101, 115, 99,
     * -2, 23, 101, 116, 98, -2, 3, 101, 116, 120, -2, 12, 102, 102, -2,
     * -2, 28, 102, 115, -2, -2, 29, 103, 115, -2, -2, 9, 104, 116, -2, 
     *-2, 10, 108, 102, -2, -2, 21, 110, 97, 107, -2, 0, 110, 117, 108, 
     *-2, 30, 114, 115, -2, -2, 15, 115, 105, -2, -2, 14, 115, 111, -2, 
     *-2, 1, 115, 111, 104, -2, 32, 115, 112, -2, -2, 2, 115, 116, 120, 
     *-2, 26, 115, 117, 98, -2, 22, 115, 121, 110, -2, 31, 117, 115, -2,
     * -2, 11, 118, 116, -2, -2/
      tp = 1
23000 continue
      tmp (tp) = buf (p)
      tp = tp + 1
      p = p + 1
23001 if (.not.(.not. (((65.le.buf (p).and.buf (p).le.90).or.(97.le.buf 
     *(p).and.buf (p).le.122)) .or. (48.le.buf (p).and.buf (p).le.57)) .
     *or. tp .ge. 128))goto 23000
23002 continue
      tmp (tp) = -2
      if (.not.(tp .eq. 2))goto 23003
      c = tmp (1)
      goto 23004
23003 continue
      call lower (tmp)
      i = 1
23005 if (.not.(i .lt. 170))goto 23007
      if (.not.(equal (tmp, text (i + 1)) .eq. 1))goto 23008
      goto 23007
23008 continue
23006 i = i + 5
      goto 23005
23007 continue
      if (.not.(i .lt. 170))goto 23010
      c = text (i)
      goto 23011
23010 continue
      c = defalt
23011 continue
23004 continue
      mntoc=(c)
      return
      end
