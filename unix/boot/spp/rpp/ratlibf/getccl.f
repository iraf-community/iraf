      integer function getccl (arg, i, pat, j)
      integer arg (128), pat (128)
      integer i, j
      integer jstart, junk
      integer addset
      i = i + 1
      if (.not.(arg (i) .eq. 126))goto 23000
      junk = addset (110, pat, j, 128)
      i = i + 1
      goto 23001
23000 continue
      junk = addset (91, pat, j, 128)
23001 continue
      jstart = j
      junk = addset (0, pat, j, 128)
      call filset (93, arg, i, pat, j, 128)
      pat (jstart) = j - jstart - 1
      if (.not.(arg (i) .eq. 93))goto 23002
      getccl = -2
      goto 23003
23002 continue
      getccl = -3
23003 continue
      return
      end
