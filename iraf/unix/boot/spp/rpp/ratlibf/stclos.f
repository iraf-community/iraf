      integer function stclos (pat, j, lastj, lastcl)
      integer pat (128)
      integer j, lastj, lastcl
      integer addset
      integer jp, jt, junk
      jp = j - 1
23000 if (.not.(jp .ge. lastj))goto 23002
      jt = jp + 4
      junk = addset (pat (jp), pat, jt, 128)
23001 jp = jp - 1
      goto 23000
23002 continue
      j = j + 4
      stclos = lastj
      junk = addset (42, pat, lastj, 128)
      junk = addset (0, pat, lastj, 128)
      junk = addset (lastcl, pat, lastj, 128)
      junk = addset (0, pat, lastj, 128)
      return
      end
