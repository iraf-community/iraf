      integer function skwrdr (index, outstr, maxch, dict)
      integer index
      integer maxch
      integer*2 outstr(*)
      integer*2 dict(*)
      integer i
      integer len
      integer start
      integer count
      integer xstrln
      save
         outstr(1) = 0
         if (.not.(dict(1) .eq. 0)) goto 110
            skwrdr = (0)
            goto 100
110      continue
         count = 1
         len = xstrln(dict)
         start = 2
120      if (.not.(count .lt. index)) goto 122
            if (.not.(dict(start) .eq. dict(1))) goto 130
               count = count + 1
130         continue
            if (.not.(start .eq. len)) goto 140
               skwrdr = (0)
               goto 100
140         continue
121         start = start + 1
            goto 120
122      continue
         i = start
150      if (.not.(dict(i) .ne. 0 .and. dict(i) .ne. dict(1))) goto 152
            if (.not.(i - start + 1 .gt. maxch)) goto 160
               goto 152
160         continue
            outstr(i - start + 1) = dict(i)
151         i = i + 1
            goto 150
152      continue
         outstr(i - start + 1) = 0
         skwrdr = (count)
         goto 100
100      return
      end
c     skwrdr  sk_wrdstr
