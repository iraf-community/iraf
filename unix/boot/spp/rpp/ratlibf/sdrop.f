      integer function sdrop (from, to, chars)
      integer from (100), to (100)
      integer chars
      integer len, start
      integer ctoc, length, min0
      len = length (from)
      if (.not.(chars .lt. 0))goto 23000
      sdrop=(ctoc (from, to, len + chars + 1))
      return
23000 continue
      start = min0 (chars, len)
      sdrop=(ctoc (from (start + 1), to, len + 1))
      return
23001 continue
      end
