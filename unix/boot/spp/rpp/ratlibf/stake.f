      integer function stake (from, to, chars)
      integer from (100), to (100)
      integer chars
      integer len, start
      integer length, ctoc, max0
      len = length (from)
      if (.not.(chars .lt. 0))goto 23000
      start = max0 (len + chars, 0)
      stake=(ctoc (from (start + 1), to, len + 1))
      return
23000 continue
      stake=(ctoc (from, to, chars + 1))
      return
23001 continue
      end
