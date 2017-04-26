      integer function gfnarg (name, state)
      integer name (100)
      integer state (4)
      integer l
      integer getarg, getlin
      integer fd
      integer rfopen
      integer in1(12)
      integer in2(12)
      integer in3(12)
      data in1(1)/47/,in1(2)/100/,in1(3)/101/,in1(4)/118/,in1(5)/47/,in1
     *(6)/115/,in1(7)/116/,in1(8)/100/,in1(9)/105/,in1(10)/110/,in1(11)/
     *49/,in1(12)/-2/
      data in2(1)/47/,in2(2)/100/,in2(3)/101/,in2(4)/118/,in2(5)/47/,in2
     *(6)/115/,in2(7)/116/,in2(8)/100/,in2(9)/105/,in2(10)/110/,in2(11)/
     *50/,in2(12)/-2/
      data in3(1)/47/,in3(2)/100/,in3(3)/101/,in3(4)/118/,in3(5)/47/,in3
     *(6)/115/,in3(7)/116/,in3(8)/100/,in3(9)/105/,in3(10)/110/,in3(11)/
     *51/,in3(12)/-2/
23000 continue
      if (.not.(state (1) .eq. 1))goto 23003
      state (1) = 2
      state (2) = 1
      state (3) = -3
      state (4) = 0
      goto 23004
23003 continue
      if (.not.(state (1) .eq. 2))goto 23005
      if (.not.(getarg (state (2), name, 128) .ne. -1))goto 23007
      state (1) = 2
      state (2) = state (2) + 1
      if (.not.(name (1) .ne. 45))goto 23009
      state (4) = state (4) + 1
      gfnarg=(-2)
      return
23009 continue
      if (.not.(name (2) .eq. -2))goto 23011
      call scopy (in1, 1, name, 1)
      state (4) = state (4) + 1
      gfnarg=(-2)
      return
23011 continue
      if (.not.(name (2) .eq. 49 .and. name (3) .eq. -2))goto 23013
      call scopy (in1, 1, name, 1)
      state (4) = state (4) + 1
      gfnarg=(-2)
      return
23013 continue
      if (.not.(name (2) .eq. 50 .and. name (3) .eq. -2))goto 23015
      call scopy (in2, 1, name, 1)
      state (4) = state (4) + 1
      gfnarg=(-2)
      return
23015 continue
      if (.not.(name (2) .eq. 51 .and. name (3) .eq. -2))goto 23017
      call scopy (in3, 1, name, 1)
      state (4) = state (4) + 1
      gfnarg=(-2)
      return
23017 continue
      if (.not.(name (2) .eq. 110 .or. name (2) .eq. 78))goto 23019
      state (1) = 3
      if (.not.(name (3) .eq. -2))goto 23021
      state (3) = 0
      goto 23022
23021 continue
      if (.not.(name (3) .eq. 49 .and. name (4) .eq. -2))goto 23023
      state (3) = stdin1
      goto 23024
23023 continue
      if (.not.(name (3) .eq. 50 .and. name (4) .eq. -2))goto 23025
      state (3) = stdin2
      goto 23026
23025 continue
      if (.not.(name (3) .eq. 51 .and. name (4) .eq. -2))goto 23027
      state (3) = stdin3
      goto 23028
23027 continue
      state (3) = rfopen(name (3), 1)
      if (.not.(state (3) .eq. -3))goto 23029
      call putlin (name, 2)
      call remark (14H:  can't open.)
      state (1) = 2
23029 continue
23028 continue
23026 continue
23024 continue
23022 continue
      goto 23020
23019 continue
      gfnarg=(-3)
      return
23020 continue
23018 continue
23016 continue
23014 continue
23012 continue
23010 continue
      goto 23008
23007 continue
      state (1) = 4
23008 continue
      goto 23006
23005 continue
      if (.not.(state (1) .eq. 3))goto 23031
      l = getlin (name, state (3))
      if (.not.(l .ne. -1))goto 23033
      name (l) = -2
      state (4) = state (4) + 1
      gfnarg=(-2)
      return
23033 continue
      if (.not.(fd .ne. -3 .and. fd .ne. 0))goto 23035
      call rfclos(state (3))
23035 continue
      state (1) = 2
      goto 23032
23031 continue
      if (.not.(state (1) .eq. 4))goto 23037
      state (1) = 5
      if (.not.(state (4) .eq. 0))goto 23039
      call scopy (in1, 1, name, 1)
      gfnarg=(-2)
      return
23039 continue
      goto 23002
23037 continue
      if (.not.(state (1) .eq. 5))goto 23041
      goto 23002
23041 continue
      call error (32Hin gfnarg:  bad state (1) value.)
23042 continue
23038 continue
23032 continue
23006 continue
23004 continue
23001 goto 23000
23002 continue
      name (1) = -2
      gfnarg=(-1)
      return
      end
