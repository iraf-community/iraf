      subroutine z8zpii
c+kpno
c
c All data statements changed to runtime assignment statements; routine
c changed from block data to subroutine.
c
c-kpno
      common /sysplt/ mmajx  ,mmajy  ,mminx  ,mminy  ,mxlab  ,mylab  ,
     1                mflg   ,mtype  ,mxa    ,mya    ,mxb    ,myb    ,
     2                mx     ,my     ,mtypex ,mtypey ,xxa    ,yya    ,
     3                xxb    ,yyb    ,xxc    ,yyc    ,xxd    ,yyd    ,
     4                xfactr ,yfactr ,xadd   ,yadd   ,xx     ,yy     ,
     5                mfmtx(3)       ,mfmty(3)       ,mumx   ,mumy   ,
     6                msizx  ,msizy  ,mxdec  ,mydec  ,mxor   ,mop(19),
     7                mname(19)      ,mxold  ,myold  ,mxmax  ,mymax  ,
     8                mxfac  ,myfac  ,modef  ,mf2er  ,mshftx ,mshfty ,
     9                mmgrx  ,mmgry  ,mmnrx  ,mmnry  ,mfrend ,mfrlst ,
     +                mcrout ,mpair1 ,mpair2 ,msblen ,mflcnt ,mjxmin ,
     1                mjymin ,mjxmax ,mjymax ,mnxsto ,mnysto ,mxxsto ,
     2                mxysto ,mprint ,msybuf(360)    ,mncpw  ,minst  ,
     3                mbufa  ,mbuflu ,mfwa(12)       ,mlwa(12)       ,
     4                mipair ,mbprs(16)      ,mbufl  ,munit  ,mbswap ,
     5                small
c
      common /nsplt1/  iclrfb ,isetfb ,ibpw   ,ifwd
c
c      variables                 use
c      ---------                 ---
c
c mmajx,mmajy,mminx,   gridal arguments stored here so they will be in a
c mminy,mxlab,mylab,   known order for insertion in the instruction
c mflg                 stream only when ultracompact metacode is
c                      being produced.
c
c mtype                scaling type of the most recent set call
c
c mx,my                plotter address of the pen location
c
c mxa,mya,mxb,myb      plotter address corresponding to the first four
c                      arguments of the most recent set call.
c
c mtypex,mtypey        a decoding of mtype-- 0 = linear, 1 = log
c
c xxa,yya,xxb,yyb,     exact copies of the first eight parameters
c xxc,yyc,xxd,yyd      of the most recent set call
c
c xfactr,yfactr,xadd,  numbers computed from the most recent set call
c yadd                 arguments so that real valued coordinates can
c                      be translated to integers by
c                       mx = xfactr*xx + xadd
c                      or
c                       mx = xfactr*alog10(xx) + xadd
c                      and similarly for y.
c
c xx,yy                most recent coordinate input to the plot package
c
c mfmtx,mfmty,mumx,    most recent labmod inputs except that mxdec = 0
c mumy,msizx,msizy,    and mydec = 0 are decoded and mxdec = 1 and
c mxdec,mydec,mxor     mydec = 1 become 0.
c
c mop(i),mname(i)      option names are given in mname and their
c                      current values in mop
c
c mxold,myold,mxmax,   all used for increment instructions only.  mxold
c mymax,mxfac,myfac    and myold are the plotter coordinates of the
c                      previous point, mxmax and mymax are the greatest
c                      distance an increment can move, and mxfac and
c                      myfac are the number of plotter units per
c                      increment unit (generally 1, but can be more if
c                      compaction is important and high resolution is
c                      not).
c
c modef                = 0 flash routines have not been used
c                      = 1 most recent flash call was to flash1
c                          (we are between flash1 and flash2 calls
c                          and the instructions should be put in the
c                          users buffer)
c                      = 2 flash1 call has been closed with a
c                          flash2 call
c                      =-3 flash3 has been entered, but not exited,
c                          i.e., flash3 is dumping a user buffer.
c                      = 3 most recent flash activity is a completed
c                          flash3 call.
c                      = 4 most recent flash call was to flash4
c
c mf2er                = 0 no flash buffer overflow
c                      = n counts the number of times the buffer
c                          was reused so the required size can be
c                          estimated
c
c mshftx,mshfty        the power of two of the ratio between the
c                      resolution of the metacode address and the
c                      resolution the user is working in.  in the
c                      default case, the user assumes the plotter
c                      is 1024 by 1024 (1024 = 2 **10).  metacode
c                      addresses have 15 bits, so their capacity is
c                      32,768.  thus, the default for mshftx and mshfty
c                      is 5, and user integer coordinates are left
c                      shifted 5 to make plotter addresses.
c
c mmgrx,mmgry,mmnrx,   tick mark lengths (positive values point in)
c mmnry
c
c mcrout               number of metacode records that have been put
c                      out via preout.
c
c mflcnt               used to count the number of flushb calls since
c                      last mbprs initialization.  it is used to avoid
c                      empty records which could otherwise be put out.
c
c mfrend               frame sets to 1 to indicate last output call of a
c                      frame,  and resets to zero before returning.
c
c mfrlst               preout manipulates, based on mfrend, so that it
c                      knows when a record is the first of a new frame.
c
c mjxmin,mjymin,       used to keep track of the range of the plotting
c mjxmax,mjymax        address on the frame being created
c
c mnxsto,mnysto        used to hold mjxmin,... after flash1 call, and
c mxxsto,mxysto        restore them after flash2.  mjxmin,... are ac-
c                      cumulated anew during flash saving, and stored
c                      in user flash buffer after flash2 call.
c
c mpair1,mpair2        two 16-bit pairs used to initialize each output
c                      record, so that preout may format first 32 bits.
c                      they are actually put into mbprs at proper times
c
c mprint               unit number for printing error messages too
c                      extensive to be handled by uliber
c
c msybuf               buffer to hold up to a few hundred metacode
c                      instructions
c
c msblen               word length of msybuf.
c
c mncpw                the number of characters per word on the host
c                      computer
c
c minst                holds instruction op-code for the instruction
c                      being formed
c
c mbufa                contains the address of the buffer for the
c                      metacode instructions, either loci(msybuf) or
c                      loci(user buffer) from a flash1 call
c
c mbuflu               the number of words of the buffer pointed to by
c                      mbufa that have been filled with metacode or
c                      dd80 instructions
c
c mfwa,mlwa            contains the first word address and the last
c                      word address for the flash buffers
c
c mipair,mbprs         mbprs is used to store byte pairs of metacode
c                      until they can be packed in an integral number
c                      of words and placed in the buffer pointed to by
c                      mbufa.  mipair tells how much of mbprs has been
c                      used.
c
c mbufl                the length of the buffer pointed to by mbufa.
c
c munit                unit number for writing metacode
c
c small                smallest positive number on the host computer.
c                      this is used when nonpositive numbers are plotted
c                      with log scaling.
c
c
      dimension mfssx(2), mfssy(2), mnsss(9)
c
      data mfssx(1)/4h(e10/
      data mfssx(2)/4h.3) /
      data mfssy(1)/4h(e10/
      data mfssy(2)/4h.3) /
c
      data mnsss(1)/4hcase/
      data mnsss(2)/4hintn/
      data mnsss(3)/4horen/
      data mnsss(4)/4hcsiz/
      data mnsss(5)/4hfont/
      data mnsss(6)/4hdpat/
      data mnsss(7)/4hssiz/
      data mnsss(8)/4hcent/
      data mnsss(9)/4hcolr/
c
      do 10 i = 1, 2
	  mfmtx(i) = mfssx(i)
10    continue
      do 11 i = 1, 2
	  mfmty(i) = mfssy(i)
11    continue
      do 12 i = 1, 9
	  mname(i) = mnsss(i)
12    continue
c
c     data iclrfb/0/, isetfb/0/, ibpw/32/, ifwd/1/
      iclrfb   = 0
      isetfb   = 0
      ibpw     = 32
      ifwd     = 1
c
c     data mtype,mtypex,mtypey/1,0,0/
      mtype    = 1
      mtypex   = 0
      mtypey   = 0
c
c     data mx,my/0,0/
      mx       = 0
      my       = 0
c
c     data xxa,yya,xxb,yyb/0.,0.,1.,1./
      xxa      = 0.0
      yya      = 0.0
      xxb      = 1.0
      yyb      = 1.0
c
c     data xxc,yyc,xxd,yyd/0.,0.,1.,1./
      xxc      = 0.0
      yyc      = 0.0
      xxd      = 1.0
      yyd      = 1.0
c
c     data mxa,mya,mxb,myb/1,1,32767,32767/
      mxa      = 1
      mya      = 32767
      mxb      = 1
      mxb      = 32767
c
c     data xfactr,yfactr/32767.,32767./
      xfactr   = 32767.
      yfactr   = 32767.
c
c     data xadd,yadd/1.,1./
      xadd     = 1.0
      yadd     = 1.0
c
c     data mumx,mumy/10,10/
      mumx     = 10
      mumy     = 10
c
c     data msizx,msizy/0,0/
      msizx    = 0
      msizy    = 0
c
c     data mxdec,mydec/655,655/
      mxdec    = 655
      mydec    = 655
c
c     data mxor/0/
      mxor     = 0
c
c     data mop(1)/0/
c     data mop(2)/204/
c     data mop(3)/0/
c     data mop(4)/128/
c     data mop(5)/0/
c     data mop(6)/65535/
c     data mop(7)/8/
c     data mop(8)/0/
c     data mop(9)/0/
      mop(1)   = 0
      mop(2)   = 204
      mop(3)   = 0
      mop(4)   = 128
      mop(5)   = 0
      mop(6)   = 65535
      mop(7)   = 8
      mop(8)   = 0
      mop(9)   = 0
c
c     data mxold,myold/-9999,-9999/
      mxold    = -9999
      myold    = -9999
c
c     data mxmax,mymax/31,31/
      mxmax    = 31
      mymax    = 31
c
c     data mxfac,myfac/1,1/
      mxfac    = 1
      myfac    = 1
c
c     data mmgrx,mmgry/385,385/
      mmgrx    = 385
      mmgry    = 385
c
c     data mmnrx,mmnry/255,255/
      mmnrx    = 255
      mmnry    = 255
c
c     data modef/0/
      modef    = 0
c
c     data mncpw/4/
      mncpw    = 4
c
c     data mbuflu/0/
      mbuflu   = 0
c
c     data msblen/360/
      msblen   = 360
c
c     data mbufl/360/
      mbufl    = 360
c
c     data mf2er/0/
      mf2er    = 0
c
c     data mshftx,mshfty/5,5/
      mshftx   = 5
      mshfty   = 5
c
c     data mbufa/-9999/
      mbufa    = -9999
c
c     data mflcnt/0/
      mflcnt   = 0
c
c     data mfrend/0/
      mfrend   = 0
c
c     data mfrlst/1/
      mfrlst   = 1
c
c     data mpair1/0/
      mpair1   = 0
c
c     data mpair2/8192/
      mpair2   = 8192
c
c     data mcrout/0/
      mcrout   = 0
c
c     data mbprs(1)/0/
      mbprs(1) = 0
c
c     data mbprs(2)/8192/
      mbprs(2) = 8192
c
c     data mipair/2/
      mipair   = 2
c
c     data mjxmax,mjymax,mjxmin,mjymin/0,0,32767,32767/
      mjxmax   = 0
      mjymax   = 0
      mjxmin   = 32767
      mjxmin   = 32767
c
c set to unit number for printer
c
c     data mprint/6/
      mprint   = 6
c
c set to unit number for plotter
c
c     data munit/8/
      munit     = 8
c set to smallest positive number on the computer
c
c     data small/1.e-25/
      small     = 1.e-25
      end
