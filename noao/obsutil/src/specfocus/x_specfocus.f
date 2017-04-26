      integer function sysruk (task, cmd, rukarf, rukint)
      integer rukarf
      integer rukint
      integer*2 task(*)
      integer*2 cmd(*)
      integer i
      integer ntasks
      integer lmarg
      integer rmarg
      integer maxch
      integer ncol
      integer rukean
      integer envgei
      integer envscn
      logical streq
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer iyy
      integer dp(2)
      integer*2 dict(10)
      integer*2 st0001(9)
      integer*2 st0002(6)
      integer*2 st0003(3)
      integer*2 st0004(6)
      integer*2 st0005(6)
      integer*2 st0006(4)
      integer*2 st0007(6)
      integer*2 st0008(2)
      integer*2 st0009(29)
      integer*2 st0010(25)
      save
      data (dict(iyy),iyy= 1, 8) /115,112,101, 99,102,111, 99,117/
      data (dict(iyy),iyy= 9,10) /115, 0/
      data (st0001(iyy),iyy= 1, 8) /116,116,121,110, 99,111,108,115/
      data (st0001(iyy),iyy= 9, 9) / 0/
      data st0002 / 99,104,100,105,114, 0/
      data st0003 / 99,100, 0/
      data st0004 /104,111,109,101, 36, 0/
      data st0005 / 72, 79, 77, 69, 36, 0/
      data st0006 /115,101,116, 0/
      data st0007 /114,101,115,101,116, 0/
      data st0008 / 9, 0/
      data (st0009(iyy),iyy= 1, 8) /105,110,118, 97,108,105,100, 32/
      data (st0009(iyy),iyy= 9,16) /115,101,116, 32,115,116, 97,116/
      data (st0009(iyy),iyy=17,24) /101,109,101,110,116, 58, 32, 39/
      data (st0009(iyy),iyy=25,29) / 37,115, 39, 10, 0/
      data (st0010(iyy),iyy= 1, 8) /105,110,118, 97,108,105,100, 32/
      data (st0010(iyy),iyy= 9,16) / 83, 69, 84, 32,105,110, 32, 73/
      data (st0010(iyy),iyy=17,24) / 82, 65, 70, 32, 77, 97,105,110/
      data (st0010(iyy),iyy=25,25) / 0/
      data (dp(iyy),iyy= 1, 2) / 1, 0/
      data lmarg /5/, maxch /0/, ncol /0/, rukean /3/
      data ntasks /0/
         if (.not.(ntasks .eq. 0)) goto 110
            i=1
120         if (.not.(dp(i) .ne. 0)) goto 122
121            i=i+1
               goto 120
122         continue
            ntasks = i - 1
110      continue
         if (.not.(task(1) .eq. 63)) goto 130
            call xerpsh
            rmarg = envgei (st0001)
            if (.not.xerpop()) goto 140
               rmarg = 80
140         continue
            call strtbl (4, dict, dp, ntasks, lmarg, rmarg, maxch, ncol)
            sysruk = (0)
            goto 100
130      continue
         if (.not.(streq(task,st0002) .or. streq(task,st0003))) goto 150
            call xerpsh
            if (.not.(cmd(rukarf) .eq. 0)) goto 170
               call xerpsh
               call xfchdr(st0004)
               if (.not.xerpop()) goto 180
                  call xfchdr(st0005)
180            continue
               goto 171
170         continue
               call xfchdr(cmd(rukarf))
171         continue
162         if (.not.xerpop()) goto 160
               if (.not.(rukint .eq. 1)) goto 190
                  call erract (rukean)
                  if (xerflg) goto 100
                  goto 191
190            continue
191            continue
160         continue
            sysruk = (0)
            goto 100
150      continue
         if (.not.(streq(task,st0006) .or. streq(task,st0007))) goto 200
            call xerpsh
            if (.not.(cmd(rukarf) .eq. 0)) goto 220
               call envlit (4, st0008, 1)
               call xffluh(4)
               goto 221
220         continue
            if (.not.(envscn (cmd) .le. 0)) goto 230
               if (.not.(rukint .eq. 1)) goto 240
                  call eprinf (st0009)
                  call pargsr (cmd)
                  goto 241
240            continue
                  goto 91
241            continue
230         continue
221         continue
212         if (.not.xerpop()) goto 210
               if (.not.(rukint .eq. 1)) goto 250
                  call erract (rukean)
                  if (xerflg) goto 100
                  goto 251
250            continue
91                call syspac (0, st0010)
251            continue
210         continue
            sysruk = (0)
            goto 100
200      continue
151      continue
131      continue
         if (.not.(streq (task, dict(dp(1))))) goto 260
            call tspecs
            sysruk = (0)
            goto 100
260      continue
         sysruk = (-1)
         goto 100
100      return
      end
c     rukint  ruk_interact
c     sysruk  sys_runtask
c     envscn  envscan
c     tspecs  t_specfocus
c     envgei  envgeti
c     syspac  sys_panic
c     eprinf  eprintf
c     rukarf  ruk_argoff
c     rukean  ruk_eawarn
c     pargsr  pargstr
c     envlit  envlist
