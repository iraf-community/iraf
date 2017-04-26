      integer function ximcot (device, name, type)
      integer*2 device(*)
      integer*2 name(*)
      integer*2 type(*)
      logical Memb(1)
      integer*2 Memc(1)
      integer*2 Mems(1)
      integer Memi(1)
      integer*4 Meml(1)
      real Memr(1)
      double precision Memd(1)
      complex Memx(1)
      equivalence (Memb, Memc, Mems, Memi, Meml, Memr, Memd, Memx)
      common /Mem/ Memd
      integer sp
      integer cmsg
      integer dev
      integer buf
      integer msglen
      integer*2 connet(255 +1)
      integer ndopen
      integer reopen
      integer xstrln
      integer ximred
      logical streq
      external ximonr
      integer fdin
      integer fdout
      integer mode
      integer nbuf
      integer nr
      integer nw
      integer*2 buffer(2047+1)
      integer ximepa
      integer ximstt
      integer oldont
      integer ximfd
      integer ximjmp(64 )
      integer ximert
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      common /ximfd/ fdin, fdout, mode, nbuf, buffer, nr, nw
      common /ximcom/ ximfd, ximjmp, ximepa, ximstt, oldont
      common /ximecm/ ximert
      integer*2 st0001(6)
      integer*2 st0002(5)
      integer*2 st0003(12)
      integer*2 st0004(8)
      integer*2 st0005(12)
      integer*2 st0006(21)
      integer*2 st0007(10)
      integer*2 st0008(8)
      save
      integer iyy
      data st0001 / 37,115, 58, 37,115, 0/
      data st0002 /116,101,120,116, 0/
      data (st0003(iyy),iyy= 1, 8) / 99,111,110,110,101, 99,116, 32/
      data (st0003(iyy),iyy= 9,12) / 37,115, 0, 0/
      data st0004 /120,105,109,116,111,111,108, 0/
      data (st0005(iyy),iyy= 1, 8) /117,110,105,120, 58, 37,115, 58/
      data (st0005(iyy),iyy= 9,12) / 37,115, 0, 0/
      data (st0006(iyy),iyy= 1, 8) / 82,101, 99,111,110,110,101, 99/
      data (st0006(iyy),iyy= 9,16) /116,101,100, 32,111,110, 32, 39/
      data (st0006(iyy),iyy=17,21) / 37,115, 39, 10, 0/
      data (st0007(iyy),iyy= 1, 8) /114,101, 97,100,121, 32, 37,115/
      data (st0007(iyy),iyy= 9,10) / 0, 0/
      data st0008 /120,105,109,116,111,111,108, 0/
      data ximert /0/
         call smark (sp)
         call salloc (buf, 1023 , 2)
         call salloc (cmsg, 1023 , 2)
         call salloc (dev, 255 , 2)
         call aclrc (memc(buf), 1023 )
         call aclrc (memc(cmsg), 1023 )
         call aclrc (memc(dev), 255 )
         call aclrc (buffer, 2047)
         fdin = 0
         fdout = 0
         nbuf = 0
         nr = 0
         nw = 0
         call sprinf (memc(dev), 255 , st0001)
         call pargsr (device)
         call pargsr (type)
         if (.not.(streq (type, st0002))) goto 110
            mode = 1
            goto 111
110      continue
            mode = 2
111      continue
         call xerpsh
         fdin = ndopen (memc(dev), 2)
         if (.not.xerpop()) goto 120
            call sfree (sp)
            ximcot = (-1)
            goto 100
120      continue
         fdout = reopen (fdin, 2)
         call sprinf (memc(cmsg), 1023 , st0003)
         call pargsr (name)
         msglen = xstrln(memc(cmsg))
         call ximmee (st0004, memc(cmsg))
         if (.not.(ximred (memc(buf), msglen) .eq. -2)) goto 130
            call sfree (sp)
            ximcot = (-1)
            goto 100
130      continue
         call xfcloe(fdout)
         call xfcloe(fdin)
         call sprinf (connet, 1023 , st0005)
         call pargsr (memc(buf+8))
         call pargsr (type)
         call xerpsh
         fdin = ndopen (connet, 2)
         if (.not.xerpop()) goto 140
            call sfree (sp)
            ximcot = (-1)
            goto 100
140      continue
         fdout = reopen (fdin, 2)
         if (.not.(.true.)) goto 150
            call eprinf (st0006)
            call pargsr (connet)
150      continue
         call sprinf (memc(cmsg), 1023 , st0007)
         call pargsr (name)
         msglen = xstrln(memc(cmsg))
         call ximmee (st0008, memc(cmsg))
         call onerrr (ximonr)
         call sfree (sp)
         ximcot = (0)
         goto 100
100      return
      end
      subroutine ximdit (sendqt)
      integer sendqt
      integer fdin
      integer fdout
      integer mode
      integer nbuf
      integer nr
      integer nw
      integer*2 buffer(2047+1)
      common /ximfd/ fdin, fdout, mode, nbuf, buffer, nr, nw
      integer*2 st0001(8)
      integer*2 st0002(5)
      save
      data st0001 /120,105,109,116,111,111,108, 0/
      data st0002 /113,117,105,116, 0/
         if (.not.(sendqt .eq. 1)) goto 110
            call ximmee (st0001, st0002)
110      continue
         call xffluh(fdout)
         call xfcloe(fdin)
         call xfcloe(fdout)
         fdin = 0
         fdout = 0
100      return
      end
      subroutine ximmee (object, messae)
      integer*2 object(*)
      integer*2 messae(*)
      logical Memb(1)
      integer*2 Memc(1)
      integer*2 Mems(1)
      integer Memi(1)
      integer*4 Meml(1)
      real Memr(1)
      double precision Memd(1)
      complex Memx(1)
      equivalence (Memb, Memc, Mems, Memi, Meml, Memr, Memd, Memx)
      common /Mem/ Memd
      integer sp
      integer msgbuf
      integer msglen
      integer olen
      integer mlen
      integer ip
      integer xstrln
      logical streq
      integer*2 st0001(8)
      integer*2 st0002(6)
      integer*2 st0003(4)
      integer*2 st0004(4)
      save
      data st0001 /120,105,109,116,111,111,108, 0/
      data st0002 /115,101,110,100, 32, 0/
      data st0003 / 32,123, 32, 0/
      data st0004 / 32,125, 0, 0/
         olen = xstrln(object)
         mlen = xstrln(messae)
         msglen = olen + mlen + 20
         call smark (sp)
         call salloc (msgbuf, msglen, 2)
         call aclrc (memc(msgbuf), msglen)
         if (.not.(streq (object, st0001))) goto 110
            call xstrcy(messae, memc(msgbuf), msglen)
            goto 111
110      continue
            ip = 0
            call amovc (st0002, memc(msgbuf+ip), 5) 
            ip = ip + 5
            call amovc (object, memc(msgbuf+ip), olen) 
            ip = ip + olen
            call amovc (st0003, memc(msgbuf+ip), 3) 
            ip = ip + 3
            call amovc (messae, memc(msgbuf+ip), mlen) 
            ip = ip + mlen
            call amovc (st0004, memc(msgbuf+ip), 2) 
            ip = ip + 3
111      continue
         msglen = xstrln(memc(msgbuf))
         call ximwre (memc(msgbuf), msglen)
         call sfree (sp)
100      return
      end
      subroutine ximalt (text, ok, cancel)
      integer*2 text(*)
      integer*2 ok(*)
      integer*2 cancel(*)
      logical Memb(1)
      integer*2 Memc(1)
      integer*2 Mems(1)
      integer Memi(1)
      integer*4 Meml(1)
      real Memr(1)
      double precision Memd(1)
      complex Memx(1)
      equivalence (Memb, Memc, Mems, Memi, Meml, Memr, Memd, Memx)
      common /Mem/ Memd
      integer sp
      integer msg
      integer*2 st0001(15)
      integer*2 st0002(6)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /123, 37,115,125, 32,123, 37,115/
      data (st0001(iyy),iyy= 9,15) /125, 32,123, 37,115,125, 0/
      data st0002 / 97,108,101,114,116, 0/
         call smark (sp)
         call salloc (msg, 1023 , 2)
         call sprinf (memc(msg), 1023 , st0001)
         call pargsr (text)
         call pargsr (ok)
         call pargsr (cancel)
         call ximmee (st0002, memc(msg))
         call sfree (sp)
100      return
      end
      subroutine ximwre (messae, len)
      integer len
      integer*2 messae(*)
      integer nleft
      integer n
      integer ip
      integer*2 msgbuf(2047+1)
      integer xstrln
      integer fdin
      integer fdout
      integer mode
      integer nbuf
      integer nr
      integer nw
      integer*2 buffer(2047+1)
      common /ximfd/ fdin, fdout, mode, nbuf, buffer, nr, nw
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(42)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /120,105,109, 95,119,114,105,116/
      data (st0001(iyy),iyy= 9,16) /101, 58, 32, 39, 37, 46, 52, 53/
      data (st0001(iyy),iyy=17,24) /115, 39, 32,108,101,110, 61, 37/
      data (st0001(iyy),iyy=25,32) /100, 32,109,111,100,101, 61, 37/
      data (st0001(iyy),iyy=33,40) /100, 32,116,111,116, 61, 37,100/
      data (st0001(iyy),iyy=41,42) / 10, 0/
         len = xstrln(messae) + 1
         messae(len) = 0
         if (.not.(mod(len,2) .eq. 1)) goto 110
            len = len + 1
            messae(len) = 0
110      continue
         ip = 1
         nleft = len
120      if (.not.(nleft .gt. 0)) goto 121
            n = min (nleft, 2047)
            call amovc (messae(ip), msgbuf, n)
            if (.not.(mode .eq. 2)) goto 130
               call achtcb (msgbuf, msgbuf, n)
               call xfwrie(fdout, msgbuf, n / 2 )
               if (xerflg) goto 100
               goto 131
130         continue
               call xfwrie(fdout, msgbuf, n)
               if (xerflg) goto 100
131         continue
            ip = ip + n
            nleft = nleft - n
            goto 120
121      continue
         nw = nw + len
         call xffluh(fdout)
         if (xerflg) goto 100
         if (.not.(.true.)) goto 140
            call eprinf (st0001)
            call pargsr (messae)
            call pargi (len)
            call pargi (mode)
            call pargi (nw)
140      continue
100      return
      end
      integer function ximred (messae, len)
      integer len
      integer*2 messae(*)
      integer i
      integer n
      integer nleft
      integer xfread
      integer fdin
      integer fdout
      integer mode
      integer nbuf
      integer nr
      integer nw
      integer*2 buffer(2047+1)
      integer ximepa
      integer ximstt
      integer oldont
      integer ximfd
      integer ximjmp(64 )
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      common /ximfd/ fdin, fdout, mode, nbuf, buffer, nr, nw
      common /ximcom/ ximfd, ximjmp, ximepa, ximstt, oldont
      integer*2 st0001(42)
      integer*2 st0002(40)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /120,105,109, 95,114,101, 97,100/
      data (st0001(iyy),iyy= 9,16) / 58, 32,116,111,116, 61, 37,100/
      data (st0001(iyy),iyy=17,24) / 32,108,101,110, 61, 37,100, 47/
      data (st0001(iyy),iyy=25,32) / 37,100, 32,109,115,103, 61, 39/
      data (st0001(iyy),iyy=33,40) / 37, 51, 48, 46, 51, 48,115, 39/
      data (st0001(iyy),iyy=41,42) / 10, 0/
      data (st0002(iyy),iyy= 1, 8) /120,105,109, 95,114,101, 97,100/
      data (st0002(iyy),iyy= 9,16) / 58, 32,110, 98,117,102, 61, 37/
      data (st0002(iyy),iyy=17,24) /100, 32,110,108,101,102,116, 61/
      data (st0002(iyy),iyy=25,32) / 37,100, 32, 98,117,102,102,101/
      data (st0002(iyy),iyy=33,40) /114, 61, 39, 37,115, 39, 10, 0/
         if (.not.(nbuf .eq. 0)) goto 110
            call aclrc (buffer, 2047)
            nbuf = 0
            call xerpsh
            n = xfread(fdin, messae, 2047)
            if (xerflg) goto 122
            if (.not.(n .lt. 0)) goto 130
               ximred = (-2)
               goto 100
130         continue
122         if (.not.xerpop()) goto 120
               call xerret()
               call zdojmp (ximjmp, 504 )
120         continue
            if (.not.(mode .eq. 2)) goto 140
               len = n * 2
               call achtbc (messae, messae, len)
               goto 141
140         continue
               len = n
141         continue
            call amovc (messae, buffer, len)
            if (.not.(buffer(len) .eq. 0 .and. buffer(len-1) .eq. 0)) 
     *      goto 150
               nbuf = len
               goto 151
150         continue
               nbuf = len + 1
151         continue
            buffer(nbuf) = -2
110      continue
         i=1
160      if (.not.(buffer(i) .ne. 0 .and. buffer(i) .ne. -2 .and. i .le.
     *    nbuf)) goto 162
            messae(i) = buffer(i)
161         i=i+1
            goto 160
162      continue
         messae(i) = 0
         len = i
         nleft = nbuf - i
         nr = nr + len
         if (.not.(buffer(i) .eq. 0 .and. buffer(i+1) .eq. -2)) goto 170
            if (.not.(i .gt. 1 .and. nleft .gt. 1)) goto 180
               call amovc (buffer(i+1), buffer, nleft)
180         continue
            nbuf = 0
            goto 171
170      continue
            if (.not.(nleft .gt. 0)) goto 190
               call amovc (buffer(i+1), buffer, nleft)
190         continue
            nbuf = nleft
171      continue
         if (.not.(.true.)) goto 200
            call eprinf (st0001)
            call pargi(nr)
            call pargi (len)
            call pargsr(messae)
            call eprinf (st0002)
            call pargi (nbuf)
            call pargi(nleft)
            call pargsr(buffer)
200      continue
         ximred = (nleft)
         goto 100
100      return
      end
      integer function ximinr ()
      external ximzxn
      integer ximepa
      integer ximstt
      integer oldont
      integer ximfd
      integer ximjmp(64 )
      common /ximcom/ ximfd, ximjmp, ximepa, ximstt, oldont
      save
         call zlocpr (ximzxn, ximepa)
         call xwhen (503 , ximepa, oldont)
         call zsvjmp (ximjmp, ximstt)
         if (.not.(ximstt .eq. 0)) goto 110
            ximinr = (0)
            goto 100
110      continue
            ximinr = (-1)
            goto 100
111      continue
100      return
      end
      subroutine ximzxn (vex, nexthr)
      integer vex
      integer nexthr
      integer ximepa
      integer ximstt
      integer oldont
      integer ximfd
      integer ximjmp(64 )
      common /ximcom/ ximfd, ximjmp, ximepa, ximstt, oldont
      save
         call ximdit (1)
         call xerret()
         call zdojmp (ximjmp, vex)
100      return
      end
      subroutine ximonr (status)
      integer status
      integer ximert
      integer code
      integer*2 buf(1023 +1)
      integer*2 errmsg(1023 +1)
      integer errget
      integer ximepa
      integer ximstt
      integer oldont
      integer ximfd
      integer ximjmp(64 )
      common /ximecm/ ximert
      common /ximcom/ ximfd, ximjmp, ximepa, ximstt, oldont
      integer*2 st0001(25)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 73, 83, 77, 32, 69,114,114,111/
      data (st0001(iyy),iyy= 9,16) /114, 44, 32, 99,111,100,101, 32/
      data (st0001(iyy),iyy=17,24) / 37,100, 58, 10, 96, 37,115, 39/
      data (st0001(iyy),iyy=25,25) / 0/
         if (.not.(status .ne. 0)) goto 110
            code = errget (errmsg, 1023 )
            call sprinf (buf, 1023 , st0001)
            call pargi (status)
            call pargsr (errmsg)
            call ximalt (buf, 0, 0)
            call ximdit (1)
110      continue
100      return
      end
c     ximonr  xim_onerror
c     sprinf  sprintf
c     onerrr  onerror
c     ximstt  ximstat
c     ximmee  xim_message
c     messae  message
c     ximcot  xim_connect
c     connet  connect
c     ximinr  xim_intrhandler
c     ximalt  xim_alert
c     oldont  old_onint
c     ximecm  ximecom
c     ximred  xim_read
c     ximjmp  xim_jmp
c     sendqt  send_quit
c     eprinf  eprintf
c     nexthr  next_handler
c     ximzxn  xim_zxwhen
c     xerret  xer_reset
c     ximdit  xim_disconnect
c     ximwre  xim_write
c     pargsr  pargstr
c     ximert  xim_errstat
