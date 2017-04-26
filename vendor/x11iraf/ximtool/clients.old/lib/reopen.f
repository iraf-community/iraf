      integer function reopen (fd, mode)
      integer fd
      integer mode
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
      integer newfp
      integer ffp
      integer newfd
      integer fgetfd
      integer*4 boffst(4096 )
      integer bufptr(4096 )
      integer buftop(4096 )
      integer iop(4096 )
      integer itop(4096 )
      integer otop(4096 )
      integer fiodes(4096 )
      integer fflags(4096 )
      integer redird(4096 )
      integer zdev(150 )
      integer nextdv
      integer fp
      integer*2 pathne(511 +1)
      logical xerflg
      common /xercom/ xerflg
      common /fiocom/ boffst, bufptr, buftop, iop, itop, otop, fiodes, 
     *fflags, redird, zdev, nextdv, fp, pathne
      save
         ffp = fiodes(fd)
         if (.not.(fd .le. 0 .or. ffp .eq. 0)) goto 110
            call syserr (733)
            if (xerflg) goto 100
110      continue
         if (.not.(memi(ffp+1) .eq. 1 .and. mode .ne. 1 )) goto 120
            call filerr (memc((((ffp+20+(10+256))-1)*2+1)) , 750)
120      continue
         if (.not.(memi(ffp+2) .ne. 12)) goto 130
            call filerr (memc((((ffp+20+(10+256))-1)*2+1)) , 751)
130      continue
         newfd = fgetfd (memc((((ffp+20+(10+256))-1)*2+1)) , mode, 12)
         newfp = fiodes(newfd)
         memi(newfp+3) = memi(ffp+3)
         memi(newfp+4) = memi(ffp+4)
         memi(newfp) = memi(ffp)
         if (.not.(memi(ffp+18) .eq. (ffp+20) )) goto 140
            call xmallc(memi(ffp+18) , (10+256), 10 )
            if (xerflg) goto 100
            call amovi (memi((ffp+20) ), memi(memi(ffp+18) ), (10+256))
140      continue
         memi(memi(ffp+18) ) = memi(memi(ffp+18) ) + 1
         memi(newfp+18) = memi(ffp+18)
         if (.not.(mode .eq. 4)) goto 150
            call xfseek(newfd, -2)
            if (xerflg) goto 100
150      continue
         reopen = (newfd)
         goto 100
100      return
      end
c     nextdv  next_dev
c     boffst  boffset
c     redird  redir_fd
c     pathne  pathname
