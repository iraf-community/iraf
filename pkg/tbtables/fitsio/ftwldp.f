C------------------------------------------------------------------------------
        subroutine ftwldp(xpix,ypix,xref,yref,xrefpix,yrefpix,
     &                    xinc,yinc,rot,type,xpos,ypos,status)

C       Fortran version of worldpos.c -- WCS Algorithms from Classic AIPS
C       Translated by James Kent Blackburn -- HEASARC/GSFC/NASA -- November 1994
C       routine to determine accurate position from pixel coordinates
C       does: -SIN, -TAN, -ARC, -NCP, -GLS, -MER, -AIT projections
C       returns   0 = good, 
C               501 = angle too large for projection;
C Input:
C dbl   xpix    x pixel number  (RA or long without rotation)
C dbl   ypiy    y pixel number  (dec or lat without rotation)
C dbl   xref    x reference coordinate value (deg)
C dbl   yref    y reference coordinate value (deg)
C dbl   xrefpix x reference pixel
C dbl   yrefpix y reference pixel
C dbl   xinc    x coordinate increment (deg)
C dbl   yinc    y coordinate increment (deg)
C dbl   rot     rotation (deg)  (from N through E)
C chr   type    projection type code e.g. "-SIN"
C Output:
C dbl    xpos   x (RA) coordinate (deg)
C dbl    ypos   y (dec) coordinate (deg)
C int   status  error status flag, zero 

        integer           status
        double precision  xpix,ypix,xref,yref,xrefpix,yrefpix
        double precision  xinc,yinc,rot,xpos,ypos
        character*(*)     type
        integer           error1,error4
        parameter         (error1=501)
        parameter         (error4=504)

        double precision  cosr,sinr,dx,dy,dz,temp
        double precision  sins,coss,dect,rat,dt,l,m,mg,da,dd,cos0,sin0
        double precision  dec0,ra0,decout,raout
        double precision  geo1,geo2,geo3
        double precision  cond2r
        parameter         (cond2r=1.745329252d-2)
        double precision  twopi,deps
        parameter         (twopi = 6.28318530717959)
        parameter         (deps = 1.0d-5)
        integer           i,itype
        character*4       ctypes(8)
        data ctypes/ '-SIN', '-TAN', '-ARC', '-NCP',
     &               '-GLS', '-MER', '-AIT', '-STG' /

        if (status .gt. 0) return
C   *** Offset from ref pixel
        dx = (xpix-xrefpix) * xinc
        dy = (ypix-yrefpix) * yinc
C   *** Take out rotation
        cosr = dcos(rot*cond2r)
        sinr = dsin(rot*cond2r)
        if (rot .ne. 0.0) then
          temp = dx * cosr - dy * sinr
          dy = dy * cosr + dx * sinr
          dx = temp
        end if
C   *** Find type of coordinate transformation (0 is linear)
        itype = 0
        do 10 i = 1, 8
          if (ctypes(i) .eq. type) itype = i
  10    continue
C   *** default, linear result for error return
        xpos = xref + dx
        ypos = yref + dy
C   *** Convert to radians
        ra0 = xref * cond2r
        dec0 = yref * cond2r
        l = dx * cond2r
        m = dy * cond2r
        sins = l*l + m*m
        decout = 0.0
        raout = 0.0
        cos0 = dcos(dec0)
        sin0 = dsin(dec0)
C   *** Process by case
        if (itype .eq. 0) then
C   *** LINEAR
          rat =  ra0 + l
          dect = dec0 + m
        else if (itype .eq. 1) then
C   *** SINE from '-SIN' type
          if (sins .gt. 1.0) then 
            status = error1
            goto 30
          end if
          coss = dsqrt(1.0 - sins)
          dt = sin0 * coss + cos0 * m
          if ((dt .gt. 1.0) .or. (dt .lt. -1.0)) then 
            status = error1
            goto 30
          end if
          dect = dasin(dt)
          rat = cos0 * coss - sin0 * m
          if ((rat .eq. 0.0) .and. (l .eq. 0.0)) then 
            status = error1
            goto 30
          end if
          rat = datan2 (l, rat) + ra0
        else if (itype .eq. 2) then
C   *** TANGENT from '-TAN' type
          if (sins .gt. 1.0) then 
            status = error1
            goto 30
          end if
          dect = cos0 - m * sin0
          if (dect .eq. 0.0) then 
            status = error1
            goto 30
          end if
          rat = ra0 + datan2(l, dect)
          dect = datan(dcos(rat-ra0) * (m * cos0 + sin0) / dect)
        else if (itype .eq. 3) then
C   *** Arc from '-ARC' type
          if (sins .ge. twopi * twopi / 4.0) then 
            status = error1
            goto 30
          end if
          sins = dsqrt(sins)
          coss = dcos(sins)
          if (sins .ne. 0.0) then
            sins = dsin(sins) / sins
          else
            sins = 1.0
          end if
          dt = m * cos0 * sins + sin0 * coss
          if ((dt .gt. 1.0) .or. (dt .lt. -1.0)) then 
            status = error1
            goto 30
          end if
          dect = dasin(dt)
          da = coss - dt * sin0
          dt = l * sins * cos0
          if ((da .eq. 0.0) .and. (dt .eq. 0.0)) then 
            status = error1
            goto 30
          end if
          rat = ra0 + datan2(dt, da)
        else if (itype .eq. 4) then
C   *** North Celestial Pole from '-NCP' type
          dect = cos0 - m * sin0
          if (dect .eq. 0.0) then 
            status = error1
            goto 30
          end if
          rat = ra0 + datan2(l, dect)
          dt = dcos(rat-ra0)
          if (dt .eq. 0.0) then 
            status = error1
            goto 30
          end if
          dect = dect / dt
          if ((dect .gt. 1.0) .or. (dect .lt. -1.0)) then 
            status = error1
            goto 30
          end if
          dect = dacos(dect)
          if (dec0 .lt. 0.0) dect = -dect
        else if (itype .eq. 5) then
C   *** Global Sinusoid from '-GLS' type
          dect = dec0 + m
          if (dabs(dect) .gt. twopi/4.0) then 
            status = error1
            goto 30
          end if
          coss = dcos(dect)
          if (dabs(l) .gt. twopi*coss/2.0) then 
            status = error1
            goto 30
          end if
          rat = ra0
          if (coss .gt. deps) rat = rat + l / coss
        else if (itype .eq. 6) then
C   *** Mercator from '-MER' type
          dt = yinc * cosr + xinc * sinr
          if (dt .eq. 0.0) dt = 1.0
          dy = (yref/2.0 + 45.0) * cond2r
          dx = dy + dt / 2.0 * cond2r
          dy = dlog(dtan(dy))
          dx = dlog(dtan(dx))
          geo2 = dt * cond2r / (dx - dy)
          geo3 = geo2 * dy
          geo1 = dcos(yref * cond2r)
          if (geo1 .le. 0.0) geo1 = 1.0
          rat = l / geo1 + ra0
          if (dabs(rat - ra0) .gt. twopi) then 
            status = error1
            goto 30
          end if
          dt = 0.0
          if (geo2 .ne. 0.0) dt = (m + geo3) / geo2
          dt = dexp(dt)
          dect = 2.0 * datan(dt) - twopi / 4.0
        else if (itype .eq. 7) then
C   *** Aitoff from '-AIT' type
          dt = yinc * cosr + xinc * sinr
          if (dt .eq. 0.0) dt = 1.0
          dt = dt * cond2r
          dy = yref * cond2r
          dx = dsin(dy+dt)/dsqrt((1.0+dcos(dy+dt))/2.0) -
     &         dsin(dy)/dsqrt((1.0+dcos(dy))/2.0)
          if (dx .eq. 0.0) dx = 1.0
          geo2 = dt / dx
          dt = xinc * cosr - yinc * sinr
          if (dt .eq. 0.0) dt = 1.0
          dt = dt * cond2r
          dx = 2.0 * dcos(dy) * dsin(dt/2.0)
          if (dx .eq. 0.0) dx = 1.0
          geo1 = dt * dsqrt((1.0+dcos(dy)*dcos(dt/2.0))/2.0) / dx
          geo3 = geo2 * dsin(dy) / dsqrt((1.0+dcos(dy))/2.0)
          rat = ra0
          dect = dec0
          if ((l .eq. 0.0) .and. (m .eq. 0.0)) goto 20
          dz = 4.0-l*l/(4.0*geo1*geo1)-((m+geo3)/geo2)*((m+geo3)/geo2)
          if ((dz .gt. 4.0) .or. (dz .lt. 2.0)) then 
            status = error1
            goto 30
          end if
          dz = 0.5 * dsqrt(dz)
          dd = (m+geo3) * dz / geo2
          if (dabs(dd) .gt. 1.0) then 
            status = error1
            goto 30
          end if
          dd = dasin(dd)
          if (dabs(dcos(dd)) .lt. deps) then 
            status = error1
            goto 30
          end if
          da = l * dz / (2.0 * geo1 * dcos(dd))
          if (dabs(da) .gt. 1.0) then 
            status = error1
            goto 30
          end if
          da = dasin(da)
          rat = ra0 + 2.0 * da
          dect = dd
        else if (itype .eq. 8) then
C   *** Stereographic from '-STG' type
          dz = (4.0 - sins) / (4.0 + sins)
          if (dabs(dz) .gt. 1.0) then 
            status = error1
            goto 30
          end if
          dect = dz * sin0 + m * cos0 * (1.0+dz) / 2.0
          if (dabs(dect) .gt. 1.0) then 
            status = error1
            goto 30
          end if
          dect = dasin(dect)
          rat = dcos(dect)
          if (dabs(rat) .lt. deps) then 
            status = error1
            goto 30
          end if
          rat = l * (1.0+dz) / (2.0 * rat)
          if (dabs(rat) .gt. 1.0) then 
            status = error1
            goto 30
          end if
          rat = dasin(rat)
          mg = 1.0 + dsin(dect)*sin0 + dcos(dect)*cos0*dcos(rat)
          if (dabs(mg) .lt. deps) then 
            status = error1
            goto 30
          end if
          mg = 2.0 * (dsin(dect)*cos0 - dcos(dect)*sin0*dcos(rat)) / mg
          if (dabs(mg-m) .gt. deps) rat = twopi/2.0 - rat
          rat = ra0 + rat
        else
C   *** Unsupported Projection
          status = error4
          goto 30
        end if
  20    continue
C   *** Return RA in range
        raout = rat
        decout = dect
        if (raout-ra0 .gt. twopi/2.0) raout = raout - twopi
        if (raout-ra0 .lt. -twopi/2.0) raout = raout + twopi
        if (raout .lt. 0.0) raout = raout + twopi
C   *** Correct units back to degrees 
        xpos = raout / cond2r
        ypos = decout / cond2r
  30    continue
        end
