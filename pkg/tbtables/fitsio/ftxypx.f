C------------------------------------------------------------------------------
        subroutine ftxypx(xpos,ypos,xref,yref,xrefpix,yrefpix,
     &                    xinc,yinc,rot,type,xpix,ypix,status)

C       Fortran version of worldpos.c -- WCS Algorithms from Classic AIPS
C       Translated by James Kent Blackburn -- HEASARC/GSFC/NASA -- November 1994
C       routine to determine accurate pixel coordinates from an RA and Dec
C       does: -SIN, -TAN, -ARC, -NCP, -GLS, -MER, -AIT projections
C       returns   0 = good, 
C               501 = angle too large for projection;
C               502 = bad values
C               503 = ???undocumented error - looks like an underflow???
C Input:
C dbl   xpos   x (RA) coordinate (deg)
C dbl   ypos   y (dec) coordinate (deg)
C dbl   xref    x reference coordinate value (deg)
C dbl   yref    y reference coordinate value (deg)
C dbl   xrefpix x reference pixel
C dbl   yrefpix y reference pixel
C dbl   xinc    x coordinate increment (deg)
C dbl   yinc    y coordinate increment (deg)
C dbl   rot     rotation (deg)  (from N through E)
C chr   type    projection type code e.g. "-SIN"
C Output:
C dbl   xpix    x pixel number  (RA or long without rotation)
C dbl   ypiy    y pixel number  (dec or lat without rotation)
C int   status  error status flag, zero

        integer           status
        double precision  xpos,ypos,xref,yref,xrefpix,yrefpix
        double precision  xinc,yinc,rot,xpix,ypix
        character*(*)     type
        integer           error1,error2,error3,error4
        parameter         (error1=501)
        parameter         (error2=502)
        parameter         (error3=503)
        parameter         (error4=504)
        double precision  dx,dy,dz,r,ra0,dec0,ra,dec
        double precision  coss,sins,dt,da,dd,sint,oldxpos
        double precision  l,m,geo1,geo2,geo3,sinr,cosr
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
C   *** 0 hour wrap around test
        oldxpos = xpos
        dt = (xpos - xref)
        if (dt .gt. +180) xpos = xpos - 360
        if (dt .lt. -180) xpos = xpos + 360
C   *** Default values - Linear
        dx = xpos - xref
        dy = ypos - yref
        dz = 0.0
C   *** Correct for rotation
        r = rot * cond2r
        cosr = dcos(r)
        sinr = dsin(r)
        dz = dx * cosr + dy * sinr
        dy = dy * cosr - dx * sinr
        dx = dz
C   *** Check axis increments - bail out if either 0
        if ((xinc .eq. 0.0) .or. (yinc .eq. 0.0)) then
          xpix = 0.0
          ypix = 0.0
          status = error2
          goto 30
        end if
        xpix = dx / xinc + xrefpix
        ypix = dy / yinc + yrefpix
C   *** Find type of coordinate transformation (0 is linear)
        itype = 0
        do 10 i = 1, 8
          if (ctypes(i) .eq. type) itype = i
  10    continue
C   *** Done if linear
        if (itype .eq. 0) goto 30
C   *** Non-Linear position
        ra0 = xref * cond2r
        dec0 = yref * cond2r
        ra = xpos * cond2r
        dec = ypos * cond2r
C   *** Compute directional cosine
        coss = dcos(dec)
        sins = dsin(dec)
        l = dsin(ra-ra0) * coss
        sint = sins * dsin(dec0) + coss * dcos(dec0) * dcos(ra-ra0)
C   *** Process by case
        if (itype .eq. 1) then
C   *** SINE from '-SIN' type
          if (sint .lt. 0.0) then 
            status = error1
            goto 30
          end if
          m = sins * dcos(dec0) - coss * dsin(dec0) * dcos(ra-ra0)
        else if (itype .eq. 2) then
C   *** TANGENT from '-TAN' type
          if (sint .le. 0.0) then 
            status = error1
            goto 30
          end if
          m = sins * dsin(dec0) + coss * dcos(dec0) * dcos(ra-ra0)
          l = l / m
          m = (sins*dcos(dec0) - coss*dsin(dec0)*dcos(ra-ra0)) / m
        else if (itype .eq. 3) then
C   *** Arc from '-ARC' type
          m = sins*dsin(dec0) + coss*dcos(dec0)*dcos(ra-ra0)
          if (m .lt. -1.0) m = -1.0
          if (m .gt. 1.0) m = 1.0
          m = dacos(m)
          if (m .ne. 0) then
            m = m / dsin(m)
          else
            m = 1.0
          end if
          l = l * m
          m = (sins*dcos(dec0) - coss*dsin(dec0)*dcos(ra-ra0)) * m
        else if (itype .eq. 4) then
C   *** North Celestial Pole from '-NCP' type
          if (dec0 .eq. 0.0) then
            status = error1
            goto 30
          else
            m = (dcos(dec0) - coss * dcos(ra-ra0)) / dsin(dec0)
          end if
        else if (itype .eq. 5) then
C   *** Global Sinusoid from '-GLS' type
          dt = ra - ra0
          if (dabs(dec) .gt. twopi/4.0) then 
            status = error1
            goto 30
          end if
          if (dabs(dec0) .gt. twopi/4.0) then 
            status = error1
            goto 30
          end if
          m = dec - dec0
          l = dt * coss
        else if (itype .eq. 6) then
C   *** Mercator from '-MER' type
          dt = yinc * cosr + xinc * sinr
          if (dt .eq. 0.0) dt = 1.0
          dy = (yref/2.0 + 45.0) * cond2r
          dx = dy + dt / 2.0 * cond2r
          dy = dlog(dtan(dy))
          dx = dlog(dtan (dx))
          geo2 = dt * cond2r / (dx - dy)
          geo3 = geo2 * dy
          geo1 = cos (yref * cond2r)
          if (geo1 .le. 0.0) geo1 = 1.0
          dt = ra - ra0
          l = geo1 * dt
          dt = dec / 2.0 + twopi / 8.0
          dt = dtan(dt)
          if (dt .lt. deps) then 
            status = error2
            goto 30
          end if
          m = geo2 * dlog(dt) - geo3
        else if (itype .eq. 7) then
C   *** Aitoff from '-AIT' type
          l = 0.0
          m = 0.0
          da = (ra - ra0) / 2.0
          if (dabs(da) .gt. twopi/4.0) then
            status = error1
            goto 30
          end if
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
          geo1 = dt*dsqrt((1.0+dcos(dy)*dcos(dt/2.0))/2.0)/dx
          geo3 = geo2 * dsin(dy) / dsqrt((1.0+dcos(dy))/2.0)
          dt = dsqrt ((1.0 + dcos(dec) * dcos(da))/2.0)
          if (dabs(dt) .lt. deps) then
            status = error3
            goto 30
          end if
          l = 2.0 * geo1 * dcos(dec) * dsin(da) / dt
          m = geo2 * dsin(dec) / dt - geo3
        else if (itype .eq. 8) then
C   *** Stereographic from '-STG' type
          da = ra - ra0
          if (dabs(dec) .gt. twopi/4.0) then
            status = error1
            goto 30
          end if
          dd = 1.0 + sins*dsin(dec0) + coss*dcos(dec0)*dcos(da)
          if (dabs(dd) .lt. deps) then 
            status = error1
            goto 30
          end if
          dd = 2.0 / dd
          l = l * dd
          m = dd * (sins*dcos(dec0) - coss*dsin(dec0)*dcos(da))
        else
C   *** Unsupported Projection
          status = error4
          goto 30
        end if
C   *** Convert back to degrees
        dx = l / cond2r
        dy = m / cond2r
C   *** Correct for rotation
        dz = dx * cosr + dy * sinr
        dy = dy * cosr - dx * sinr
        dx = dz
C   *** Convert to PIXELS ... yeah!
        xpix = dx / xinc + xrefpix
        ypix = dy / yinc + yrefpix
  30    continue
C   *** reset xpos to correct for in place modification
        xpos = oldxpos
        end
