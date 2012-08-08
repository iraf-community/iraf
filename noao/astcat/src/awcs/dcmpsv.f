C 	This routine was copied from the stsdas$pkg/analysis/gasp/gasplib/
C       directory. See stsdas$copyright.stsdas for copyright restrictions.
C 
	subroutine dcmpsv (a,m,n,w,v)
	parameter (nmax=1000)
	real*8 a(m,n),w(n),v(n,n),rv1(nmax)
	real*8 c, g, f, h, s, y, z, x, scale, anorm

	g=0.0
	scale=0.0
	anorm=0.0
	do i=1,n
		l=i+1
		rv1(i)=scale*g
		g=0.0
		s=0.0
		scale=0.0
		if (i.le.m) then
			do k=i,m
				scale=scale+dabs(a(k,i))
			enddo
			if (scale.ne.0.0) then
				do k=i,m
					a(k,i)=a(k,i)/scale
					s=s+a(k,i)*a(k,i)
				enddo
				f=a(i,i)
				g=-dsign(dsqrt(s),f)
				h=f*g-s
				a(i,i)=f-g
				if (i.ne.n) then
					do j=l,n
						s=0.0
						do k=i,m
							s=s+a(k,i)*a(k,j)
						enddo
						f=s/h
						do k=i,m
							a(k,j)=a(k,j)+f*a(k,i)
						enddo
					enddo
				endif
				do k= i,m
					a(k,i)=scale*a(k,i)
				enddo
			endif
		endif
		w(i)=scale *g
		g=0.0
		s=0.0
		scale=0.0
		if ((i.le.m).and.(i.ne.n)) then
			do k=l,n
				scale=scale+dabs(a(i,k))
			enddo
			if (scale.ne.0.0) then
				do k=l,n
					a(i,k)=a(i,k)/scale
					s=s+a(i,k)*a(i,k)
				enddo
				f=a(i,l)
				g=-dsign(dsqrt(s),f)
				h=f*g-s
				a(i,l)=f-g
				do k=l,n
					rv1(k)=a(i,k)/h
				enddo
				if (i.ne.m) then
					do j=l,m
						s=0.0
						do k=l,n
							s=s+a(j,k)*a(i,k)
						enddo
						do k=l,n
							a(j,k)=a(j,k)+s*rv1(k)
						enddo
					enddo
				endif
				do k=l,n
					a(i,k)=scale*a(i,k)
				enddo
			endif
		endif
		anorm=dmax1(anorm,(dabs(w(i))+dabs(rv1(i))))
	enddo
	do i=n,1,-1
		if (i.lt.n) then
			if (g.ne.0.0) then
				do j=l,n
					v(j,i)=(a(i,j)/a(i,l))/g
				enddo
				do j=l,n
					s=0.0
					do k=l,n
						s=s+a(i,k)*v(k,j)
					enddo
					do k=l,n
						v(k,j)=v(k,j)+s*v(k,i)
					enddo
				enddo
			endif
			do j=l,n
				v(i,j)=0.0
				v(j,i)=0.0
			enddo
		endif
		v(i,i)=1.0
		g=rv1(i)
		l=i
	enddo
	do i=n,1,-1
		l=i+1
		g=w(i)
		if (i.lt.n) then
			do j=l,n
				a(i,j)=0.0
			enddo
		endif
		if (g.ne.0.0) then
			g=1.0/g
			if (i.ne.n) then
				do j=l,n
					s=0.0
					do k=l,m
						s=s+a(k,i)*a(k,j)
					enddo
					f=(s/a(i,i))*g
					do k=i,m
						a(k,j)=a(k,j)+f*a(k,i)
					enddo
				enddo
			endif
			do j=i,m
				a(j,i)=a(j,i)*g
			enddo
		else
			do j= i,m
				a(j,i)=0.0
			enddo
		endif
		a(i,i)=a(i,i)+1.0
	enddo
	do k=n,1,-1
		do its=1,30
			do l=k,1,-1
				nm=l-1
				if ((dabs(rv1(l))+anorm).eq.anorm)  go to 2
				if ((dabs(w(nm))+anorm).eq.anorm)  go to 1
			enddo
1			c=0.0
			s=1.0
			do i=l,k
				f=s*rv1(i)
				if ((dabs(f)+anorm).ne.anorm) then
					g=w(i)
					h=dsqrt(f*f+g*g)
					w(i)=h
					h=1.0/h
					c= (g*h)
					s=-(f*h)
					do j=1,m
						y=a(j,nm)
						z=a(j,i)
						a(j,nm)=(y*c)+(z*s)
						a(j,i)=-(y*s)+(z*c)
					enddo
				endif
			enddo
2			z=w(k)
			if (l.eq.k) then
				if (z.lt.0.0) then
					w(k)=-z
					do j=1,n
						v(j,k)=-v(j,k)
					enddo
				endif
				go to 3
			endif
			if (its.eq.30) pause 'nO CONVERGENCE IN 30 ITERATIONS'
			x=w(l)
			nm=k-1
			y=w(nm)
			g=rv1(nm)
			h=rv1(k)
			f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y)
			g=dsqrt(f*f+1.0)
			f=((x-z)*(x+z)+h*((y/(f+dsign(g,f)))-h))/x
			c=1.0
			s=1.0
			do j=l,nm
				i=j+1
				g=rv1(i)
				y=w(i)
				h=s*g
				g=c*g
				z=dsqrt(f*f+h*h)
				rv1(j)=z
				c=f/z
				s=h/z
				f= (x*c)+(g*s)
				g=-(x*s)+(g*c)
				h=y*s
				y=y*c
				do nm=1,n
					x=v(nm,j)
					z=v(nm,i)
					v(nm,j)= (x*c)+(z*s)
					v(nm,i)=-(x*s)+(z*c)
				enddo
				z=sqrt(f*f+h*h)
				w(j)=z
				if (z.ne.0.0) then
					z=1.0/z
					c=f*z
					s=h*z
				endif
				f= (c*g)+(s*y)
				x=-(s*g)+(c*y)
				do nm=1,m
					y=a(nm,j)
					z=a(nm,i)
					a(nm,j)= (y*c)+(z*s)
					a(nm,i)=-(y*s)+(z*c)
				enddo
			enddo
			rv1(l)=0.0
			rv1(k)=f
			w(k)=x
		enddo
3		continue
	enddo
	return
	end
