include <math.h>

# ME_ELLGEOM -- Given the semi-major axis, ratio of semi-minor to semi-major
# axes, and position angle, compute the parameters of the equation of the
# ellipse, where the ellipse is defined as A * X ** 2 + B * x * y +
# C * Y ** 2 - F = 0.

procedure me_ellgeom (a, ratio, theta, aa, bb, cc, ff)

real    a                       #I the semi-major axis
real    ratio                   #I the ratio of semi-minor to semi-major axes
real    theta                   #I the position angle of the major axis
real    aa                      #O the coefficient of x ** 2
real    bb                      #O the coefficient of x * y
real    cc                      #O the coefficient of y ** 2
real    ff                      #O the constant term

real    cost, sint, costsq, sintsq
real    asq, bsq

begin
        # Get the angles.
        cost = cos (DEGTORAD(theta))
        sint = sin (DEGTORAD(theta))
        costsq = cost ** 2
        sintsq = sint ** 2

        # Compute the parameters of the outer ellipse.
        asq = a ** 2
        bsq = (ratio * a) ** 2
        aa = bsq * costsq + asq * sintsq
        bb = 2.0 * (bsq - asq) * cost * sint
        cc = asq * costsq + bsq * sintsq
        ff = asq * bsq
end


# ME_RECTGEOM -- Construct a polygon representation of a rotated rectangle
# givev the half-width of the long axis, the ratio of the half-width of the
# short axis to the long axis, and the rotation angle.

procedure me_rectgeom (hwidth, ratio, theta, xout, yout)

real    hwidth          #I the half-width of the long axis of the rectangle
real    ratio           #I the ratio of short to long axes of the rectangle
real    theta           #I the rotation angle
real    xout[ARB]       #O the x coordinates of the output vertices
real    yout[ARB]       #O the y coordinates of the output vertices

real    cost, sint, x, y

begin
        cost = cos (DEGTORAD(theta))
        sint = sin (DEGTORAD(theta))
        x = hwidth
        y = ratio * x
        xout[1] = x * cost - y * sint
        yout[1] = x * sint + y * cost
        x = -x
        y = y
        xout[2] = x * cost - y * sint
        yout[2] = x * sint + y * cost
        x = x
        y = -y
        xout[3] = x * cost - y * sint
        yout[3] = x * sint + y * cost
        x = -x 
        y = y
        xout[4] = x * cost - y * sint
        yout[4] = x * sint + y * cost
end

