.RP
.ND
.TL
\s+3IRAF FILTERPHOT PACKAGE\s-3
.sp
All-Sky Reduction Equations
.br
Differential Reduction Equations
.br
Methods of Coefficient Calculation
.AU
Paul C. Schmidtke
.AI
.K2
.TU
.sp
July 15, 1983
.br
Last Revision: November 14, 1983
.AB
.PP
This paper discusses the IRAF Filterphot Package.
Topics include the standard reduction equations
(both all-sky and differential)
and the methods used to calculate the reduction coefficients.
Specific examples for common photometric systems are presented.
.AE
.TL
IRAF FILTERPHOT PACKAGE
.sp 2
.NH 
All-Sky Reduction Equations
.PP
The standard equations for photometric reduction of all-sky data in the
IRAF Filterphot Package are:
.sp
.EQ
delim @@
define = '~"="~'
define + '~"+"~'
define - '~"-"~'
define x '^\(bu^'
define Qi 'Q sub i'
define Qn 'Q sub n'
define Qs 'Q sub s'
define Ci 'C sub i'
define Cs 'C sub s'
define k1 'kappa sub 1'
define k2 'kappa sub 2'
.EN
.EQ I (1)
Qn = Qi - k1 x X - k2 x X x Ci
.EN
.EQ I (2)
Qs = eta x Qn + xi x Cs + zeta
.EN
where
.IP
@Qi@, @Qn@, and @Qs@ are the instrumental, natural,
and standard system magnitudes of a particular photometric quantity.
This quantity can be the result of a measurement through a single filter
(i.e. a magnitude),
the ratio of measurements through two filters
(i.e. a color index),
or a combination of measurements through three or more filters
(i.e. a complex index).
.IP
@Ci@ and @Cs@ are the instrumental and standard system magnitudes
of another photometric quantity, usually a color index.
@Ci@ may refer to the same quantity as @Qi@,
but @Cs@ cannot refer to the quantity as @Qs@.
Examples are given the in next section.
.IP
@X@ is the air mass of a observation.
.IP
@k1@ and @k2@ are the first- and second-order extinction coefficients,
respectively.
.IP
@eta@, @xi@, and @zeta@ are the transformation coefficients
(@zeta@ is the nightly zero point).
.LP
Equation (1) is used to remove the effects of atmospheric extinction
from a data set while equation (2) is used to transform the
extinction-corrected data to a standard system of measurement.
For some photometric systems,
the transformation equation does not adequately
describe all available data.
In these cases, the data are usually split into several parts,
based upon the value of @Cs@, and
equation (2) is independently applied to all parts.
Collectively, equations (1) and (2) are referred to as the all-sky
reduction equations.
Differential reduction equations are discussed in a subsequent section.
.PP
The user must be aware that such simple mathematical relationships
are meaningless if they fail to model the physical phenomena
that cause measurements through one filter set (and atmosphere)
to deviate from measurements through a standard filter set (and atmosphere).
The adopted equations are consistent with conventional theories of
blackbody radiation passing through simple models of the Earth's atmosphere.
In practice, however, small deviations from theory ought to occur.
As a specific example,
consider the use of a below-the-atmosphere color term in equation (1).
Some authors argue that an above-the-atmosphere color is more appropriate
while Young (1967, A.J. \fI72\fP, 945)
advocates that a mean of the above- and
below-the-atmosphere colors should be used.
These suggestions are kindly ignored because they apply only to wide-band
photometric systems.
Inasmuch as the error introduced by the "wrong" second-order correction
is usually far less than the accuracy of transformation to the standard system
(i.e. the UBV system is defined only to the \(ap2% level),
the choice of reduction equation is based on the ease of application.
.NH
Examples of All-Sky Reduction Equations
.PP
In this section,
examples of reduction equations for all-sky data are given for specific
photometric systems.
The most prevalently used notation is illustrated for each application.
The reader should note that the equation for a given photometric quantity
need not contain all the terms or coefficients that
are present in equations (1) and (2).
It is often the case that some extinction or transformation coefficients
are set to the value zero (0) or one (1).
.NH 2
UBV Photometry
.PP
The notation is adopted from Hardie
(1962, \fIBasic Astronomical Techniques\fP, p. 178).
The extinction equations are:
.EQ I (3.a)
v sub 0 = v - k prime sub v x X
- k prime prime sub v x X x (b-v)
.EN
.EQ I (3.b)
(b-v) sub 0 = (b-v) - k prime sub bv x X
- k prime prime sub by x X x (b-v)
.EN
.EQ I (3.c)
(u-b) sub 0 = (u-b) - k prime sub ub x X
.EN
The transformation equations are:
.EQ I (4.a)
V = v sub 0 + epsilon x (B-V) + zeta sub v
.EN
.EQ I (4.b)
(B-V) = mu x (b-v) sub 0 + zeta sub bv
.EN
.EQ I (4.c)
(U-B) = psi x (u-b) sub 0 + zeta sub ub
.EN
These sets of extinction and transformation equations are
readily expanded to include the additional colors in
UBVR or UBVRI photometry.
.NH 2
uvby Photometry
.PP
The notation is from Crawford and Barnes
(1970, A.J. \fI75\fP, 978).
The extinction equations are:
.EQ I (5.a)
y sub obs = y prime - K x X
.EN
.EQ I (5.b)
(b-y) sub obs = (b-y) prime - K sub 1 x X
.EN
.EQ I (5.c)
m sub 1 sub obs = m sub 1 prime - K sub 2 x X
.EN
.EQ I (5.d)
c sub 1 sub obs = c sub 1 prime - K sub 3 x X
.EN
The transformation equations are:
.EQ I (6.a)
V = y sub obs + B x (b-y) + A
.EN
.EQ I (6.b)
(b-y) = D x (b-y) sub obs + C
.EN
.EQ I (6.c)
m sub 1 = F x m sub 1 sub obs + J x (b-y) + E
.EN
.EQ I (6.d)
c sub 1 = H x c sub 1 sub obs + I x (b-y) + G
.EN
.NH 2
H\(*b Photometry
.PP
The notation is from Crawford and Mander
(1966, A.J. \fI71\fP, 114).
There are no extinction corrections because the H\(*b index is a ratio
of measurements (through filters of different widths) at the same
effective wavelength,
i.e. in the instrumental system
@beta prime prime = -2.5 x log sub 10 ~
left [ { narrow ~ filter } over { wide ~ filter } right ]@.
However, a night correction @NC@ can be applied that
transforms the instrumental system ratio to a natural system ratio.
The correction is found by calculating the mean difference
between known (or adopted) natural system
values @beta prime@ and the instrumental values @beta prime prime@
for observations of standard stars.
That is, @NC = < beta prime - beta prime >@.
All observations are then corrected by this amount.
The transformation equation is:
.EQ I (7)
beta = B x beta prime + A
.EN
If the definition of @beta prime@ is substituted into equation (7),
the following relationship holds:
.EQ I (8)
beta = B x beta prime prime + ( B x NC + A )
.EN
which is functionally equivalent to equation (7).
Why then resort to the use of night corrections?
It's only use is to segregate the instrumental measurements into
smaller, hopefully more uniform, groups of data.
Perhaps the instrumental gain changes more for one filter's measurements
than for the other's during a night.
Or, perhaps there is a small change in the effective wavelength
of the narrower filter relative to the wider filter as a function of
temperature during a night.
Regardless of cause,
the effect is as readily apparent in the residuals of @beta@ as it is
in the residuals of @beta prime@.
Because the segregation can be performed at any stage of processing,
the use of night corrections for H\(*b photometry is ignored.
.NH 2
Simple Systems
.PP
Simple systems require only two coefficients for
the reduction of each photometric quantity
(which is usually just a magnitude as opposed to a color):
an extinction coefficient and a transformation coefficient.
Examples of simple systems are
the Wing 8-color system and nearly all photometric systems using
data from photoelectric scanners.
For each quantity, the extinction equation is:
.EQ I (9)
M sub n = M sub i - A x X
.EN
For each quantity, the transformation equation is:
.EQ I (10)
M sub s = M sub n + Z
.EN
.NH
Methods of Reduction Coefficient Calculation
.PP
With the adoption of a standardized set of reduction equations,
it is possible to define a uniform technique for the calculation of
extinction and transformation coefficients.
Heeding the warnings of Popper
(1982, Pub. A.S.P. \fI94\fP, 204),
the suggestion
(Harris et al. 1981, Pub. A.S.P. \fI93\fP, 507)
to combine the calculation of all coefficients into one least-squares
solution is, at present, ignored.
Instead, separate solutions are advocated so as not to hide
systematic effects in extinction within the transformation coefficients,
and vise-versa.
.NH 2
Calculation of Extinction Coefficients
.PP
The calculation of extinction coefficients is often divided
into separate determinations of first-order
and second-order (i.e. color-dependent) coefficients.
.NH 3
Calculation of Second-Order Extinction Coefficients
.PP
It is essential that stars of greatly different color be observed
expressly for the purpose of determining second-order extinction coefficients.
Using the classical method (Hardie
1962, \fIBasic Astronomical Techniques\fP, p. 178),
a close pair of stars
of substantially different color (i.e. one blue and the other red)
is observed several times during a night.
Differential natural system magnitudes are described by the equation:
.EQ I (11)
DELTA Qn = DELTA Qi - k1 x DELTA X - k2 x X bar x DELTA Ci
.EN
which is derived from equation (1) by subtracting the relationship
for one star from the other.
The order of subtraction is not important, but it must be consistent.
Note the use of  @X bar@ in equation (11).
In most cases, the absolute value of the term involving @k2@
is larger than the term involving @k1@,
so that the @k1@ term can be ignored.
If this is the case (the user must take the responsibility
to verify that it is), then rearranging equation (11) yields:
.EQ I (12)
DELTA Qi = k2 x X bar x DELTA Ci + DELTA Qn
.EN
which is a linear equation with two unknowns
that can calculated with least-squares techniques.
Therefore, two differential measurements are needed to determine @k2@,
and three are needed if an error estimate is desired.
.NH 3
Calculation of First-Order Extinction Coefficients
.PP
Schmidtke (1982, International Amateur-Professional
Photoelectric Photometry Comm. No. \fI9\fP, p. 95) has demonstrated
three unique methods for calculating the extinction
coefficients in simple systems:
weighted-average, standard-star, and global.
All three methods, however, can be generalized to include the color term
of equation (1).
.NH 4
Weighted-Average Method
.PP
Equation (1) can be rearranged so that:
.EQ I (13)
left [ Qi - k2 x X x Ci right ] = k1 x X + Qn
.EN
which is a linear equation with two unknowns if
one of the following conditions is met:
.DS
@k2@ is zero (0) by definition
@k2@ is assumed to be zero and the range of @Ci@ is restricted
@k2@ is known from previous calculations
.DE
In each case,
the left side of equation (13) is now defined, and the value of
@k1@ and its error estimate can be found by least-squares techniques.
The adopted value of @k1@ is a weighted-average of separate
determinations for each star with multiple measurements.
There is no restriction on the nature of the stars used to calculate @k1@,
other than the lack of variability during a night.
They can be standard, program, or (intentionally designated) extinction
stars.
Various weighting schemes have been proposed.
Inasmuch as the purpose of such schemes is to weight the good
determinations of @k1@ at the expense of the poor,
it is suggested that the weight of each determination
be the product of the number of observations
times the range of airmass for that star.
With this technique,
a few observations over a large air mass are equivalent to
many observations over a small air mass.
.PP
A simultaneous solution of @k1@ and @k2@ is possible by rearranging
equation (13) so that @k2@ is on the right side of the equation.
The resultant linear equation contains three unknowns,
which can be solved by least-squares techniques.
The use of this variation in calculation, however, is
likely to troublesome.
The range of @Ci@ is small for an individual star,
yielding a poor estimate for @k2@.
As a result, the determination of @k1@ is also degraded.
.NH 4
Standard-Star Method
.PP
The standard-star method utilizes known values of @Qs@ for standard stars
at the expense of introducing coefficients from the transformation equation.
Equations (1) and (2) can be combined and rearranged so that:
.EQ I (14)
left [ Qi - { Qs over eta } - k2 x X x Ci + { xi x Cs over eta } right ] =
k1 x X - { zeta over eta }
.EN
which is a linear equation with two unknowns if
one of the following conditions is met:
.DS
@k2@ is zero (0) by definition
@k2@ is assumed to be zero and the range of @Ci@ is restricted
@k2@ is known from previous calculations
.DE
and if one of the following conditions is met:
.DS
@xi@ is zero (0) by definition
@xi@ is assumed to be zero and the range of @Cs@ is restricted
@xi@ is approximately known from previous calculations
.DE
and if one of the following conditions is met:
.DS
@eta@ is one (1) by definition
@eta@ is assumed to be one and the ranges of @Cs@ and @Qs@ are restricted
@eta@ is approximately known from previous calculations
.DE
In each combination of cases,
the left side of equation (14) is now defined, and the value of
@k1@ and its error estimate can be found by least-squares techniques.
Note that all observations of stars with known standard values
are used in a single determination of @k1@.
Unlike the weighed-average method,
there is no restriction on the number of observations of each star;
that is, a single observation of one star can be used with a single
observation of another.
The user must be aware, however, that errors
in the known standard values
can yield systematic errors in @k1@.
This method of calculation is frequently used for narrow-band
(i.e. simple) photometric systems.
.PP
A variation of the standard-star method of calculation is to
rearrange equation (14) so that the term involving @k2@ is on the
right side of the equation.
The resultant linear equation contains three equations, which
can be calculated using standard techniques.
Unlike a similar variation of the weighted-average method,
a simultaneous solution of @k1@ and @k2@ can produce
good results, provided there is a suitable range of standard star colors.
.NH 4
Global Method
.PP
The global method is a simultaneous least-squares solution using
all measurements of multiply-observed stars.
Let @delta Qn@ be the residual of a given observation from
its theoretical value, then:
.EQ I (15)
delta Qn = Qs - left [ eta x left [ Qi - k1 x X - k2 x X x Ci right ]
+ xi x Cs + zeta right ]
.EN
In the global method, values of the coefficients are sought
so that @sum ^ [ delta Qn ] sup 2@ for all observations be a minimum.
The following relationship must be satisfied:
.EQ I (16)
{ partial ^ sum ^ [ delta Qn ] sup 2 } over { partial ^ k1 } = 0
.EN
Equation (15) can substituted in equation (16), and the resultant
relationship expanded.
In the expansion, terms involving @Qs@ cancel each other.
The gory details of this process were first pointed out to
the author by D. S. Hayes;
they will not be reproduced here.
The value of @k1@ is given by:
.EQ I (17)
k1 = { sum from j=1 to J sum from k=1 to K sub j
^ alpha sub jk x X sub jk
- sum from j=1 to J ^ left [
{ 1 over K sub j }
x sum from k=1 to K sub j ^ alpha sub jk
x sum from k=1 to K sub j ^ X sub jk right ] } over
{ sum from j=1 to J sum from k=1 to K sub j
^ X sub jk sup 2
- sum from j=1 to J ^ left [
{ 1 over K sub j }
x left [ sum from k=1 to K sub j ^ X sub jk right ] sup 2 right ] }
.EN
where @alpha = Qi - k2 x X x Ci + xi x { Cs over eta }@.
The subscript @jk@ refers to the @k sup roman th@ observation
of the @j sup roman th@ star.
The number of extinction stars is @J@, and the number of observations of
a given star is @K sub j@.
The value of @k1@ and its error estimate, which is defined using terms
similar to those in equation (17),
can be calculated if one of the following conditions is met:
.DS
@k2@ is zero (0) by definition
@k2@ is assumed to be zero and the range of @Ci@ is restricted
@k2@ is known from previous calculations
.DE
and if one of the following conditions is met:
.DS
@xi@ is zero (0) by definition
@xi@ is assumed to be zero and the range of @Cs@ is restricted
@xi@ is approximately known from previous calculations
.DE
and if one of the following conditions is met:
.DS
@eta@ is one (1) by definition
@eta@ is assumed to be one and the range of @Ci@ is restricted
@eta@ is approximately known from previous calculations
.DE
These conditions are identical to those required by the
standard-star method,
except for the lack of restrictions on @Qs@.
The advantage of the global method compared to the standard-star method,
therefore, is the independence from standard star values,
provided @xi = 0@.
If @xi = 0@ and @eta = 1@, then the necessary conditions
to calculate @k1@ are identical to those for the weighed-average method.
The advantage of the global method compared to the weighted-average method
is a much more accurate determination of @k1@,
which has been demonstrated with synthetic data for simple photometric systems.
.PP
A simultaneous solution for both @k1@ and @k2@ using the global
method of calculation is, in theory, possible.
The equations, however, have not been written out in detail.
Work in this area is continuing.
.NH 2
Calculation of Transformation Coefficients
.PP
Extinction-corrected data can be transformed to a standard system
by means of equation (2).
Following common practice,
if a single equation does not adequately describe all data,
they can be subdivided into smaller segments so that
each portion can be modeled by equation (2).
The separation is usually based on the value of @Cs@.
At present, higher order terms are prohibited.
The calculation of coefficients @eta@, @xi@, and @zeta@
and their error estimates is a straightforward least-squares problem.
From the examples given previously, the user should note that
some photometric systems require the determination
of only one or two coefficients, not all three.
The legitimate combinations are @zeta@ alone (i.e. @eta = 1@ and @xi = 0@),
@eta@ plus @zeta@ (i.e. @xi = 0@),
@xi@ plus @zeta@ (i.e. @eta = 1@),
and, of course, @eta@ plus @xi@ plus @zeta@.
In particular, the combination of @eta@ plus @xi@ is considered invalid
because a nightly zero point must be present in the transformation equation.
.NH
Differential Reduction Equations
.PP
As originally stated,
equations (1) and (2) are used for the reduction of all-sky photometry.
In differential photometry, however,
two stars are observed: a variable (i.e. program) star and
a comparison (i.e. a psuedo-standard) star.
They ought to be relatively close to each other in the sky,
have approximately the same photometric colors,
and be observed in rapid succession.
If these conditions are met,
it can be assumed that the atmospheric and instrumental conditions
effecting both stars are nearly identical
and that the accuracy of the results is increased by
measuring the variable star relative to the comparison.
That is, differential magnitudes between the two stars can be
measured more precisely than separate all-sky magnitudes for each star.
The differential reduction equations, found by subtracting the
all-sky relationship for one star from the other, are:
.EQ I (18)
DELTA Qn = DELTA Qi - k1 x DELTA X - k2 x X bar x DELTA Ci
.EN
.EQ I (19)
DELTA Qs = eta x DELTA Qn + xi x DELTA Cs
.EN
where the order of subtraction is not important,
but it must be consistent.
An equation similar to equation (18) can be used
in the determination of @k2@, as described in a previous section.
In that case, the stars (i.e. blue-red pairs) are sufficiently close
in the sky that the term @k1 x DELTA X@ can be ignored.
In the general case, however, the term must be included.
For differential photometry,
it is assumed that values for all reduction coefficients are
derived from all-sky photometry,
using data explicitly obtained for such calculations.
.PP
It is common to insert an additional star into the observing
sequence: a check (i.e. another psuedo-standard) star.
This star is also reduced differentially, relative to the designated
comparison star.
If the differential magnitudes are not constant
(from night to night or from observing run to observing run),
then one of these supposed-constant stars is in fact variable.
That is, the check star is used to check the constancy of the comparison star.
.NH
Examples of Differential Reduction Equations
.PP
In this section,
examples of differential reduction equations are given for
the UBV photometric system.
The notation is adopted from Hardie
(1962, \fIBasic Astronomical Techniques\fP, p. 178).
The differential extinction equations are:
.EQ I (20.a)
DELTA v sub 0 = DELTA v - k prime sub v x DELTA X
- k prime prime sub v x X bar x DELTA (b-v)
.EN
.EQ I (20.b)
DELTA (b-v) sub 0 = DELTA (b-v) - k prime sub bv x DELTA X
- k prime prime sub by x X bar x DELTA (b-v)
.EN
.EQ I (20.c)
DELTA (u-b) sub 0 = DELTA (u-b) - k prime sub ub x DELTA X
.EN
The transformation equations are:
.EQ I (21.a)
DELTA V = DELTA v sub 0 + epsilon x DELTA (B-V)
.EN
.EQ I (21.b)
DELTA (B-V) = mu x DELTA (b-v) sub 0
.EN
.EQ I (21.c)
DELTA (U-B) = psi x DELTA (u-b) sub 0
.EN
These sets of extinction and transformation equations are
readily expanded to include the additional colors in
UBVR or UBVRI differential photometry.

