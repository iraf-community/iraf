c function pbinom.f
c
c source
c   Bevington, page 31.
c
c purpose
c   evaluate binomial probability coefficient
c
c usage
c   result = pbinom (nobs, ntotal, prob)
c
c description of parameters
c   nobs   - number of items observed
c   ntotal - total number of items
c   prob   - probability of observing each item
c
c subroutines and function subprograms required
c   factor (n)
c      calculates n factorial for integers
c
      function pbinom (nobs,ntotal,prob)
1     notobs=ntotal-nobs
2     pbinom=factor(ntotal)/(factor(nobs)*factor(notobs))*
     *       (prob**nobs)*(1.-prob)**notobs
      return
      end
