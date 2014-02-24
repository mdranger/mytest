
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


C...GPDF.F

        real function gpdf(x,a,b,c,d) 

	real    x, a, b, c, d

c******************** flowlib ********************* 

c...(run-time sub-set of modaid, exclusive of modctl) 

c...Release 1.0  (first formal release of modaid) 

c       james m. vevea
c       natural resource ecology lab
c       colorado state university 
c       fort collins, colorado  80523 

c...This routine is functionally equivalent to the routine of the
c     same name, described in the publication: 

c       Some Graphs and their Functional Forms 
c       Technical Report No. 153 
c       William Parton and George Innis (1972) 
c       Natural Resource Ecology Lab.
c       Colorado State University
c       Fort collins, Colorado  80523

c...12/90 Corrected by McKeown - exponent on frac changed from d to c

 
c...Local variables
        real    frac

        if (b .lt. x) then
           b = x +5
        endif

        frac = (b-x) / (b-a) 
        gpdf = 0.
        if (frac .gt. 0.) then
          gpdf = exp(c/d * (1. - frac**d)) * (frac**c)
        else
c....add by zp to avoid the negative or extremem value        
          gpdf = 0
        endif

        return 
        end
