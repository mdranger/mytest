

c		Copyright 1993 Colorado State University
c			All Rights Reserved



       real function anorm (amean,stdev) 
c 
c  ******************** flowlib ********************* 
c 
c			(run-time sub-set of modaid, exclusive of modctl) 
c 
c			release 1.0  (first formal release of modaid) 
c 
c			james m. vevea
c			natural resource ecology lab
c			colorado state university 
c			fort collins, colorado	80523 
c
      include 'pi.inc'

c****   Local variables
	integer idummy
	real amean, rand, randu, stdev

	idummy = 1
	rand = randu(idummy)
	anorm = sqrt(-2. * alog(rand)) * cos(PI2 *
     1		randu(idummy)) * stdev + amean
	return 
	end
