
c               EROS Data Center
c		Shuguang Liu, 10/99


       subroutine defact()
     
        
        include 'prof.inc'
        include 'param.inc'
        include 'parfx.inc'
        include 'plot1.inc'
        include 'const.inc'
        include 'dovars.inc'
	include 'wth.inc'
	include 'timvar.inc'
	include 'zztim.inc'
	include 't0par.inc'

        real tcalc, irrigt, pevap
        external tcalc, irrigt, pevap

c...This function calculates 
c	1) the impact of soil temperature and moisture on decomposition
c	2) the impact of soil anerobic conditions on decomposition.  

c...Called From:decom_p.f

c...Declaration explanations:
c     aneref(3) - minimum impact
c     drain     - percentage of excess water lost by drainage


c...Local variables
      real      slope1, xh2o,tfunc, wfunc, rprpet, pet0, irract0
	real   h2o,lowanerb,ratio0, top
	real depth, deff(10),optwfps
	integer i, j
	
	if ( debug_flag > 0 ) then
        open(unit=14, file='defact.out',status='UNKNOWN')
      endif
	lowanerb = 0.1
	optwfps = 0.65
	j=0
	depth = 0.0
	do 10 i=1, MAXLYR
	   if (adep(i) .gt. 0) j = j + 1
10	continue
	
        do 50 i = 1, j
	  h2o = 0.0
c...Effect of temperature on decomposition

         tfunc = tcalc(st(i), teff)
      
	depth = depth + adep(i)

c...If irrigating, determine actual amount of irrigation, irract
c      if (doirri) then
c        irract0 = irrigt(month)
c      else
c        irract0 = 0
c      endif


c...Compute the ratio of precipitation to PET.
c   Added avh2o(3) to rprpet calculation  -rm 6/91


c...Effect of moisture on decomposition
c         rwcf = (asmos(i)/adep(i)-awilt(i))/(afiel(i)-awilt(i))
c	print *,rwcf(i)
         wfunc = 1.0/(1.0 + 4.0 * exp(-6.0*(rwcf(i))))

         if (wfunc .ge. 1.0) wfunc = 1.0
     
c...Combined effects of temperature and moisture
         defacl(i) = tfunc*wfunc

	defacl(i) = tfunc
         defacl(i) = tfunc*wfunc
c	defacl(i) = 1.0

c	  defacl(i) = min(tfunc,wfunc)
         
c	print *
c         print *, i, tfunc, st(i)
         
         if (defacl(i) .lt. 0.0) defacl(i) = 0.0
      
c...Calculate the effect impact of anerobic conditions on decomposition
c...xh2o is water-filled pore space here, if it is larger than 0.75
c...the soil environment starting to become anaerobic (Drury.., Firestone..
c...based on N2O and NO production
c...It is assumed that anerobl reduced to 0.40 under saturated conditions
c...because majority of the SOC was Slow (-90%) and Passive (-30%) (according to Gale and Gilmour, 1988
c.... Net mineralization of carbon and nitrogen under aerobic and anaerobic conditions,
c....SSSAJ 52: 1006-1010, if we assume that slow:passive is 1:1, then the reduction
c....would be 60%.
c....Linn and Doran, 1984, SSSAJ 48: 1267-1272, CO2 evolution reduced 60% as compared to the 
c.....optimal conditions
     
        h2o = rwcf(i)*(afiel(i)-awilt(i))+awilt(i)
c        xh2o = (rwcf(i)*(afiel(i)-awilt(i))+awilt(i))/
        xh2o = h2o/(1.0-lyrden(i)/2.65)
c	print *,i, xh2o, afiel(i), awilt(i), rwcf(i),'WFPS', h2o
        if (xh2o .gt. optwfps) then
	  slope1 = (1.0 - lowanerb)/(1.0-optwfps)
          anerobl(i) = 1.0 - slope1 * (xh2o - optwfps)
	  if (anerobl(i) .lt. lowanerb) anerobl(i)=lowanerb
	else
	  anerobl(i) = xh2o/optwfps
        endif

c	  anerobl(i) = anerobl(i) - 0.15 * lyrclay(i)  

c	anerobl(i) = anerobl(i) - 0.4*depth/100.0
	if (anerobl(i) .le. lowanerb) then
		anerobl(i) = lowanerb
	endif

	defacl(i) = defacl(i) * anerobl(i)

      anerobl(i) = min(1.0,1.0/
     $(1.0+2.0*(depth/100.0)**2.0)**0.75 + lowanerb)
	anerobl(i) = min(1.0,1.0*anerobl(i))

c	if (i .eq. 1) ratio0 = anerb/anerobl(1)
c	anerobl(i) = anerobl(i)*ratio0

c	defacl(i) = min(defacl(i),anerobl(i))

c	anerobl(i) = anerb 

c	write(*,44) i,time,tfunc, wfunc, defac, defacl(i),anerb, anerobl(i)
	deff(i) = defacl(i)*anerobl(i)

50	continue
        
        if ( debug_flag > 0 ) then
        if (time .gt. real(tend)-20.0) then
c           write (14, 45) time,deff
            write (14, 45) time,defacl
c           write (14, 45) time,anerobl
c           write (14, 45) time,afiel
c           write (14, 45) time,defacl,anerobl
	endif
	endif

44	format (1i6,10f8.2)
45	format (21f8.2)

      return
      end
