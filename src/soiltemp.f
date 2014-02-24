
c               EROS Data Center
c		Shuguang Liu, 11/99


	subroutine soiltemp()



        include 'chrvar.inc'
        include 'comput.inc'
        include 'const.inc'
        include 'param.inc'
        include 'parfx.inc'
        include 'plot1.inc'
	include 'plot2.inc'
        include 'zztim.inc'
        include 'prof.inc'
        include 'timvar.inc'
        include 'wth.inc'
	include 't0par.inc'


c...Local variables
        integer   i, time_lag
        real      depth
        real 	wac, porosity, tc, hc, rf, dd, st0
        
c...... calculate soil temperature profile based on soil temperature
c 	in the top layer as predicted by the CENTURY (stemp)
      if ( debug_flag > 0) then
	open(unit=13, file='soilt.out',status='UNKNOWN')
      endif
      
	depth = 0.0
        st0 = meant
c	print *, "soil temperature"
!	print *, "i, month, time_lag,meant, amp, stemp, st(i),adep(i)"
	do 50 i=1, MAXLYR
	   if (adep(i) .gt. 0.) then
	   depth = depth + adep(i)
	   
!........tc: thermal conductivity (Yan and Arp, 1993) (stemp > 0)
!...	wac: volumetric water content (cm/cm)
!... asmos       the soil water content of the ith soil layer(cm h2o)
c	joule -> cal (0.2388)
	   wac = asmos(i)/(0.01+adep(i))
	   porosity = 1.0 - lyrden(i)/2.65	
	   tc = 0.2388*(5.57*wac**0.8 + porosity)/
     $          (10.0**(2.34*(porosity + 0.5)))   
!...        write(*,12) lyrden(i),wac,porosity,adep(i)
12	format (5f15.5)        
        

c........hc: heat capacity (Hadas, 1979): hc = 0.48 solid + wc * 1 + 0.6 * SOC
c	specific density of SOC is 1.3 g/cm3 (Hiller, 1980)
c	therefore the specific density of SOC is 0.65 g/cm3
c          if (adep(i) .gt. 0.01) then
         if (tave. lt. 0) then
c.....for frozen soil, the hc is equal to water heat capacity 1.0
             hc = 1.0
         else
	   hc = 0.48*lyrden(i)/2.65 + wac + 
     $        0.6*lyrsoc(i)/(0.65*10000.0*(adep(i)+0.01))	   
         
         endif

c.......radial frequency (1.99*10-7 s-1)
	   rf = PI2/(365.0*24.0*3600.0)
	   
c......damping depth dd (cm)
	   dd = sqrt(2.0*tc/(hc*rf))
	   
c	   if (time .lt. 1955.0 .and. time .gt. 1950.) then
!...	     write (*, 15) tc, hc, rf, wac,adep(i), asmos(i)
15	format(12f12.2)	
c	   endif

	
c..... soil surface temperature predicted
c..... meant is the annual average temperature calculated in the 
c      weathr.f
c..... amp is the temperature amplitude of the year
	 if (i .eq. 1) then 
c..... check if the current month temperature is lower than 0
c..... if so, treat as snow event in the month
c..... using the different technique to calculate the surface
c..... temperature, 
c         print *, "ave temp", aint(time), month, tave         
         if (tave .le. 0 ) then
c..... use snow calculation and use the current month preciptaion 
c..... as snow depth
c..... The damping depth is using a constant 127, as table 12.3 of Hillel(1980)
         st0 = meant + amp * 0.5 * ( 1 - exp(-1 *precip(month)/280))
         else         
	   st0 = meant + amp/exp((depth-0.5*adep(i))/dd)*
     $          (sin(2.0*PI*(float(month)-maxtt)/12.0-
     $		(depth-0.5*adep(i))/dd))
         endif
       else
         st0 = st0
       endif
   
      	  surft(month) = st0
c      	    surft(month) = stemp
   	    st00(month) = st0 	   
    	  
c    	  print *, depth, adep(i), dd, PI2
    	  
	   time_lag = int(12.0*(depth-0.5*adep(i))/(dd*PI2))/30
     
      	   time_lag = month - time_lag
      	   
     	   if (time_lag .le. 0) then
     	      time_lag = time_lag + 12
     	   endif
   	   
c..... soil temperature profile 
c           print *, "soiltemp", time, month, dd, time_lag
          if (tave .le. 0 ) then
c..... soil under the snow cover using freezing
c..... component          
	    st(i) = meant + amp/
     $          exp((depth-0.5*adep(i))/dd)* 
     $          (sin(PI2*(float(month)-maxtt)/12.0-
     $		(depth-0.5*adep(i))/dd))     
          else
	    st(i) = meant + amp/
     $          exp((depth-0.5*adep(i))/dd)* 
     $          (sin(PI2*(float(month)-maxtt)/12.0-
     $		(depth-0.5*adep(i))/dd)) +
     $		(surft(time_lag)-st00(time_lag))/
     $		exp((depth-0.5*adep(i))/dd)
          endif
c....  finish calculate different layer soil temp
        endif
c	write(*,55) i, month, time_lag,meant, amp, stemp, st(i),adep(i)
c	write(*,55) i, time_lag, surft(month), surft(time_lag),
c     $           st00(time_lag),st00(month)
50	continue

55	format(3i8, 6f8.2)
        
c	   if (time .gt. real(tend)-20.0) then

      if ( debug_flag .eq. 2) then
	   write (13, 100) aint(time), month, 
     $       (maxtmp(month)+mintmp(month))/2.0,
c     $       meant, maxtmp(month),mintmp(month), 
     $       st(1), st(2), st(3), st(4), st(5), st(6)
c    $       depth-0.5*adep(i), dd, meant,
c     $       amp,st0
c     $       defacl(1),
c     $       defacl(3),
c     $       defacl(5),
c     $       anerobl(1),
c     $       anerobl(3),
c     $       anerobl(5)
        endif
c	  endif
        
100	format(f8.2, i5, 20f8.2)
	return
	end
