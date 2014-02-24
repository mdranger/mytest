
c               EROS Data Center
c              All Rights Reserved


C...DECOM_P.F

      subroutine decom_p(dtm)

      real      dtm

c...Soil Organic Matter Decomposition                written by Liu, 10/99
c     Decompose SOM1, SOM2, and SOM3 for each soil layer except the top layer.
c         defac = decomposition factor based on water and
c                 temperature computed in prelim and in cycle
c         dtm = time step (dt/ntspm)
c     fract  =  fraction of som1c in each layer assuming is the same 
c		as in the top layer
c     lyrsoc = unlabeled and labeled C in soil microbes
c                     (som1ci(SOIL,1)+som1ci(SOIL,2))
c     eftextp      = effect of soil texture on the soil microbe
c                     decomposition rate (computed in prelim)
c     anerbp	  = impact of soil anaerobic conditions
c     dec3(SOIL)  = intrinsic decomposition rate of
c                     soil microbes
c     dec5      = intrinsic decomposition rate of som2
c     dec4      = intrinsic decomposition rate of som3 (?)
c     tosom1    =  dead coarse roots to som1
c     tosom2    =  dead coarse roots to som2

      include 'comput.inc'
      include 'const.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'zztim.inc'
      include 'prof.inc'
      include 'monprd.inc'
      include 'accu.inc'
      include 'timvar.inc'
      include 'dovars.inc'
      include 'parfs.inc'
	include 'seq.inc'
	include 'forrem.inc'


c...Local variables
      integer   i
      real      cfs1s2, cfs1s3, cfs2s1, cfs2s3, 
     $          cfs3s1, cleach, co2los(8), linten, 
     $          tcflow, 
     $		p1, p2, sts1, sts2, mes1, rtdh, rdeath
      real	tcflow1, tcflow2, left, mrtgrth, tosom1, tosom2
      
c....calculate root distribution
c	call root_fr()
	
c....calculate 1) abiotic decomsition factor as a function
c	of soil temperature and moisture
c 	and 2) anaerobic impact on decomposition

c	call defact()
c	 print *, time, dotrem, crootc, frootc, mfprd(FROOT), mfprd(CROOT)
c	if (time .gt. 1872) stop


	do 65 i = 2, MAXLYR
	       	    
c	print *, i, defacl(i), anerobl(i)
c....calculate growth and death
c....crop
c.....calculate C in biomass of roots due to monthly growth  
c.......mcprd(BELOW): monthly C below-ground (top layer) production (GROWTH.F)
	    
	       mrtgrth = mcprd(BELOW)/real(ntspm) * lyrrtfr(i)/lyrrtfr(1)
 	       livroot(i) = livroot(i) + mrtgrth
 	       npp = npp + mrtgrth
          
c....tree root growth in deep layers
c....calculation is scaled from the top layer: fine root growth afrcis, coarse root growth --- acrcis
c....fine root
c...the acclocation to coarse and fine root is different from the top layer according to Van Dam
c....Veldkamp (1997).
c mrtgrth = 0.95*(mfprd(CROOT)+mfprd(FROOT))/real(ntspm) * lyrrtfr(i)/lyrrtfr(1)
	       mrtgrth = mfprd(FROOT)/real(ntspm) * lyrrtfr(i)/lyrrtfr(1)
 	       livfroot(i) = livfroot(i) + mrtgrth
c....coarse root
!...	       mrtgrth = 0.05*(mfprd(CROOT)+mfprd(FROOT))/real(ntspm) * lyrrtfr(i)/lyrrtfr(1)
	       mrtgrth = mfprd(CROOT)/real(ntspm) * lyrrtfr(i)/lyrrtfr(1)
 	       livcroot(i) = livcroot(i) + mrtgrth

!	  print *, livcroot(i),livroot(i) , lyrrtfr(i)
c...root death.  They depend
c     on moisture in each layers.
c...This is representing the death of fine surface roots.  They depend
c     on moisture in the top soil layers, so use avh2o(1).
c	assuming root death rate is the same through the whole profile
c         rtdh = 1.0 - avh2o(1)/(deck5+avh2o(1))
c.................
c...crop/pasture
           rtdh = 1.0 - asmos(1)/(deck5+asmos(1))
           rdeath = rdr/real(ntspm) * rtdh 
           if (rdeath .gt. 0.95) then
             rdeath = 0.95
           endif
           if (dohrvt) then
             rdeath = 0.95
           endif
           

	   rdeath = rdeath * livroot(i)
           deadrt(i) = deadrt(i) + rdeath
   	   livroot(i) = livroot(i) - rdeath 
	   if (livroot(i) .lt. 0.0) then 
		livroot(i) = 0.0
	   endif
             	    
  	    
c   	 if (adep(i) .gt. 0.) then   
c  	    print 11, time, i, lyrrtfr(i), adep(i), mcprd(BELOW), 
c     $ 		livroot(i), deadrt(i), deadroot
c         endif
      
c11	format(f6.1, i5, 8f10.3)

c.......fine roots
	if (livfroot(i) .gt. 0.0) then
	if (dotrem .or. dofire(FORSYS)) then
	rdeath = livfroot(i)/real(ntspm) * fd(1)
	else
	rdeath = livfroot(i)/real(ntspm) * wooddr(FROOT)
	endif

           deadrt(i) = deadrt(i) + rdeath
   	   livfroot(i) = livfroot(i) - rdeath 
	if (livfroot(i) .lt. 0.0) livfroot(i) = 0.0
	endif

c.......death of coarse roots
	if (livcroot(i) .gt. 0.0) then
	if (dotrem .or. dofire(FORSYS)) then
	rdeath = livcroot(i)/real(ntspm) * fd(2)
	else
	rdeath = livcroot(i)/real(ntspm) * wooddr(CROOT)
	endif


           deadcrt(i) = deadcrt(i) + rdeath
   	   livcroot(i) = livcroot(i) - rdeath 
	if (livcroot(i) .lt. 0.0) livcroot(i) = 0.0
	endif

c*****************************coarse dead roots *******************
c to  som1 and som2 with co2 loss
c******************************************************************
	if (deadcrt(i).gt. 1.e-7 .and. adep(i) .gt. 0.) then
c..compute total c flow out of the dead coarse roots
		tcflow = deadcrt(i) * defacl(i)*decw3*
     $     exp(-pligst(SOIL)*wdlig(CROOT))*anerobl(i)*dtm
	

		deadcrt(i) = deadcrt(i) - tcflow
c	print *, i, defac, defacl(i), decw3, anerobl(i), dtm
c	print *, i, tcflow, deadcrt(i),pligst(SOIL), wdlig(CROOT)

c               Decompose to SOM2
c               -----------------------

c...gross c flow to som2
	tosom2 = tcflow*wdlig(CROOT)
c...respiration associated with decomposition to som2
	co2los(8) = tosom2*rsplig

c...net C flow to som2
	tosom2 = tosom2 - co2los(8)
c...c flow to som2
c	  lyrsocp(i,2) = lyrsocp(i,2) + tosom2 

c	print *, i,crootc, livcroot(i), wood3c, deadcrt(i) 
c	print *, i, defac, defacl(i), anerb, anerobl(i)


c               Decompose to SOM1
c               -----------------------

c...Gross C flow to som1 
          tosom1 = tcflow - tosom2 - co2los(8)

c...Respiration associated with decomposition to som1 
          co2los(7) = tosom1 * ps1co2(SOIL)

c...Net C flow to SOM1
          tosom1 = tosom1 - co2los(7)

c...C flow to som1
c	  lyrsocp(i,1) = lyrsocp(i,1) + tosom1 
c	print *, i, tosom1, tosom2, co2los(7)+co2los(8)
c	print *, crootc, livcroot(i),wood3c,deadcrt(i)

	else
           co2los(7) = 0.0
	   co2los(8) = 0.0
	   tosom1 = 0.0
	   tosom2 = 0.0
	endif


 
c*******************************STRUCTURAL C****************************
c  Soil structural C decomposes to SOM2 and SOM3 with CO2 loss and 
c  possible leaching of organics.
c***********************************************************************

      if ((deadrt(i)+deadcrt(i)) .gt. 1.e-7 .and. adep(i) .gt. 0.) then
      
          p1 = deadrt(i)*strucc(SOIL)/(strucc(SOIL)+metabc(SOIL))
          
          tcflow1 = p1* defacl(i) *dec1(SOIL) * 
     $       exp(-pligst(SOIL)*strlig(SOIL))
     $	         * anerobl(i) * dtm
     
c     	print *, defacl(i), anerobl(i)
c	print *, deadrt(i), tcflow, p1
	     
     	  if (tcflow1 .gt. p1) then
     	     tcflow1 = p1
     	  endif
     	  
c  Decompose structural C to SOM2
c...Gross C flow to som2 
	  sts2 = tcflow1 * strlig(SOIL)
	  
c...Respiration associated with decomposition to som2 
	  co2los(4) = sts2 * rsplig

c...Net C flow to SOM2
	  sts2 = sts2 - co2los(4) 
	  lyrsocp(i,2) = lyrsocp(i,2) + sts2

c Decompose structural C to SOM1
c...Gross C flow to som1 
	  sts1 = tcflow1 - sts2 - co2los(4)

c...Respiration associated with decomposition to som1 
	  co2los(5) = sts1 * ps1co2(SOIL)

c...Net C flow to SOM1
	  sts1 = sts1 - co2los(5)
	  
	  lyrsocp(i,1) = lyrsocp(i,1) + sts1
	  
     
!	print *, lyrsocp(i,1), sts1, sts2

c****************************METABOLIC C**********************************
c  Soil metabolic C decomposes to SOM2 and SOM3 with CO2 loss and 
c  possible leaching of organics.
c***********************************************************************

          p2 = deadrt(i) - tcflow1
      
          tcflow2 = p2* defacl(i) *dec2(SOIL)  
     $	         * anerobl(i) * dtm
     	  if (tcflow2 .gt. p2) then
     	     tcflow2 = p2
     	  endif
     
c...CO2 loss 
	  co2los(6) = tcflow2 * pmco2(SOIL)
	  
c Decompose metabolic C to SOM1
	  mes1 = tcflow2 - co2los(6)
	  lyrsocp(i,1) = lyrsocp(i,1) + mes1

c...update dead root pool
	  deadrt(i) = deadrt(i) - tcflow1 - tcflow2	  
     
      else
	co2los(4) = 0.0
	co2los(5) = 0.0
	co2los(6) = 0.0
	mes1 = 0.0
	sts1 = 0.0
	sts2 = 0.0
      endif

     
c*******************************SOM1************************************
c  Soil SOM1 decomposes to SOM2 and SOM3 with CO2 loss and 
c  possible leaching of organics.
c***********************************************************************

      if (lyrsocp(i,1) .gt. 1.e-07 .and. adep(i) .gt. 0.) then

c...Compute total C flow out of soil microbes 
          tcflow = lyrsocp(i, 1) * defacl(i) *dec3(SOIL) * 
     $             anerobl(i) * dtm
c     $             eftxtl(i) *anerobl(i) * dtm
     
     	  if (tcflow .gt. lyrsocp(i,1)) then
     	      tcflow = lyrsocp(i,1)
     	  endif

c=============================1
c...CO2 Loss - Compute and schedule respiration flows
          co2los(1) = tcflow * s1co2(i)
          
c==============================2
c...Decompose Soil SOM1 to SOM3
c     The fraction of tcflow that goes to SOM3 is a function of 
c     clay content (fps1s3 is computed in prelim).
        cfs1s3 = tcflow * s1s3(i) * (1.0 + animpt * 
     $             (1.0 -anerobl(i)))

	    left = tcflow - co2los(1) - cfs1s3
c=====================================3
c...Leaching of SOM1
c     This only occurs when the water flow out of water layer 

        if(amov(i) .gt. 0.0) then
        
c           linten = min(1.0-(omlech(3)-amov(2))/omlech(3), 1.0)
            linten = min(1.0-(omlech(3)-amov(i))/omlech(3), 1.0)
            cleach = tcflow * orglchl(i) * linten
	    if (cleach .gt. left) then
	        cleach = left
	    endif
	    lyrsocp(i, 1) = lyrsocp(i, 1) - cleach
	    
	    if (adep(i) .gt. 0.) then
		    lyrsocp(i+1, 1) = lyrsocp(i+1, 1) + cleach
	    else
	            lchout = lchout + cleach
	    endif
c...No leaching this time step
        else
            cleach = 0.
        endif

c=====================================3
c...Decompose Soil SOM1 to SOM2.
c     SOM2 gets what's left of tcflow.
              cfs1s2 = left - cleach

	else  
		cfs1s2 = 0.
		co2los(1) = 0.
		cfs1s3 = 0.
	endif
	
	
c**************************SOM2***************************************
c...SOM2 decomposes to soil SOM1 and SOM3 with CO2 loss
c*********************************************************************
      if (lyrsocp(i,2) .gt. 1.e-07) then

c...Compute total C flow out of SOM2C
          tcflow = lyrsocp(i,2) * defacl(i) * dec5 * anerobl(i) * 
     $        dtm
c     $       eftxtl(i)* dtm
c...where
c     dec5      = intrinsic decomposition rate of som2
c     cltfac(2) = cultivation factor for som2 (set in cycle)

     	  if (tcflow .gt. lyrsocp(i,2)) then
     	      tcflow = lyrsocp(i,2)
     	  endif
c...CO2 loss - Compute and schedule respiration flows
              co2los(2) = tcflow * p2co2

c...Rearranged the order of calculation.  There is a calculated
c     impact of clay on the decomposition of som2 to som3.  -rm 12/91
c...Decompose SOM2 to SOM3, SOM1 gets what's left of tcflow.
              cfs2s3 = tcflow * fps2s3 * (1.0 + animpt *
     $          (1.0 - anerobl(i)))

	   left = tcflow - co2los(2) - cfs2s3
c...Leaching of SOM2 is added by Liu, 10/99
c     This only occurs when the water flow out of water layer 

        if(amov(i) .gt. 0.0) then
            linten = min(1.0-(omlech(3)-amov(i))/omlech(3), 1.0)
            cleach = tcflow * orglchl(i) * linten
	    if (cleach .gt. left) then
	        cleach = left
	    endif
           
	    lyrsocp(i, 2) = lyrsocp(i, 2) - cleach
c......leach to the next layer	    
	    if (adep(i) .gt. 0.0) then
		    lyrsocp(i+1, 2) = lyrsocp(i+1, 2) + cleach
	    else
	            lchout = lchout + cleach
	    endif
c...No leaching this time step
        else
            cleach = 0.
        endif

c...Decompose SOM2 to SOM1 (what's left)
              cfs2s1 = left - cleach
              
              
        else
              cfs2s1 = 0.
              co2los(2) = 0.
              cfs2s3 = 0.
           
        endif


c******************************SOM3***********************************
c...SOM3 decomposes to soil SOM1 with CO2 loss.
c*********************************************************************

      if (lyrsocp(i,3) .gt. 1.e-07) then
      
      
c...Compute total C flow out of SOM3C
          tcflow = lyrsocp(i,3) * defacl(i) * dec4 * anerobl(i) * 
     $          dtm
c     $          eftxtl(i)* dtm
     	  if (tcflow .gt. lyrsocp(i,3)) then
     	      tcflow = lyrsocp(i,3)
     	  endif

c...CO2 loss 
              co2los(3) = tcflow * p3co2 * anerobl(i)


c...Leaching of SOM3.  Added by Liu, 10/99
c     This only occurs when the water flow out of water layer 

       	   left = tcflow - co2los(3)

        if(amov(i) .gt. 0.0) then
            linten = min(1.0-(omlech(3)-amov(i))/omlech(3), 1.0)
            cleach = tcflow * orglchl(i) * linten

	    if (cleach .gt. left) then
	        cleach = left
	    endif
        
	    lyrsocp(i, 3) = lyrsocp(i, 3) - cleach
c......leach to the next layer	    
	    if (adep(i) .gt. 0.0) then
		    lyrsocp(i+1, 3) = lyrsocp(i+1,3) + cleach
	    else
	            lchout = lchout + cleach
	    endif
c...No leaching this time step
        else
            cleach = 0.
        endif
        
c...Decompose SOM3 to soil SOM1
              cfs3s1 = left - cleach

c...End of SOM3 Decomposition
	 else
              cfs3s1 = 0.
              co2los(3) = 0.
	endif
              
      
c....update SOM1 for each layer (leaching has been updated previously)    
        lyrsocp(i, 1) = lyrsocp(i, 1) - co2los(1) 
     $		- cfs1s2 - cfs1s3 + cfs3s1 + cfs2s1   
     $		+ tosom1   
      
c....update SOM2 for each layer (leaching has been updated previously)    
        lyrsocp(i, 2) = lyrsocp(i, 2) - co2los(2) 
     $		- cfs2s1 - cfs2s3  + cfs1s2 
     $		+ tosom2   
       
!....update SOM3 for each layer (leaching has been updated previously)
        lyrsocp(i, 3) = lyrsocp(i, 3) - co2los(3) 
     $		- cfs3s1 + cfs1s3 + cfs2s3   
               
       co2ac = co2ac + co2los(1) + co2los(2) + co2los(3) 
     $              + co2los(4) + co2los(5) + co2los(6)
     $              + co2los(7) + co2los(8) 
     
!       lyrsoc(1) = somtc + strucc(SOIL) + metabc(SOIL)
!        lyrsoc(1) = somtc
!...Change this to exclude the dead litter C,zp, 2010.9.6
       lyrsoc(1) = somsc
!...each layer including dead fine root and coarse root     
       if (i .gt. 1) then
          lyrsoc(i) = lyrsocp(i,1)+lyrsocp(i,2)+lyrsocp(i,3)
!...Change this to exclude dead litter, zp, 2010.9.6
!     $          + deadrt(i) + deadcrt(i)
       endif
!       print *, "lyrsocp",i, lyrsocp(i, 1), lyrsocp(i,2),lyrsocp(i,3)

!      if (time .ge. 1985 .and. time .lt. 1986) then
!      print *, i, time,adep(i),mcprd(BELOW),lyrsocp(i,1),lyrsocp(i,2),lyrsocp(i,3)
!	if (time .gt. 1871.42) stop
!	endif
 
65    continue

      return
      end
