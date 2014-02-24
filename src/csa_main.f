
c		Copyright 1993 Colorado State University
c			All Rights Reserved





c			DISCLAIMER
c
c	   Neither the Great Plains System Research Unit - USDA (GPSR) nor
c	Colorado State University (CSU) nor any of their employees make
c	any warranty or assumes any legal liability or responsibility for
c	the accuracy, completeness, or usefulness of any information,
c	apparatus, product, or process disclosed, or represents that its
c	use would not infringe privately owned rights.  Reference to any
c	special commercial products, process, or service by tradename,
c	trademark, manufacturer, or otherwise, does not necessarily
c	constitute or imply endorsement, recommendation, or favoring by  
c	the GPSR or CSU.  The views and opinions of the authors do not
c	necessarily state or reflect those of GPSR or CSU and shall not 
c	be used for advertising or product endorsement. 
c       Add outputflag to change the output format

      program main
      
c...Century Soil Organic Matter Model
c     Simulation of carbon, nitrogen, phosphorous, and sulfur cycling
c     As of Dec. 1991, uses a 1 month time step
c     Project - Soil Fertility in the Great Plains
c     Modeler - Bill Parton
c     Programmers - Vicki Kirchner, Becky McKeown, Laura Harding
c...Change this to exclude the dead litter C,zp, 2010.9.6
c...State variables and flows are grams/m2.

      include 't0par.inc'
      include 'timvar.inc'
      include 'zztim.inc'
      include 'prof.inc'
      include 'parfx.inc'
      include 'param.inc'
      include 'accu.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'const.inc'
      include 'comput.inc'
      include 'seq.inc'
      include 'wth.inc'
      
	integer 	i, jfd_case, ensem_case, outputflag
	real		a, c,b, biomc, ttt,d1,d2,d3
	
c...           (unit 1) = plot/print file used by modaid (unformatted)
c...'site'.100 (unit 7) = parameter values and initial values for 
c                           state variables; see subroutine sitein.
c...fix.100    (unit 8) = fixed parameter values values for 
c                           state variables; see subroutine fixin.
c...           (unit 9) = a file of weather data read in subroutines 
c                           wthini, weathr
c...c14data   (unit 10) = a data file specifying years in which labeled
c                         carbon is added via plant growth and what 
c                         fraction of the growth is labeled.

c...If you're getting floating point errors mentioned after you exit
c	Century, uncomment the following lines, recompile, run Century
c	in dbx with the 'catch FPE' option to find the offending code.
c	You can also run Century outside of dbx, in which case you will
c	get messages on your screen giving you an fpe error code (see
c	the Floating Point Programmer's Guide, p20) and a not-very-
c	useful-to-3rd-or-4th-generation-language-programmers location. 
c	The error handler 'mysigal' is a Fortran callable C routine 
c	written by MartinFowler; it can be replaced by any user written
c	handler or any of several library handlers, the most useful 
c	probably being SIGFPE_ABORT.  The calls to ieee_handler won't 
c	compile using poa's binaries.

c      external mysigal
c      ieeer=ieee_handler('set','invalid',SIGFPE_ABORT)
c      ieeer=ieee_handler('set','division',mysigal)
c      ieeer=ieee_handler('set','overflow',mysigal)
c      ieeer=ieee_handler('set','underflow',SIGFPE_ABORT)

c...You probably wont want to uncomment the following line; inexact
c     floating point signals occur all over the place.

c       ieeer=ieee_handler('set','inexact',mysigal)
c...flags for starting status and ending status recordings
       logical inFlag, outFlag

        ttt = 0.0 
        biomc = 0.0
! default is the single output
        outputflag = 1
           if ( debug_flag .eq. 2 ) then
 	 open(unit=20, file='profout.txt',status='UNKNOWN')
 	 write(20,*) 'time  depth(cm) SOC(gC/m2) slowC'
         endif

     

c.....btain startup information from user, do initializations based on
c     answers to Modaid questions
      inFlag = .FALSE.
      outFlag = .FALSE.
c....init the wood decomposition rate
      decp_wood1c = 0.0
      decp_wood2c = 0.0
      decp_wood2c = 0.0
      decp_times = 0

c...Obtain startup information from user, do initializations based on
c     answers to Modaid questions
      call detiv(inFlag, outFlag, jfd_case, ensem_case, outputflag)
      print *, 'JFD:  ', jfd_case
      print *, 'Ens:  ', ensem_case
      
c...Write out starting values and ini the month, zli 12/3/08
      month = int((time - int(time))*12)
      if (month.eq.0) then
        month = 12
      endif
!YYYY.00 is the previous year's Dec
!YYYY.08 is Jan
!YYYY.16 is Feb
      call wrtbin(jfd_case, ensem_case, time, outputflag)
!      call wrtstatus(jfd_case, ensem_case, time)
      
!...read previous status
      if (inFlag) then
         call readstatus(jfd_case,ensem_case)
         print *, "after read previous status: "
         print *, "time, somtc, acrcis(1), fsysc, prdxtree, prdxcrop"
         print *, time, somtc, acrcis(1), fsysc, prdxtree, prdxcrop
      endif
      
!=====================main loop of the program ====================
!...Update month
20    continue
      month = mod(month,12)+ 1
! It seems the month is late in output, 
! move this step to the back of time+dt

      

!....added the read in prdx scalar
      if (npp_input .eq. 2 ) then
      if (month .eq. 1) then
        call prdxin(int(time) - strtyr)
      endif
      endif

      
!...If time is greater than the ending time for the current block,
!     read the next block
      if ((abs(time - blktnd) .lt. (0.5 * dt)) .and.
     $   (abs(time - tend)   .gt. (0.5 * dt))) then
        call readblk()
      endif
      
!.. old position by CENTURY moved later
!...Perform annual tasks
!...At the first month, set the growth of crop to 0
!...zli 12/2008
      
      if (month .eq. 1) then
        call eachyr
      endif

!.....output some variables
      
	if (int(time) .eq. int(tend-1) .and. month .eq. 12) then 
!.....output if the debug flag is not 0
      if ( debug_flag .eq. 1 ) then
	  write (*, *)
	  write (*, *) 'Total SOC deposited/eorded:  ', 
     $              int(sclosa), ' g C/m2 over ',
     $             	  int(tend - strtyr0),' years'
	  print *, 'Total SOC leached:           ', 
     $	            int(lchout), ' g C/m2 over ',
     $             	  int(tend - strtyr0), ' years'
	  print *, 'Total SOC emitted from deep layers:           ', 
     $	            int(co2ac), ' g C/m2'
 	  print *, 'CO2 emitted from the top layer:    '
     	  print *, '     ast1', '     ast2',  '    amt1 ', 
     $             '     amt2', '     as11',  '    as21 ', 
     $             '     as2 ', '     as3 '  
	  
	  write(*, 30) int(ast1), int(ast2), int(amt1),int(amt2),  
     $      int(as11), int(as21),   int(as2),   int(as3)  
30	format(8i9)
      endif
	  
	  ttt = as3+as2+as21+as11+amt2+amt1+ast2+ast1
	
	if ( debug_flag .eq. 1 ) then
	  print *, '        total  ', int(ttt), ' g C/m2'
	  print *, 'NPP (gC/m2/yr)---------'
	  print *, 'long-term mean (npp/years):    ', 
     $              int(npp/(tend-strtyr0))
	  print *, 'year ', int(time), ' (cproda):       ',cproda
	  print *
	  print *, 'Harvest (gc/m2):  '
          print *, ' Grain: ', int(graina)
          print *, ' Straw: ', int(strawa)
     	  print *, ' total: ', int(graina+strawa)
	  print *, 'year ', int(time), ' (cgrain):       ',cgrain

          print *
      endif
      
	if (cursys .eq. CRPSYS) then
	   woodc = 0.0
	   frstc = 0.0
	   wood1c = 0.0
	   wood2c = 0.0
	   wood3c = 0.0
	endif
	a=0.0
	b = 0.0
	c=0.0
	do 195 i = 2, MAXLYR
	   if (adep(i) .gt. 0.) then
	      a = a +livcroot(i) 
	      b = b +livfroot(i) 
	      c = c + livroot(i) 
	   endif
195	continue
	
         biomc = strucc(SRFC) + metabc(SRFC) + som1c(SRFC)
     $         + aglivc +  stdedc + bglivc 
     $         + frstc + wood1c + wood2c + wood3c 
     
c-----------------------------------------------------	
c.....display some information
	if ( debug_flag .ne. 0 ) then
	print *, 'Forest (gC/m2)----------------'
	print *, 'leaves (rleavc) --------- ',int(rleavc)
     $     ,'  (',int(100*rleavc/(frstc+b+a)),')'
	print *, 'fine branch (fbrchc) ---- ', int(fbrchc)
     $     ,'  (',int(100*fbrchc/(frstc+b+a)),')'
	print *, 'large wood (rlwodc) --- ',int( rlwodc)
     $     ,'  (',int(100*rlwodc/(frstc+b+a)),')'
	print *, 'fine roots (frootc) ----- ', int(frootc+b)
     $     ,'  (',int(100*(frootc+b)/(frstc+b+a)),')'
	print *, 'coarse roots (frootc) --- ', int(crootc+a)
     $     ,'  (',int(100*(crootc+a)/(frstc+b+a)),')'
	print *, 'Total live (frstc) ------ ', int(frstc+b+a)
     $     ,'  (100)'
	print *, 'Total (fsysc) ----------- ', int(fsysc)
	print *
	print *, 'Crop/pasture (gC/m2)----------------'
	print *, 'aglivc ----- ',   aglivc
	print *, 'bglivc ----- ',  bglivc+c
	print *, 'stdedc ----- ',   stdedc
	print *
	print *, 'Soil Carbon (gC/2)'
	print *, 'strucc(SFRC) ----  ',   strucc(SRFC)
	print *, 'metabc(SFRC) ----  ',   metabc(SRFC)
	print *, 'som1c(SRFC) -----  ',   SOM1c(SRFC)
	print *, 'som1c(SOIL) -----  ',   SOM1c(SOIL)
	print *, 'som2c       -----  ',   som2c
	print *, 'som3c       -----  ',   som3c
	print *

	  print *, 'Total existing biomass C (excluding deep soils): '
     $    , int(biomc), ' g C/m2 '
          print *
      endif   
      endif

!...The main driver for the model; call decomposition, growth, etc.
!...clean up the monthly variables with NEE calculation
!...
      
      call simsom()
      
!...NEE calculation
!...fgpp - forest gross primary production
!...cgpp - crop gross primary production (including grass)
!...fresp - forest total respiration = forest maintaince resp + growth resp
!...cresp - crop total respiration = crop maintaince resp + growth resp
!...sresp - surface soil respiration, sum of all surface layers in simsom function
!...sdresp - deep soil respiration, 
      nee = fgpp + cgpp - fresp - cresp - sresp - sdresp
!      print *, "fgpp in main", time, fgpp, cgpp
!...Write out values
!...dtpl is the output time interval that reads from scheduling file      
      
      if ((tplt - time) .lt. (dt * 0.5)) then
!          print *, "write out",time,month, maxtmp(month), precip(month)
          call wrtbin(jfd_case, ensem_case, time, outputflag)
          tplt = time + dtpl
      endif
      
!...Update time 
      time = time + dt
      if (time .ge. -1.0e-07 .and. time .le. 1.0e-07) then
        time = 0.0
      endif
!Move the original month change steps here to avoid the problem
!of month late step
!            month = mod(month,12)+ 1
!...Perform annual tasks, change back to start from Jan by zp, 11/28/2008
!      if (month .eq. 12) then
!        call eachyr
!      endif

	a=0.0
	c=0.0
	d1 = 0.0
	d2=0.0
	d3=0.0
	
	lyrsoc(1) = somtc
	
!...write profile information
	if (month .eq. 12) then
	lyrsocp(1,1) = som1c(2)
	lyrsocp(1,2) = som2c
	lyrsocp(1,3) = som3c
	do 90 i = 1, MAXLYR
	   if (adep(i) .gt. 0.) then
	      a = a + adep(i)
	      c = c + lyrsoc(i)
		d1= d1 + lyrsocp(i,1)
		d2= d2 + lyrsocp(i,2)
		d3= d3 + lyrsocp(i,3)
c	      print 92, i, time, adep(i), lyrsoc(i)
c92	format (I6, 9f10.2)
	   endif
90	continue

        if ( debug_flag .eq. 2 ) then
	if (time .gt. real(tend)-300.0) then
	write (20, 95) time, a,c,d2,d3,co2ac
	endif
        endif


95	format(9f9.1)
	endif


!...Run for tend years

      if ((tend-time) .gt. (dt*.5)) then
        goto 20
      endif
      

c=========================end of the main loop ===================
!...Write out final values
      call wrtbin(jfd_case, ensem_case, time, outputflag)

       call wrtstatus(jfd_case, ensem_case, time)

c...write the final soil profile (depth), added by Liu, 10/99
!	lyrsoc(1) = somtc
	a = 0.
	c = 0.
	d1 = 0.0
	d2 = 0.0
	d3 = 0.0
	livroot(1) = bglivc
	livcroot(1) = crootc
	livfroot(1) = frootc
	if (livfroot(1) .lt. 0.0) livfroot(1) = 0.0
	if (livcroot(1) .lt. 0.0) livcroot(1) = 0.0
	deadrt(1) = strucc(SOIL)+metabc(SOIL)
	deadcrt(1) = wood3c 
	
!----------------------------------------------
	if ( debug_flag .ne. 0 ) then
	print *
	write (*,*) '########## Final Profile Info ############'
	print *
	print *, ' layer thkns(cm) BulkD   SOC   Sand   Clay  LiveRoot
     $    deadrt rootfr' 
	print * 
      endif
!----------------------------------------------
	
	do 100 i = 1, MAXLYR
	
	   if (adep(i) .gt. 0.) then
	      if ( debug_flag .ne. 0 ) then   
	      write(*, 200) i,adep(i),lyrden(i),lyrsoc(i),lyrsand(i),
     $			lyrclay(i), livroot(i)+livcroot(i)+livfroot(i),
     $			deadrt(i)+deadcrt(i),lyrrtfr(i)
            endif
	      a = a + adep(i)
	      c = c + lyrsoc(i) + livroot(i)
	      d1 = lyrsocp(i,1) + d1
	      d2 = lyrsocp(i,2) + d2
	      d3 = lyrsocp(i,3) + d3
	   endif
100	continue
	
	lyrsocp(1,1) = som1c(2)
	lyrsocp(1,2) = som2c
	lyrsocp(1,3) = som3c
!----------------------------------------------
      if ( debug_flag .ne. 0 ) then
	print *
	print 210, 'Total', a, c
	print *
	
	print *, 'Soil Pools .........................'
	print *, 'ID    Thick(cm) Fast  Slow  Passive'
	
	do 110 i = 1, MAXLYR
	   if (adep(i) .gt. 0.) then
	      write(*, 200) i,adep(i),lyrsocp(i,1),
     $          lyrsocp(i,2),lyrsocp(i,3)
	   endif
110	continue
	write (*,200)
c	      write(*, 200) i,a,d1,d2,d3
     
200	format(i4, 8f8.2)
210	format(a5, f8.2, f16.2)
      endif
!===============================================
	
!...Close data files

!...Close the weather file
      close(unit=9)
c...Close the c14data file
      close(unit=10)
c...Close the schedule file
      close(unit=15)
c...Close the profile output file
      if ( debug_flag .eq. 2 ) then
      close(unit=20)
      endif

c...Mark end of file
      endfile(unit=1)
 
c...Close binary file
      close(unit=1)
 
      STOP 'Execution success.'
      end
