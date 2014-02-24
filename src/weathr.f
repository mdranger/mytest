
c              Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine weathr(precip,prcstd,prcskw,mintmp,maxtmp)

      real      precip(12), prcstd(12), prcskw(12),
     $          mintmp(12), maxtmp(12)

c...Determines the current year values for precipitation and
c     temperature and next year values for precipitation for predicting
c     production potential.

      include 'chrvar.inc'
      include 'const.inc'
      include 'wth.inc'
      include 'comput.inc'
c	include 'param.inc'
      
c...Function declarations
      real      anorm, sknorm
      external  anorm, sknorm

c...Local variables
      integer   m, mintt
      real 	maxt, mint, temp,sensi,psensi
      character label*4

c....sensitivity test
c...for precip
c...commented by Liu 2004 9 17
c        psensi = 0.95*0.95
        psensi = 1.0
c...for temp
	sensi = 1.0 - 0.00

c...Current year precipitation, regardless of wthr type
      do 10 m = 1, MONTHS
        prcurr(m) = prcnxt(m)
10    continue

c...Weather option M uses the same weather data for each year
      if (wthr .eq. 'M') then
        do 20 m = 1, MONTHS
          prcurr(m) = psensi*precip(m)
          prcnxt(m) = psensi*precip(m)
20      continue
	goto 85
        return
      endif

c...Weather option S
      if (wthr .eq. 'S') then
c...If skewness value is 0.0, select precipitation values from
c     normal distribution
c   Else, use skewness value to generate precip values
	do 30 m = 1, MONTHS
          if (prcskw(m) .eq. 0.0) then
            prcnxt(m) = max(0.0, anorm(precip(m),prcstd(m)))
          else
            prcnxt(m) = max(0.0, 
     $                  sknorm(precip(m), prcstd(m), prcskw(m)))
          endif
        prcnxt(m) = prcnxt(m)*psensi
30      continue
	goto 85
	return
      endif

c...Weather options F and C, read from weather data file
40    format(a4,6x,12f7.0)


c...Current year temperatures

c...Minimum monthly temperatures at 2 meters (deg c)
      read(9,40) label, mintmp
      if (label .ne. wlabel(2)) then
        call faterr(label,wlabel(2))
      endif

c...Check for -99.99 missing values flag
      do 50 m = 1, MONTHS
        if (mintmp(m) .eq. -99.99) then
          mintmp(m) =  tmn2m(m)
        endif
        mintmp(m) = sensi*mintmp(m)
50    continue

c...Maximum monthly temperatures at 2 meters (deg c)
      read(9,40) label, maxtmp
      if (label .ne. wlabel(3)) then
        call faterr(label,wlabel(3))
      endif

c...Check for -99.99 missing values flag
      do 60 m = 1, MONTHS
        if (maxtmp(m) .eq. -99.99) then
          maxtmp(m) =  tmx2m(m)
        endif
        maxtmp(m) = sensi*maxtmp(m)
60    continue

c..calculate daily cumulative temperature assuming 30 days in a month
      do 65 m = 1, MONTHS
          meanTemp = ((mintmp(m) + maxtmp(m))/2-5.0) * 30
          if (meanTemp .le. 0) then
             cum_temp(m) = 0
          else
!              changed the original code according to Yiping Wu's comments
!              if (m .eq. 1) then
!             cum_temp(m) = meanTemp
!              endif
!             cum_temp(m) = cum_temp(m-1) + meanTemp

             if (m .eq. 1) then
             cum_temp(m) = meanTemp
             else
             cum_temp(m) = cum_temp(m-1) + meanTemp
             endif
          endif
65    continue
                                                                                                                  

c...Next year precipitation (cm/month)
70    read(9,40,end=90) label,prcnxt
      if (label .ne. wlabel(1)) then
        call faterr(label,wlabel(1))
      endif

c...Check for -99.99 and generate skewed values for ones found
c...if precip is negative then generate, Bug fixed by Liu Feb 2001
      do 80 m = 1, MONTHS
        if (prcnxt(m) .lt. 0.0) then
c        if (prcnxt(m) .eq. -99.99) then
          if (prcskw(m) .eq. 0.0) then
            prcnxt(m) = max(0.0, anorm(precip(m),prcstd(m)))
          else
            prcnxt(m) = max(0.0, 
     $                  sknorm(precip(m), prcstd(m), prcskw(m)))
          endif
        endif
       prcnxt(m) = psensi*prcnxt(m)
80    continue
 
c....  added by Liu, 10/99
c.... calculate the aplitude of annual temperature and phase shift 
c 	for soil temperature profile 

c......time when the maximum air temperature is reached
85	maxt = -100.0
	mint = 100.0
	maxtt = 0
	mintt = 15
	meant = 0.0
	amp = 0

	do 100 m=1, MONTHS
	  meant = meant + maxtmp(m) + mintmp(m)
	  temp = 0.5*(maxtmp(m) + mintmp(m))
!	  print *, "temp", m, maxtmp(m), mintmp(m),temp
	  
	  if (temp .gt. maxt) then
	     maxtt = m
	     maxt = temp
	  endif	  
	  if (temp .lt. mint) then
	     mintt = m
	     mint = temp
	  endif	  
100	continue
	meant = meant/24.0
	
	if (maxtt .eq. 12 .or. maxtt .eq. 1) then
	   maxtt = mintt + 3.0
	elseif (mintt .eq. 12 .or. mintt .eq. 1) then
	   maxtt = maxtt -3.0
	else
	   maxtt = 0.5*(mintt + maxtt)
	endif
	
c...assume soil temp amplitude is 75% air temp amplitude
	amp = 0.5*0.75*(maxt - mint) 
     
!       print *
!       print *, meant, maxtt, maxt, mintt, mint, amp, '  in weathr'
!       print * 
	
      return

c...End of file encountered; rewind and reuse.
90    rewind 9
      goto 70

      end
