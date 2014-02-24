!     
! File:   main.f
! Author: wyuan
!
! Created on November 19, 2008, 11:05 AM
!
!modify the input, use year and month as input
!the output should be the GPP of current month
!Fix the long line bug on line 94 by using extension line
!
      real function ecluegpp (year, month)
! Input parameters
      
      integer year, month

! new parameters for calculation      
      integer days
      real PI, mgpp
      integer i, inyear, inmonth, maxdays, jd
! days - total number of records in the input file, used by Wenping
!      double,parameter::PI = 3.14159267
!      integer,parameter::days = 594
! inyear - year of input data
! inmonth - month of the input data
! mgpp - monthly GPP value, it is the sum of the daily GPP
! maxdays - maximum days in a month, this is set to 31
! jd - change from real type to integer type
! le_in - measured latent heat at fluxtower site
! Rn - 
      real rh,vpd,rn,par_in,ta,lai_in,ndvi_in,les,lec,pardf,lwg,prec
      real tmin_open,tmin_close,mtmin,vpd_open,vpd_close,mvpd
      real s,rnc,rns,ele,ps,rcorr,fa,rr,psy,rtot,rc,cs,rs,ad,cp
      real es,ra,linshi,beta,lesp,fpar,maxlueest,wsest,tsest
      real prtot,c1,optest,topt,scale
      real gpp,gppest,le_in,leest,sh
      real delta,bet
      real mdry,mh2o,ph2o,xh2o,set
      real a0, a1,a2,a3,a4,a5,pwdmid,pwd,a6
      character*100 infile

      infile = 'ECLUE.txt'
 
!      fixfile(1:pathlen) = libpath
!      fixfile(pathlen+1:pathlen+9) = '\graz.100'

      PI = 3.14159267
      days = 594
!      open(10,file="D:\Temp\Zhengpeng\howland.txt",action="read")

      open(10,file=infile,action="read")
! skip the file header
      read(10,*)
!constants used for  calculation
      mdry = 28.9644
      mh2o = 18.01534
      a0 = 6.107799961
      a1 = 4.43651/10
      a2 = 1.4289/100
      a3 = 2.65/10000
      a4 = 3.03/1000000
      a5 = 2.03/100000000
      a6 = 6.1368/100000000

      prtot = 200
      c1 = 0.000413
      vpd_close = 2.52
      beta = 1.59

      maxlueest=2.43
!...Debug, for WCR site only, change this one from 29 to 22
!...Since the MMS site average temp is 12 and WCR site is only 5
!...WCR showed much less autocorrelation(0.6) than MMS site (0.85)
      optest = 29.0
      vpd_open = 0.65
      topt = 20

      ad = 1.2  !air density, kg/m3
      cp = 0.001 !dry air specific heat capacity MJ/kg/K
      fa = 0.0000000567  !fa is the Stefan-Boltzman constant = 5.67 x 10-8 Watts m-2 K-4
 

      mgpp = 0.0
      maxdays = 0

! Calculate day length for calculating Rn and Parof daytime
! Start the loop to read input file
!      do i = 1, days
       

!Start to read the meteorology data
       
      inyear = 1
      lai_in = 0
       do while (inyear .gt. 0)
!inyear, inmonth, jd - data year, month and Julian day
!gpp, le_in - observed flux GPP and Latent Heat
!rn
!par_in
       read(10,*) inyear,inmonth, jd,gpp,le_in,rn,par_in,ta,rh,vpd,ps,
     $  ndvi_in,lai_in 
!...       print *, inyear,inmonth, jd,gpp,le_in,rn,par_in,ta,rh,vpd,ps,
!...     $  ndvi_in,lai_in 
      
       if ( (inyear.eq. year) .and. (inmonth.eq. month)) then
!Start to calculate latent heat in order to simulate GPP
                mtmin = exp(-((ta-topt)/topt)*((ta-topt)/topt))
                if(mtmin>1) mtmin = 1
                if(mtmin<0.3) mtmin = 0.3
                rns = rn * exp(-0.7*lai_in)
                rnc = rn - rns
                rcorr = 1/((((273.15+ta)/293.15)**1.75)*(101.3/ps))
                rr = ad * cp/(4*fa/1000000*((ta+273)**3))
                if(rr<0.01) rr = 0.01
                es = 0.6108*exp((17.27*ta)/(ta+237.3)) !saturation vapor pressure K Pa
                s = 4099*es/((ta+237.3)*(ta+237.3))!the slope of Pa/K
                psy = 0.00065*ps
                if(prtot*rcorr<0.01) rtot = 0.01
                if(prtot*rcorr>0.01) rtot = prtot*rcorr
                rc = rtot
                ra = rc * rr/(rc + rr)
                if(ra<0.01) ra = 0.01
                lesp = (s*rns+86400*ad*cp*es*(1-rh)/ra)/(s+psy*rtot/ra)
                mvpd = (vpd_close-vpd)/(vpd_close-vpd_open)
                if (vpd < vpd_open) mvpd = 1
                if (vpd > vpd_close) mvpd = 0.1
                cs = c1 * mvpd * mtmin*lai_in
                rs = 1/cs
                linshi = psy*(1+rs/ra)
                lec = (s*rnc+86400*ad*cp*es*(1-rh)/ra)/(s+linshi)
                les = lesp * (rh**beta)
                leest = lec + les

! Start to calculate GPP using simulated LE (leest) or observed LE(le)
                fpar = 1.24 * ndvi_in - 0.168
                if(fpar < 0) fpar = 0
                wsest = leest/rn
!debug:  use observed LE instead of modeled LE
!                wsest = le_in/rn

                if(wsest<0) wsest = 0
        tsest = (ta-0)*(ta-40)/((ta-0)*(ta-40)-(ta-optest)*(ta-optest))
                if(tsest<0) tsest = 0
!...comparing the two limit factor and
!...make sure the lowest limitation is temp(tsest) or water (wsest)
                scale = wsest
                if(wsest>tsest) scale = tsest

                gppest = fpar * par_in * maxlueest * scale

! Add the info together
               maxdays = maxdays + 1
! Add a check here to make sure the days in  one month won't exceed 31
               if (maxdays .gt. 31 ) then
                 print *, "too many days in the input", inmonth, infile
                 stop
               endif
               mgpp = gppest + mgpp
! Use obs GPP instead
!               mgpp = gpp + mgpp
        endif
        
! Continue to read next line of data        
        enddo

      
! Saved the calculated GPP to the return value      
! to avoid the missing values in the data, 
      if (maxdays.eq.0) then
        ecluegpp = 0
      else
!...Make sure we have positive GPP
        if ( mgpp .gt. 0 ) then
            ecluegpp = mgpp / maxdays * 30
        else
            ecluegpp = 0
        endif
       
      endif

!      print *, "eclue", year, month, maxdays, ecluegpp, mgpp
! Close the file 
      close(10)
      
      END