! USGS EROS
! File:   wetland.inc
! Author: zli
! The variables to handle the wetland emission
! surfwater - is the depth of surface water in cm, the same unit as input prec
! floodtime - percentage of time in one month that the water covers the surface,
!             value is between 0 and 1
! ch4a - accumulator of CH4 through the time
! ch4vege - monthly CH4 emission from vegetation, this is related with NPP, may
!           have different values for different vege
! ch4soil - monthly CH4 emission from soil, only realted with soil characters
! n2oa - accumulator of N2O through the time, at this time, n2oa = VOLEXA+VOLGMA+VOLPLA
       common/wetland/ surfwater, floodtime, ch4a, n2oa, ch4vege, ch4soil
       real surfwater, floodtime, ch4a, n2oa, ch4vege, ch4soil

       save /wetland/


