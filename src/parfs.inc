
c               Copyright 1993 Colorado State University
c                       All Rights Reserved
c   add flaginv as the inversion flag to decide the npp
c   inversion process in annacc.f 05/28/08

       common/parfs/basfct,btolai,ccefor(2,5,3),cerfor(3,5,3),
     - decid,decw1,decw2,decw3,
     - fcfrac(5,2),forgrw,forrtf(3),klai,laitop,ldrmlt,leafdr(12),
     - maxlai,maxldr,sapk,swold,
     - wdlig(5),wooddr(5),forage,steady,steadyage,
     - livwodc,curlivwodc,prevnpp,prefstgr
     -,prevfbrchc,prevrlwodc,prevcrootc
     -,curfbrchc,currlwodc,curcrootc,death2gr
     -,gpp_eco, opt_temp, pforcfac, pcropcfac,flaginv
     -,growthlen, fgrowthlen

       integer decid,forgrw,forage,steady,death2gr
     -,gpp_eco,opt_temp, flaginv

       real basfct,btolai,ccefor,cerfor,decw1,decw2,decw3
     -,fcfrac,forrtf,klai,laitop,ldrmlt,leafdr
     -,maxlai,maxldr,sapk,swold
     -,wdlig,wooddr,steadyage
     -,livwodc,curlivwodc,prevnpp,prefstgr
     -,prevfbrchc,prevrlwodc,prevcrootc
     -,curfbrchc,currlwodc,curcrootc
     -,  pforcfac, pcropcfac,growthlen, fgrowthlen

      save /parfs/
