
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


       common/param/afiel(10),amov(10),awilt(10),basef,bulkd,
     - co2ipr(2),co2ice(2,2,3),co2irs(2),co2itr(2),co2sys,co2tm(2),
     - drain0,epnfa(2),epnfs(2),falprc,
     - hpttr(12),htran(12),ivauto,labtyp,labyr,
     - maxtmp(12),mctemp,micosm,mintmp(12),
     - nelem,nlayer,nlaypg,ph,ppdf(4,2),prcskw(12),prcstd(12),prdx(3),
     - precip(12),psloss,socmov,pslsrb,rcelit(2,3),rces1(2,3),
     - rces2(3),rces3(3),remwsd,scale,grcurv,rtprof,
     - satmos(2),satmt,sirri,snfxmx(2),sorpmx,stormf,
     - strm5l,strm5u,swflag,totco2,trbasl,
     - agefact_k, agefact_a, agefact_b, agefact_c, agefact_d,
     - debug_flag, cropdrain, prdx_scalar, npp_input,
     - actlayers, drain_coefficient, tile_depth, root_flag
     
       integer falprc,ivauto,labtyp,labyr,micosm,nelem
     - ,nlayer,nlaypg,swflag,grcurv,debug_flag, npp_input
     -,actlayers, root_flag

       real afiel,amov,awilt,basef,bulkd
     - ,co2ipr,co2ice,co2irs,co2itr,co2sys,co2tm
     - ,drain0,epnfa,epnfs,scale, cropdrain
     - ,hpttr,htran,maxtmp,mctemp,mintmp
     - ,ph,ppdf,prcskw,prcstd,prdx,precip,psloss,socmov
     - ,pslsrb,rcelit,rces1,rces2,rces3,remwsd
     - ,satmos,satmt,sirri,snfxmx,sorpmx,stormf
     - ,strm5l,strm5u,totco2,trbasl, rtprof
     - ,agefact_k, agefact_a, agefact_b, agefact_c, agefact_d
     - ,prdx_scalar, drain_coefficient, tile_depth
      save /param/
