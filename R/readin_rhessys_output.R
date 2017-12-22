#' Pulls output from a single RHESSys run into R
#'
#' It does fanciness...
#'
#' @param pre file
#' @param b basin
#' @param c canopy
#' @param z basin
#' @param p canopy
#' @param h basin
#' @param g canopy
#' @param f fire
#' @param wy wateryear
#'
#'
#' @export
readin_rhessys_output = function(pre, b = 1, c = 0, z = 0, p = 0, h = 0, g = 0, f = 0, wy = 1)
{
  bd = NULL
  bdg = NULL
  cd = NULL
  cdg = NULL
  zd = NULL
  zdg = NULL
  pd = NULL
  pdg = NULL
  fd = NULL
  bd.wy = NULL
  bdg.wy = NULL
  cd.wy = NULL
  cdg.wy = NULL
  zd.wy = NULL
  zdg.wy = NULL
  pd.wy = NULL
  pdg.wy = NULL
  fd.wy = NULL
  bd.wyd = NULL
  bdg.wyd = NULL
  cd.wyd = NULL
  cdg.wyd = NULL
  zd.wyd = NULL
  zdg.wyd = NULL
  pd.wyd = NULL
  pdg.wyd = NULL
  fd.wyd = NULL
  hd = NULL
  hdg = NULL
  hd.wy = NULL
  hdg.wy = NULL
  hdg.wyd = NULL
  hd.wyd = NULL
  if (b == 1) {
    nme = sprintf("%s_basin.daily", pre)
    bd = read.table(nme, header = T)
    bd$et = bd$trans + bd$evap
    bd$unfilled_cap = bd$sat_def - bd$unsat_stor - bd$rz_storage
    bd = mkdate(bd)
    if (wy == 1) {
      bd.wy = aggregate(bd, by = list(bd$wy), mean)
      bd.wyd = aggregate(bd, by = list(bd$wyd), mean)
      tmp = aggregate(bd$snowpack, by = list(bd$wy), max)
      bd.wy$pkswe = tmp$x
    }
    if (g == 1) {
      nme = sprintf("%s_grow_basin.daily", pre)
      bdg = read.table(nme, header = T)
      bdg = mkdate(bdg)
      porosity = bd$sat_def/bd$sat_def_z
      if (nrow(bd)==nrow(bdg)){
        bd$veg_awc = ifelse((bdg$root_depth < bd$sat_def_z),
                            porosity * (bd$sat_def_z - bdg$root_depth) +
                              bd$rz_storage, bd$rz_storage)
      } else {
        print("bd and bdg have a mismatch number of rows. veg_awc not computed.")
      }
      if (wy == 1) {
        bdg.wy = aggregate(bdg, by = list(bdg$wy), mean)
        bdg.wyd = aggregate(bdg, by = list(bdg$wyd),
                            mean)
      }
    }
  }
  if (z == 1) {
    nme = sprintf("%s_zone.daily", pre)
    zd = read.table(nme, header = T)
    zd = mkdate(zd)
    if (wy == 1) {
      zd.wy = aggregate(zd, by = list(zd$wy), mean)
      zd.wyd = aggregate(zd, by = list(zd$wyd), mean)
    }
    if (g == 1) {
      nme = sprintf("%s_grow_zone.daily", pre)
      zdg = read.table(nme, header = T)
      zdg = mkdate(zdg)
      if (wy == 1) {
        zdg.wy = aggregate(zdg, by = list(zdg$wy), mean)
        zdg.wyd = aggregate(zdg, by = list(zdg$wyd),
                            mean)
      }
    }
  }
  if (h == 1) {
    nme = sprintf("%s_hillslope.daily", pre)
    hd = read.table(nme, header = T)
    hd = mkdate(hd)
    if (wy == 1) {
      hd.wy = aggregate(hd, by = list(hd$wy), mean)
      hd.wyd = aggregate(hd, by = list(hd$wyd), mean)
    }
    if (g == 1) {
      nme = sprintf("%s_grow_hillslope.daily", pre)
      hdg = read.table(nme, header = T)
      hdg = mkdate(hdg)
      if (wy == 1) {
        hdg.wy = aggregate(hdg, by = list(hdg$wy), mean)
        hdg.wyd = aggregate(hdg, by = list(hdg$wyd),
                            mean)
      }
    }
  }
  if (c == 1) {
    nme = sprintf("%s_stratum.daily", pre)
    cd = read.table(nme, header = T)
    nme = sprintf("%s_stratum.daily", pre)
    cd = mkdate(cd)
    if (wy == 1) {
      cd.wy = aggregate(cd, by = list(cd$wy), mean)
      cd.wyd = aggregate(cd, by = list(cd$wyd), mean)
    }
    if (g == 1) {
      nme = sprintf("%s_grow_stratum.daily", pre)
      cdg = read.table(nme, header = T)
      cdg = mkdate(cdg)
      cdg$woodc = cdg$live_stemc + cdg$dead_stemc + cdg$live_crootc +
        cdg$dead_crootc
      cdg$plantc = cdg$woodc + cdg$frootc + cdg$leafc
      if (wy == 1) {
        cdg.wy = aggregate(cdg, by = list(cdg$wy), mean)
        cdg.wyd = aggregate(cdg, by = list(cdg$wyd),
                            mean)
        tmp = aggregate(cdg$cpool, by = list(cdg$wy),
                        min)
        cdg.wy$mincpool = tmp$x
        cdg.wy$mincpoolp = tmp$x/cdg.wy$plantc * 100
      }
    }
  }
  if (p == 1) {
    nme = sprintf("%s_patch.daily", pre)
    pd = read.table(nme, header = T)
    pd = mkdate(pd)
    if (wy == 1) {
      pd.wy = aggregate(pd, by = list(pd$wy), mean)
      pd.wyd = aggregate(pd, by = list(pd$wyd), mean)
    }
    if (g == 1) {
      nme = sprintf("%s_grow_patch.daily", pre)
      pdg = read.table(nme, header = T)
      pdg = mkdate(pdg)
      if (wy == 1) {
        pdg.wy = aggregate(pdg, by = list(pdg$wy), mean)
        pdg.wyd = aggregate(pdg, by = list(pdg$wyd),
                            mean)
      }
    }
  }
  if (f == 1) {
    nme = sprintf("%s_fire.daily", pre)
    fd = read.table(nme, header = T)
    fd = mkdate(fd)
    if (wy == 1) {
      fd.wy = aggregate(fd, by = list(fd$wy), mean)
      fd.wyd = aggregate(fd, by = list(fd$wyd), mean)
    }
  }
  a = list(bd = bd, bdg = bdg, bd.wy = bd.wy, bdg.wy = bdg.wy, bd.wyd = bd.wyd,
           bdg.wyd = bdg.wyd, fd = fd, fd.wy = fd.wy, fd.wyd = fd.wyd,
           hd = hd, hdg = hdg, hd.wy = hd.wy, hdg.wy = hdg.wy, hd.wyd = hd.wyd,
           hdg.wyd = hdg.wyd, zd = zd, zdg = zdg, zd.wy = zd.wy, zdg.wy = zdg.wy,
           zd.wyd = zd.wyd, zdg.wyd = zdg.wyd, pd = pd, pdg = pdg, pd.wy = pd.wy,
           pdg.wy = pdg.wy, pd.wyd = pd.wyd, pdg.wyd = pdg.wyd, cd = cd,
           cdg = cdg, cd.wy = cd.wy, cdg.wy = cdg.wy, cd.wyd = cd.wyd,
           cdg.wyd = cdg.wyd)
  return(a)
}
