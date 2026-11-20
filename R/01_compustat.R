# Compustat - annual and quarterly

# annual
funda_db <- tbl(wrds, I("comp.funda"))

compustat <- funda_db %>%
  filter(indfmt == "INDL", datafmt == "STD", popsrc == "D", consol == "C") %>%
  filter(datadate >= start_date, datadate <= end_date) %>%
  select(
    # header
    gvkey, datadate, fyear,
    
    # income statement
    sale, revt, cogs, xsga, dp, xrd, xad, ib, ebitda, ebit, nopi, spi, pi,
    txp, ni, txfed, txfo, txt, xint,
    
    # cash flow etc
    capx, oancf, dvt, ob, gdwlia, gdwlip, gwo,
    
    # assets
    rect, act, che, ppegt, invt, at, aco, intan, ao, ppent, gdwl, fatb, fatl,
    
    # liabilities
    lct, dlc, dltt, lt, dm, dcvt, cshrc, dcpstk, pstk, ap, lco, lo,
    drc, drlt, txdi,
    
    # equity
    ceq, scstkc, emp, csho,
    
    # market
    prcc_f,
    
    # other
    dc, txdfed
  ) %>%
  collect()

compustat <- compustat %>%
  filter(
    !is.na(at),
    !is.na(prcc_f),
    !is.na(ni),
    datadate >= start_date
  ) %>%
  arrange(gvkey, datadate) %>%
  distinct(gvkey, datadate, .keep_all = TRUE) %>%
  group_by(gvkey) %>%
  mutate(
    count = row_number(),
    dr = case_when(
      !is.na(drc) & !is.na(drlt) ~ drc + drlt,
      !is.na(drc) & is.na(drlt) ~ drc,
      is.na(drc) & !is.na(drlt) ~ drlt,
      TRUE ~ NA_real_
    ),
    dc = case_when(
      is.na(dcvt) & !is.na(dcpstk) & !is.na(pstk) & dcpstk > pstk ~ dcpstk - pstk,
      is.na(dcvt) & !is.na(dcpstk) & is.na(pstk) ~ dcpstk,
      TRUE ~ dcvt
    ),
    xint0 = if_else(is.na(xint), 0, xint),
    xsga0 = if_else(is.na(xsga), 0, xsga)
  ) %>%
  ungroup()

# annual characteristics
# fiscal_lag = assume accounts are public 6 months after year end
compustat <- compustat %>%
  arrange(gvkey, datadate) %>%
  group_by(gvkey) %>%
  mutate(
    fiscal_lag = floor_date(datadate + months(6), "month"),
    mktcap = abs(prcc_f * csho),
    mktcap_lag = lag(mktcap),
    oancf_fallback = ( (act - lag(act)) - (che - lag(che)) ) -
      ( (lct - lag(lct)) - (dlc - lag(dlc)) - (txp - lag(txp)) - dp ),
    oancf_used = if_else(!is.na(oancf), oancf, oancf_fallback),
    acc = (ib - oancf_used) / ( (at + lag(at)) / 2 ),
    absacc = abs(acc),
    agr = (at / lag(at)) - 1,
    cfp = (ib - oancf_fallback) / mktcap_lag,
    cfp = if_else(!is.na(oancf), oancf / mktcap_lag, cfp),
    chato = (sale / ((at + lag(at)) / 2)) - (lag(sale) / ((lag(at) + lag(at, 2)) / 2)),
    chinv = (invt - lag(invt)) / ((at + lag(at)) / 2),
    dy = dvt / (prcc_f * csho),
    egr = (ceq - lag(ceq)) / lag(ceq),
    grcapx = (capx - lag(capx, 2)) / lag(capx, 2),
    hire = (emp - lag(emp)) / lag(emp),
    chcsho = (csho / lag(csho)) - 1,
    lgr = (lt / lag(lt)) - 1,
    div_paid = if_else(!is.na(dvt) & dvt > 0, 1, 0),
    div_paid_lag = lag(div_paid),
    divi = if_else(div_paid == 1 & div_paid_lag == 0, 1, 0),
    divo = if_else(div_paid == 0 & div_paid_lag == 1, 1, 0),
    chpm = (ib / sale) - (lag(ib) / lag(sale)),
    currat = act / lct,
    invest = ((ppegt - lag(ppegt)) + (invt - lag(invt))) / lag(at),
    salecash = sale / che,
    salerec = sale / rect,
    saleinv = sale / invt,
    operprof = (revt - cogs - xsga0 - xint0) / lag(ceq),
    orgcap = accumulate(xsga, ~ 0.3 * .y + 0.9 * .x, .init = 0)[-1],
    pchsaleinv = ((sale / invt) - (lag(sale) / lag(invt))) / (lag(sale) / lag(invt)),
    pchcurrat = (currat - lag(currat)) / abs(lag(currat)),
    pchcapx = (capx - lag(capx)) / abs(lag(capx)),
    pchsale_pchinvt = ((sale - lag(sale)) / lag(sale)) - ((invt - lag(invt)) / lag(invt)),
    pchsale_pchrect = ((sale - lag(sale)) / lag(sale)) - ((rect - lag(rect)) / lag(rect)),
    pchgm_pchsale = (((sale - cogs) - (lag(sale) - lag(cogs))) / (lag(sale) - lag(cogs))) -
      ((sale - lag(sale)) / lag(sale)),
    pchsale_pchxsga = ((sale - lag(sale)) / lag(sale)) - ((xsga - lag(xsga)) / lag(xsga)),
    quick = (act - invt) / lct,
    pchquick = (quick - lag(quick)) / abs(lag(quick)),
    depr = dp / ppent,
    pchdepr = (depr - lag(depr)) / abs(lag(depr)),
    grltnoa = ((at - act - lt + lct) - lag(at - act - lt + lct)) / ((at + lag(at)) / 2),
    rd = if_else(((xrd/at) - (lag(xrd)/lag(at))) / (lag(xrd)/lag(at)) > 0.05, 1, 0, missing = 0),
    rd_mve = xrd / mktcap_lag,
    sgr = (sale / lag(sale)) - 1,
    roa = ib / at,
    # F-score components
    f_roa = as.integer(roa > 0),
    f_cfo = as.integer(oancf > 0),
    f_droa = as.integer(roa - lag(roa) > 0),
    f_accrual = as.integer(oancf > ib),
    f_dlev = as.integer((lt / at) < lag(lt / at)),
    f_liq = as.integer((act / lct) > lag(act / lct)),
    f_eqiss = as.integer(csho <= lag(csho)),
    gross_margin = (sale - cogs) / sale,
    f_margin = as.integer(gross_margin > lag(gross_margin)),
    f_turn = as.integer((sale / at) > lag(sale / at)),
    ps = f_roa + f_cfo + f_droa + f_accrual +
      f_dlev + f_liq + f_eqiss +
      f_margin + f_turn
  ) %>%
  ungroup()

# tax income to book income (top statutory rate)
compustat <- compustat %>%
  mutate(
    tr = case_when(
      fyear <= 1978 ~ 0.48,
      fyear <= 1986 ~ 0.46,
      fyear == 1987 ~ 0.40,
      fyear <= 1992 ~ 0.34,
      fyear >= 1993 ~ 0.35,
      TRUE ~ NA_real_
    ),
    tb = case_when(
      !is.na(txfo) & !is.na(txfed) ~ ((txfo + txfed) / tr) / ib,
      TRUE ~ ((txt - txdi) / tr) / ib
    ),
    tb = if_else((txfo + txfed > 0 | txt > txdi) & ib <= 0, 1, tb)
  )

dbWriteTable(tidy_finance, "compustat", value = compustat, overwrite = TRUE)

# quarterly - assume public 2 months after quarter end
fundq_db <- tbl(wrds, I("comp.fundq"))

compustat_q <- fundq_db %>%
  filter(indfmt == "INDL", datafmt == "STD", popsrc == "D", consol == "C",
         datadate >= start_date, 
         datadate <= end_date
  ) %>%
  select(
    gvkey, datadate, txtq, ppentq, saleq, atq,
    actq, lctq, cheq, dlcq, dpq, rdq, txpq, ibq, prccq, cshoq
  ) %>%
  collect() %>%
  mutate(fiscal_lag = floor_date(datadate + months(2), "month")) %>%
  arrange(gvkey, datadate) %>%
  group_by(gvkey) %>%
  mutate(
    oancfq_proxy = ((actq - lag(actq)) - (cheq - lag(cheq))) -
      ((lctq - lag(lctq)) - (dlcq - lag(dlcq)) - (txpq - lag(txpq)) - dpq),
    accq = (ibq - oancfq_proxy) / ((atq + lag(atq)) / 2),
    scaled_acc = if_else(saleq == 0 | is.na(saleq), accq / 0.01, accq / saleq),
    stdacc = slide_dbl(
      .x = scaled_acc,
      .f = ~ sd(.x, na.rm = TRUE),
      .before = 15,
      .after = 0,
      .complete = TRUE
    ),
    chtx = (txtq - lag(txtq, 4)) / lag(atq, 4),
    invest_t = (ppentq - lag(ppentq, 1)) / pmax(saleq, 0.01),
    invest_t_1 = (lag(ppentq, 1) - lag(ppentq, 2)) / pmax(lag(saleq, 1), 0.01),
    invest_t_2 = (lag(ppentq, 2) - lag(ppentq, 3)) / pmax(lag(saleq, 2), 0.01),
    cinvest = invest_t - (invest_t_1 + invest_t_2) / 2,
    saleq_lag4 = lag(saleq, 4),
    rsup_num   = saleq - saleq_lag4,
    mktcap_q   = prccq * cshoq,
    rsup       = rsup_num / mktcap_q,
    eps_growth = ibq > lag(ibq, 4),  # compare to same quarter 1 year ago
    nincr = ave(eps_growth, gvkey, FUN = function(x) {
      r <- rle(x)
      rep(ifelse(r$values, r$lengths, 0), r$lengths)
    }),
    accrual_q  = ( (actq - lag(actq)) - (cheq - lag(cheq)) ) -
      ( (lctq - lag(lctq)) - (dlcq - lag(dlcq)) - (txpq - lag(txpq)) ),
    
    cf_q       = ibq - accrual_q,                       # operating cash flow
    stdcf = slide_dbl(
      cf_q / pmax(saleq, .01),
      ~ sd(.x, na.rm = TRUE),
      .before   = 15,
      .after    = 0,
      .complete = FALSE),
    stdcf = zoo::na.locf(stdcf, na.rm = FALSE),
    roa_q      = ibq / pmax(atq, 0.01),
    roavol     = slide_dbl(roa_q,
                           sd, .before = 15, .after = 0, .complete = TRUE),
    roa_q   = ibq  / ((atq + lag(atq)) / 2),
    sgr_q   = (saleq / lag(saleq)) - 1,
    varroa  = slide_dbl(roa_q, ~ sd(.x, na.rm=TRUE), .before = 7, .complete=TRUE),
    varsgr  = slide_dbl(sgr_q, ~ sd(.x, na.rm=TRUE), .before = 7, .complete=TRUE)
  ) %>%
  ungroup() %>%
  select(gvkey, fiscal_lag, chtx, cinvest, rdq, rsup, nincr,stdcf, roavol,roa_q,sgr_q,varroa,varsgr, stdacc)
