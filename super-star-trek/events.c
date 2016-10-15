#include "sst.h"
#include <math.h>

void events(void) {

	int ictbeam=0, ipage=0, istract=0, line, i, j, k, l, ixhold, iyhold;
	double fintim = g_d.stardate + Time, datemin, xtime, repair, yank;
	

#ifdef DEBUG
	if (idebug) prout("EVENTS");
#endif

	if (ship_date_chart_damaged == 1e30 && !REPORTS)
	{
		/* chart will no longer be updated because radio is dead */
		ship_date_chart_damaged = g_d.stardate;
		for (i=1; i <= 8 ; i++)
			for (j=1; j <= 8; j++)
				if (starch[i][j] == 1) starch[i][j] = g_d.galaxy[i][j]+1000;
	}

	for (;;) {
		/* Select earliest extraneous event, line==0 if no events */
		line = FSPY;
		if (alldone) return;
		datemin = fintim;
		for (l=1; l<=NEVENTS; l++)
			if (future[l] <= datemin) {
				line = l;
				datemin = future[l];
			}
		xtime = datemin-g_d.stardate;
#ifdef CLOAKING
		if (iscloaking) {
			ship_energy -= xtime*500.0;
			if (ship_energy <= 0.) {
				finish(FNRG);
				return;
			}
		}
#endif
		g_d.stardate = datemin;
		/* Decrement Federation resources and recompute remaining time */
		g_d.remaining_resources -= (g_d.remaining_klingons+4*g_d.remaining_commanders)*xtime;
		g_d.remaining_time = g_d.remaining_resources/(g_d.remaining_klingons+4*g_d.remaining_commanders);
		if (g_d.remaining_time <=0) {
			finish(FDEPLETE);
			return;
		}
		/* Is life support adequate? */
		if (damage[DLIFSUP] && ship_condition != IHDOCKED) {
			if (ship_life_support_reserves < xtime && damage[DLIFSUP] > ship_life_support_reserves) {
				finish(FLIFESUP);
				return;
			}
			ship_life_support_reserves -= xtime;
			if (damage[DLIFSUP] <= xtime) ship_life_support_reserves = game_initial_lifesupport;
		}
		/* Fix devices */
		repair = xtime;
		if (ship_condition == IHDOCKED) repair /= docfac;
		/* Don't fix Deathray here */
		for (l=1; l<=ndevice; l++)
			if (damage[l] > 0.0 && l != DDRAY)
                damage[l] -= (damage[l]-repair > 0.0 ? repair : damage[l]);
        /* Fix Deathray if docked */
        if (damage[DDRAY] > 0.0 && ship_condition == IHDOCKED)
            damage[DDRAY] -= (damage[l] - xtime > 0.0 ? xtime : damage[DDRAY]);
		/* If radio repaired, update star chart and attack reports */
		if (ship_date_chart_damaged != 1e30 && REPORTS) {
			ship_date_chart_damaged = 1e30;
			prout("Lt. Uhura- \"Captain, the sub-space radio is working and");
			prout("   surveillance reports are coming in.");
			skip(1);
			for (i=1; i <= 8 ; i++)
				for (j=1; j <= 8; j++)
					if (starch[i][j] > 999) starch[i][j] = 1;
			if (has_seen_attack_report==0) {
				attakreport();
				has_seen_attack_report = 1;
			}
			skip(1);
			prout("   The star chart is now up to stardate.\"");
			skip(1);
		}
		/* Cause extraneous event LINE to occur */
		Time -= xtime;
		switch (line) {
			case FSNOVA: /* Supernova */
				if (ipage==0) pause(1);
				ipage=1;
				snova(0,0);
				future[FSNOVA] = g_d.stardate + expran(0.5*game_initial_time);
				if (g_d.galaxy[quadx][quady] == 1000) return;
				break;
			case FSPY: /* Check with spy to see if S.C. should tractor beam */
                if (g_d.remaining_supercommanders == 0 ||
#ifdef CLOAKING
                      iscloaked ||  /* Cannot tractor beam if we can't be seen! */
#endif
					ictbeam+istract > 0 ||
					ship_condition==IHDOCKED || is_supercommander_attacking_base==1 || currentq_is_supercommander_here==1) return;
				if (ientesc ||
					(ship_energy < 2000 && torps < 4 && ship_shield_strength < 1250) ||
					(damage[DPHASER]>0 && (damage[DPHOTON]>0 || torps < 4)) ||
					(damage[DSHIELD] > 0 &&
					 (ship_energy < 2500 || damage[DPHASER] > 0) &&
					 (torps < 5 || damage[DPHOTON] > 0))) {
					/* Tractor-beam her! */
					istract=1;
					yank = square(g_d.qx_supercommander-quadx) + square(g_d.qy_supercommander-quady);
					/*********TBEAM CODE***********/
				}
				else return;
			case FTBEAM: /* Tractor beam */
				if (line==FTBEAM) {
					if (g_d.remaining_commanders == 0) {
						future[FTBEAM] = 1e30;
						break;
					}
					i = (int)(Rand()*g_d.remaining_commanders+1.0);
					yank = square(g_d.qx_commander[i]-quadx) + square(g_d.qy_commander[i]-quady);
                    if (istract || ship_condition == IHDOCKED ||
#ifdef CLOAKING
                          iscloaked || /* cannot tractor beam if we can't be seen */
#endif
                          yank == 0) {
						/* Drats! Have to reschedule */
						future[FTBEAM] = g_d.stardate + Time +
										 expran(1.5*game_initial_time/g_d.remaining_commanders);
						break;
					}
				}
				/* tractor beaming cases merge here */
				yank = sqrt(yank);
				if (ipage==0) pause(1);
				ipage=1;
				Time = (10.0/(7.5*7.5))*yank; /* 7.5 is yank rate (warp 7.5) */
				ictbeam = 1;
				skip(1);
				proutn("***");
				crmshp();
				prout(" caught in long range tractor beam--");
				/* If Kirk & Co. screwing around on planet, handle */
				atover(1); /* atover(1) is Grab */
				if (alldone) return;
				if (is_aboard_shuttle == 1) { /* Caught in Galileo? */
					finish(FSTRACTOR);
					return;
				}
				/* Check to see if shuttle is aboard */
				if (has_suttlecraft==0) {
					skip(1);
					if (Rand() >0.5) {
						prout("Galileo, left on the planet surface, is captured");
						prout("by aliens and made into a flying McDonald's.");
						damage[DSHUTTL] = -10;
						has_suttlecraft = -1;
					}
					else {
						prout("Galileo, left on the planet surface, is well hidden.");
					}
				}
				if (line==0) {
					quadx = g_d.qx_supercommander;
					quady = g_d.qy_supercommander;
				}
				else {
					quadx = g_d.qx_commander[i];
					quady = g_d.qy_commander[i];
				}
				iran10(&sectx, &secty);
				crmshp();
				proutn(" is pulled to");
				cramlc(1, quadx, quady);
				proutn(", ");
				cramlc(2, sectx, secty);
				skip(1);
				if (resting) {
					prout("(Remainder of rest/repair period cancelled.)");
					resting = 0;
				}
				if (is_shield_up==0) {
					if (damage[DSHIELD]==0 && ship_shield_strength > 0) {
						sheild(2); /* Shldsup */
						is_shield_changing=0;
					}
					else prout("(Shields not currently useable.)");
				}
				newqad(0);
				/* Adjust finish time to time of tractor beaming */
				fintim = g_d.stardate+Time;
				if (g_d.remaining_commanders <= 0) future[FTBEAM] = 1e30;
				else future[FTBEAM] = g_d.stardate+Time+expran(1.5*game_initial_time/g_d.remaining_commanders);
				break;
			case FSNAP: /* Snapshot of the universe (for time warp) */
				snapsht = g_d;
				g_d.snap = 1;
				future[FSNAP] = g_d.stardate + expran(0.5 * game_initial_time);
				break;
			case FBATTAK: /* Commander attacks starbase */
				if (g_d.remaining_commanders==0 || g_d.remaining_bases==0) {
					/* no can do */
					future[FBATTAK] = future[FCDBAS] = 1e30;
					break;
				}
				i = 0;
				for (j=1; j<=g_d.remaining_bases; j++) {
					for (k=1; k<=g_d.remaining_commanders; k++)
						if (g_d.qx_base[j]==g_d.qx_commander[k] && g_d.qy_base[j]==g_d.qy_commander[k] &&
							(g_d.qx_base[j]!=quadx || g_d.qy_base[j]!=quady) &&
							(g_d.qx_base[j]!=g_d.qx_supercommander || g_d.qy_base[j]!=g_d.qy_supercommander)) {
							i = 1;
							break;
						}
					if (i == 1) break;
				}
				if (j>g_d.remaining_bases) {
					/* no match found -- try later */
					future[FBATTAK] = g_d.stardate + expran(0.3*game_initial_time);
					future[FCDBAS] = 1e30;
					break;
				}
				/* commander + starbase combination found -- launch attack */
				batx = g_d.qx_base[j];
				baty = g_d.qy_base[j];
				future[FCDBAS] = g_d.stardate+1.0+3.0*Rand();
				if (is_supercommander_attacking_base) /* extra time if SC already attacking */
					future[FCDBAS] += future[FSCDBAS]-g_d.stardate;
				future[FBATTAK] = future[FCDBAS] +expran(0.3*game_initial_time);
				has_seen_attack_report = 0;
				if (!REPORTS)
				     break; /* No warning :-( */
				has_seen_attack_report = 1;
				if (ipage==0) pause(1);
				ipage = 1;
				skip(1);
				proutn("Lt. Uhura-  \"Captain, the starbase in");
				cramlc(1, batx, baty);
				skip(1);
				prout("   reports that it is under atttack and that it can");
				proutn("   hold out only until stardate ");
				cramf(future[FCDBAS],1,1);
				prout(".\"");
				if (resting) {
					skip(1);
					proutn("Mr. Spock-  \"Captain, shall we cancel the rest period?\"");
					if (ja()) {
						resting = 0;
						Time = 0.0;
						return;
					}
				}
				break;
			case FSCDBAS: /* Supercommander destroys base */
				future[FSCDBAS] = 1e30;
				is_supercommander_attacking_base = 2;
				if (g_d.galaxy[g_d.qx_supercommander][g_d.qy_supercommander]%100 < 10) break; /* WAS RETURN! */
				ixhold = batx;
				iyhold = baty;
				batx = g_d.qx_supercommander;
				baty = g_d.qy_supercommander;
			case FCDBAS: /* Commander succeeds in destroying base */
				if (line==FCDBAS) {
					future[FCDBAS] = 1e30;
					/* find the lucky pair */
					for (i = 1; i <= g_d.remaining_commanders; i++)
						if (g_d.qx_commander[i]==batx && g_d.qy_commander[i]==baty) break;
					if (i > g_d.remaining_commanders || g_d.remaining_bases == 0 ||
						g_d.galaxy[batx][baty] % 100 < 10) {
						/* No action to take after all */
						batx = baty = 0;
						break;
					}
				}
				/* Code merges here for any commander destroying base */
				/* Not perfect, but will have to do */
				if (starch[batx][baty] == -1) starch[batx][baty] = 0;
				/* Handle case where base is in same quadrant as starship */
				if (batx==quadx && baty==quady) {
					if (starch[batx][baty] > 999) starch[batx][baty] -= 10;
					quad[currentq_base_sx][currentq_base_sy]= IHDOT;
					currentq_base_sx=currentq_base_sy=0;
					newcnd();
					skip(1);
					prout("Spock-  \"Captain, I believe the starbase has been destroyed.\"");
				}
				else if (g_d.remaining_bases != 1 && REPORTS) {
					/* Get word via subspace radio */
					if (ipage==0) pause(1);
					ipage = 1;
					skip(1);
					prout("Lt. Uhura-  \"Captain, Starfleet Command reports that");
					proutn("   the starbase in");
					cramlc(1, batx, baty);
					prout(" has been destroyed by");
					if (is_supercommander_attacking_base==2) prout("the Klingon Super-Commander");
					else prout("a Klingon Commander");
				}
				/* Remove Starbase from galaxy */
				g_d.galaxy[batx][baty] -= 10;
				for (i=1; i <= g_d.remaining_bases; i++)
					if (g_d.qx_base[i]==batx && g_d.qy_base[i]==baty) {
						g_d.qx_base[i]=g_d.qx_base[g_d.remaining_bases];
						g_d.qy_base[i]=g_d.qy_base[g_d.remaining_bases];
					}
				g_d.remaining_bases--;
				if (is_supercommander_attacking_base == 2) {
					/* reinstate a commander's base attack */
					batx = ixhold;
					baty = iyhold;
					is_supercommander_attacking_base = 0;
				}
				else {
					batx = baty = 0;
				}
				break;
			case FSCMOVE: /* Supercommander moves */
				future[FSCMOVE] = g_d.stardate+0.2777;
				if (ientesc+istract==0 &&
					is_supercommander_attacking_base!=1 &&
					(currentq_is_supercommander_here!=1 || justin==1)) scom(&ipage);
				break;
			case FDSPROB: /* Move deep space probe */
				future[FDSPROB] = g_d.stardate + 0.01;
				probe_global_x += probe_increment_gx;
				probe_global_y += probe_increment_gy;
				i = (int)(probe_global_x/10 +0.05);
				j = (int)(probe_global_y/10 + 0.05);
				if (probe_qx != i || probe_qy != j) {
					probe_qx = i;
					probe_qy = j;
					if (i < 1 || i > 8 || j < 1 || j > 8 ||
						g_d.galaxy[probe_qx][probe_qy] == 1000) {
						// Left galaxy or ran into supernova
						if (REPORTS) {
							if (ipage==0) pause(1);
							ipage = 1;
							skip(1);
							proutn("Lt. Uhura-  \"The deep space probe ");
							if (i < 1 ||i > 8 || j < 1 || j > 8)
								proutn("has left the galaxy");
							else
								proutn("is no longer transmitting");
							prout(".\"");
						}
						future[FDSPROB] = 1e30;
						break;
					}
					if (REPORTS) {
						if (ipage==0) pause(1);
						ipage = 1;
						skip(1);
						proutn("Lt. Uhura-  \"The deep space probe is now in ");
						cramlc(1, probe_qx, probe_qy);
						prout(".\"");
					}
				}
				/* Update star chart if Radio is working or have access to
				   radio. */
				if (REPORTS) 
					starch[probe_qx][probe_qy] = damage[DRADIO] > 0.0 ?
						                    g_d.galaxy[probe_qx][probe_qy]+1000 : 1;
				probe_active_sectors_remaining--; // One less to travel
				if (probe_active_sectors_remaining == 0 && is_probe_armed &&
					g_d.galaxy[probe_qx][probe_qy] % 10 > 0) {
					/* lets blow the sucker! */
					snova(1,0);
					future[FDSPROB] = 1e30;
					if (g_d.galaxy[quadx][quady] == 1000) return;
				}
				break;
		}
	}
}

				
void waiting(void) {
	int key;
	double temp, delay, origTime;

	ididit = 0;
	for (;;) {
		key = scan();
		if (key  != IHEOL) break;
		proutn("How long? ");
	}
	chew();
	if (key != IHREAL) {
		huh();
		return;
	}
	origTime = delay = aaitem;
	if (delay <= 0.0) return;
	if (delay >= g_d.remaining_time || currentq_num_enemies != 0) {
		prout("Are you sure? ");
		if (ja() == 0) return;
	}

	/* Alternate resting periods (events) with attacks */

	resting = 1;
	do {
		if (delay <= 0) resting = 0;
		if (resting == 0) {
			cramf(g_d.remaining_time, 0, 2);
			prout(" stardates left.");
			return;
		}
		temp = Time = delay;

		if (currentq_num_enemies) {
			double rtime = 1.0 + Rand();
			if (rtime < temp) temp = rtime;
			Time = temp;
		}
		if (Time < delay) attack(0);
		if (currentq_num_enemies==0) movetho();
		if (alldone) return;
		events();
		ididit = 1;
		if (alldone) return;
		delay -= temp;
	} while (g_d.galaxy[quadx][quady] != 1000); // leave if quadrant supernovas

	resting = 0;
	Time = 0;
}

void nova(int ix, int iy) {
	static double course[] =
		{0.0, 10.5, 12.0, 1.5, 9.0, 0.0, 3.0, 7.5, 6.0, 4.5};
	int bot, top, top2, /*burst,*/ hits[11][3], kount, icx, icy, mm, nn, j;
	int iquad, iquad1, i, ll, newcx, newcy, ii, jj;
	if (Rand() < 0.05) {
		/* Wow! We've supernova'ed */
		snova(ix, iy);
		return;
	}

	/* handle initial nova */
	quad[ix][iy] = IHDOT;
	crmena(1, IH_STAR, 2, ix, iy);
	prout(" novas.");
	g_d.galaxy[quadx][quady] -= 1;
	g_d.killed_stars++;
	
	/* Set up stack to recursively trigger adjacent stars */
	bot = top = top2 = 1;
	kount = 0;
	icx = icy = 0;
	hits[1][1] = ix;
	hits[1][2] = iy;
	while (1) {
		for (mm = bot; mm <= top; mm++) 
		for (nn = 1; nn <= 3; nn++)  /* nn,j represents coordinates around current */
			for (j = 1; j <= 3; j++) {
				if (j==2 && nn== 2) continue;
				ii = hits[mm][1]+nn-2;
				jj = hits[mm][2]+j-2;
				if (ii < 1 || ii > 10 || jj < 1 || jj > 10) continue;
				iquad = quad[ii][jj];
				switch (iquad) {
//					case IHDOT:	/* Empty space ends reaction
//					case IHQUEST:
//					case IH_BLACK_HOLE:
//					case IH_THOLIAN:
//					case IH_THOLIAN_WEB:
					default:
						break;
					case IH_STAR: /* Affect another star */
						if (Rand() < 0.05) {
							/* This star supernovas */
							snova(ii,jj);
							return;
						}
						top2++;
						hits[top2][1]=ii;
						hits[top2][2]=jj;
						g_d.galaxy[quadx][quady] -= 1;
						g_d.killed_stars++;
						crmena(1, IH_STAR, 2, ii, jj);
						prout(" novas.");
						quad[ii][jj] = IHDOT;
						break;
					case IH_PLANET: /* Destroy planet */
						g_d.newstuf[quadx][quady] -= 1;
						g_d.killed_planets++;
						crmena(1, IH_PLANET, 2, ii, jj);
						prout(" destroyed.");
						g_d.plnets[currentq_planet_id] = nulplanet;
						currentq_planet_id = currentq_planet_sx = currentq_planet_sy = 0;
						if (is_landed == 1) {
							finish(FPNOVA);
							return;
						}
						quad[ii][jj] = IHDOT;
						break;
					case IH_BASE: /* Destroy base */
						g_d.galaxy[quadx][quady] -= 10;
						for (i = 1; i <= g_d.remaining_bases; i++)
							if (g_d.qx_base[i]==quadx && g_d.qy_base[i]==quady) break;
						g_d.qx_base[i] = g_d.qx_base[g_d.remaining_bases];
						g_d.qy_base[i] = g_d.qy_base[g_d.remaining_bases];
						g_d.remaining_bases--;
						currentq_base_sx = currentq_base_sy = 0;
						g_d.killed_bases++;
						newcnd();
						crmena(1, IH_BASE, 2, ii, jj);
						prout(" destroyed.");
						quad[ii][jj] = IHDOT;
						break;
					case IH_ENTERPRISE: /* Buffet ship */
					case IH_FAERIE_QUEEN:
						prout("***Starship buffeted by nova.");
						if (is_shield_up) {
							if (ship_shield_strength >= 2000.0) ship_shield_strength -= 2000.0;
							else {
								double diff = 2000.0 - ship_shield_strength;
								ship_energy -= diff;
								ship_shield_strength = 0.0;
								is_shield_up = 0;
								prout("***Shields knocked out.");
								damage[DSHIELD] += 0.005*game_damage_factor*Rand()*diff;
							}
						}
						else ship_energy -= 2000.0;
						if (ship_energy <= 0) {
							finish(FNOVA);
							return;
						}
						/* add in course nova contributes to kicking starship*/
						icx += sectx-hits[mm][1];
						icy += secty-hits[mm][2];
						kount++;
						break;
					case IH_KLINGON: /* kill klingon */
						deadkl(ii,jj,iquad, ii, jj);
						break;
					case IH_COMMANDER: /* Damage/destroy big enemies */
					case IH_SUPER_COMMANDER:
					case IH_ROMULAN:
						for (ll = 1; ll <= currentq_num_enemies; ll++)
							if (kx[ll]==ii && ky[ll]==jj) break;
						kpower[ll] -= 800.0; /* If firepower is lost, die */
						if (kpower[ll] <= 0.0) {
							deadkl(ii, jj, iquad, ii, jj);
							break;
						}
						newcx = ii + ii - hits[mm][1];
						newcy = jj + jj - hits[mm][2];
						crmena(1, iquad, 2, ii, jj);
						proutn(" damaged");
						if (newcx<1 || newcx>10 || newcy<1 || newcy>10) {
							/* can't leave quadrant */
							skip(1);
							break;
						}
						iquad1 = quad[newcx][newcy];
						if (iquad1 == IH_BLACK_HOLE) {
							proutn(", blasted into ");
							crmena(0, IH_BLACK_HOLE, 2, newcx, newcy);
							skip(1);
							deadkl(ii, jj, iquad, newcx, newcy);
							break;
						}
						if (iquad1 != IHDOT) {
							/* can't move into something else */
							skip(1);
							break;
						}
						proutn(", buffeted to");
						cramlc(2, newcx, newcy);
						quad[ii][jj] = IHDOT;
						quad[newcx][newcy] = iquad;
						kx[ll] = newcx;
						ky[ll] = newcy;
						kavgd[ll] = sqrt(square(sectx-newcx)+square(secty-newcy));
						kdist[ll] = kavgd[ll];
						skip(1);
						break;
				}
			}
		if (top == top2) break;
		bot = top + 1;
		top = top2;
	}
	if (kount==0) return;

	/* Starship affected by nova -- kick it away. */
	dist = kount*0.1;
	if (icx) icx = (icx < 0 ? -1 : 1);
	if (icy) icy = (icy < 0 ? -1 : 1);
	direc = course[3*(icx+1)+icy+2];
	if (direc == 0.0) dist = 0.0;
	if (dist == 0.0) return;
	Time = 10.0*dist/16.0;
	skip(1);
	prout("Force of nova displaces starship.");
	iattak=2;	/* Eliminates recursion problem */
	lmove();
	Time = 10.0*dist/16.0;
	return;
}
	
	
void snova(int insx, int insy) {
	int comdead, nqx, nqy, nsx, nsy, num, kldead, iscdead;
	int nrmdead, npdead;
	int insipient=0;

	nsx = insx;
	nsy = insy;

	if (insy== 0) {
		if (insx == 1) {
			/* NOVAMAX being used */
			nqx = probe_qx;
			nqy = probe_qy;
		}
		else {
			int stars = 0;
			/* Scheduled supernova -- select star */
			/* logic changed here so that we won't favor quadrants in top
			left of universe */
			for (nqx = 1; nqx<=8; nqx++) {
				for (nqy = 1; nqy<=8; nqy++) {
					stars += g_d.galaxy[nqx][nqy] % 10;
				}
			}
			if (stars == 0) return; /* nothing to supernova exists */
			num = (int)(Rand()*stars + 1);
			for (nqx = 1; nqx<=8; nqx++) {
				for (nqy = 1; nqy<=8; nqy++) {
					num -= g_d.galaxy[nqx][nqy] % 10;
					if (num <= 0) break;
				}
				if (num <=0) break;
			}
#ifdef DEBUG
			if (idebug) {
				proutn("Super nova here?");
				if (ja()==1) {
					nqx = quadx;
					nqy = quady;
				}
			}
#endif
		}

		if (nqx != quady || nqy != quady || justin != 0) {
			/* it isn't here, or we just entered (treat as inroute) */
			if (REPORTS) {
				skip(1);
				proutn("Message from Starfleet Command       Stardate ");
				cramf(g_d.stardate, 0, 1);
				skip(1);
				proutn("     Supernova in");
				cramlc(1, nqx, nqy);
				prout("; caution advised.");
			}
		}
		else {
			/* we are in the quadrant! */
			insipient = 1;
			num = (int)(Rand()* (g_d.galaxy[nqx][nqy]%10) + 1);
			for (nsx=1; nsx < 10; nsx++) {
				for (nsy=1; nsy < 10; nsy++) {
					if (quad[nsx][nsy]==IH_STAR) {
						num--;
						if (num==0) break;
					}
				}
				if (num==0) break;
			}
		}
	}
	else {
		insipient = 1;
	}

	if (insipient) {
		skip(1);
		prouts("***RED ALERT!  RED ALERT!");
		skip(1);
		proutn("***Incipient supernova detected at");
		cramlc(2, nsx, nsy);
		skip(1);
		nqx = quadx;
		nqy = quady;
		if (square(nsx-sectx) + square(nsy-secty) <= 2.1) {
			proutn("Emergency override attempts t");
			prouts("***************");
			skip(1);
			stars();
			alldone=1;
		}
	}
	/* destroy any Klingons in supernovaed quadrant */
	num=g_d.galaxy[nqx][nqy];
    kldead = num/100;
    g_d.remaining_klingons -= kldead; // Moved here to correctly set remaining Klingon count
	comdead = iscdead = 0;
	if (nqx==g_d.qx_supercommander && nqy == g_d.qy_supercommander) {
		/* did in the Supercommander! */
		g_d.remaining_supercommanders = g_d.qx_supercommander = g_d.qy_supercommander = is_supercommander_attacking_base = currentq_is_supercommander_here = 0;
		iscdead = 1;
		kldead--; /* Get proper kill credit */
		future[FSCMOVE] = future[FSCDBAS] = 1e30;
	}

    if (g_d.remaining_commanders) {
		int maxloop = g_d.remaining_commanders, l;
		for (l = 1; l <= maxloop; l++) {
			if (g_d.qx_commander[l] == nqx && g_d.qy_commander[l] == nqy) {
				g_d.qx_commander[l] = g_d.qx_commander[g_d.remaining_commanders];
				g_d.qy_commander[l] = g_d.qy_commander[g_d.remaining_commanders];
				g_d.qx_commander[g_d.remaining_commanders] = g_d.qy_commander[g_d.remaining_commanders] = 0;
				g_d.remaining_commanders--;
				kldead--;
				comdead++;
				if (g_d.remaining_commanders==0) future[FTBEAM] = 1e30;
				break;
			}
		}
	}
	/* destroy Romulans and planets in supernovaed quadrant */
	num = g_d.newstuf[nqx][nqy];
	g_d.newstuf[nqx][nqy] = 0;
	nrmdead = num/10;
	g_d.remaining_romulans -= nrmdead;
	npdead = num - nrmdead*10;
	if (npdead) {
		int l;
		for (l = 1; l <= inplan; l++)
			if (g_d.plnets[l].qx == nqx && g_d.plnets[l].qy == nqy) {
				g_d.plnets[l] = nulplanet;
			}
	}
	/* Destroy any base in supernovaed quadrant */
	if (g_d.remaining_bases) {
		int maxloop = g_d.remaining_bases, l;
		for (l = 1; l <= maxloop; l++)
			if (g_d.qx_base[l]==nqx && g_d.qy_base[l]==nqy) {
				g_d.qx_base[l] = g_d.qx_base[g_d.remaining_bases];
				g_d.qy_base[l] = g_d.qy_base[g_d.remaining_bases];
				g_d.qx_base[g_d.remaining_bases] = g_d.qy_base[g_d.remaining_bases] = 0;
				g_d.remaining_bases--;
				break;
			}
	}
	/* If starship caused supernova, tally up destruction */
	if (insx) {
		num = g_d.galaxy[nqx][nqy] % 100;
		g_d.killed_stars += num % 10;
		g_d.killed_bases += num/10;
		g_d.killed_klingons += kldead;
		g_d.killed_commanders += comdead;
		g_d.killed_romulans += nrmdead;
		g_d.killed_planets += npdead;
		g_d.killed_supercommanders += iscdead;
	}
	/* mark supernova in galaxy and in star chart */
	if ((quadx == nqx && quady == nqy) || REPORTS)
		starch[nqx][nqy] = 1;
	g_d.galaxy[nqx][nqy] = 1000;
	/* If supernova destroys last klingons give special message */
	if (g_d.remaining_klingons==0 && (nqx != quadx || nqy != quady)) {
		skip(2);
		if (insx == 0) prout("Lucky you!");
		proutn("A supernova in");
		cramlc(1, nqx, nqy);
		prout(" has just destroyed the last Klingons.");
		finish(FWON);
		return;
	}
	/* if some Klingons remain, continue or die in supernova */
	if (alldone) finish(FSNOVAED);
	return;
}
		
				
