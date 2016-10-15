#include "sst.h"

static int tryexit(int lookx, int looky, int ienm, int loccom, int irun) {
	int iqx, iqy, l;

	iqx = quadx+(lookx+9)/10 - 1;
	iqy = quady+(looky+9)/10 - 1;
	if (iqx < 1 || iqx > 8 || iqy < 1 || iqy > 8 ||
		g_d.galaxy[iqx][iqy] > 899)
		return 0; /* no can do -- neg energy, supernovae, or >8 Klingons */
	if (ienm == IH_ROMULAN) return 0; /* Romulans cannot escape! */
	if (irun == 0) {
		/* avoid intruding on another commander's territory */
		if (ienm == IH_COMMANDER) {
			for (l = 1; l <= g_d.remaining_commanders; l++)
				if (g_d.qx_commander[l]==iqx && g_d.qy_commander[l]==iqy) return 0;
			/* refuse to leave if currently attacking starbase */
			if (batx==quadx && baty==quady) return 0;
		}
		/* don't leave if over 1000 units of energy */
		if (kpower[loccom] > 1000.) return 0;
	}
	/* print escape message and move out of quadrant.
	   We know this if either short or long range sensors are working */
	if (damage[DSRSENS] == 0.0 || damage[DLRSENS] == 0.0 ||
		ship_condition == IHDOCKED) {
		proutn("***");
		cramen(ienm);
		proutn(" escapes to");
		cramlc(1, iqx, iqy);
		prout(" (and regains strength).");
	}
	/* handle local matters related to escape */
	kx[loccom] = kx[currentq_num_enemies];
	ky[loccom] = ky[currentq_num_enemies];
	kavgd[loccom] = kavgd[currentq_num_enemies];
	kpower[loccom] = kpower[currentq_num_enemies];
	kdist[loccom] = kdist[currentq_num_enemies];
	currentq_num_klingons--;
	currentq_num_enemies--;
	if (ship_condition != IHDOCKED) newcnd();
	/* Handle global matters related to escape */
	g_d.galaxy[quadx][quady] -= 100;
	g_d.galaxy[iqx][iqy] += 100;
	if (ienm==IH_SUPER_COMMANDER) {
		currentq_has_supercommander=0;
		currentq_is_supercommander_here=0;
		ientesc=0;
		is_supercommander_attacking_base=0;
		future[FSCMOVE]=0.2777+g_d.stardate;
		future[FSCDBAS]=1e30;
		g_d.qx_supercommander=iqx;
		g_d.qy_supercommander=iqy;
	}
	else {
		for (l=1; l<=g_d.remaining_commanders; l++) {
			if (g_d.qx_commander[l]==quadx && g_d.qy_commander[l]==quady) {
				g_d.qx_commander[l]=iqx;
				g_d.qy_commander[l]=iqy;
				break;
			}
		}
		currentq_num_commanders = 0;
	}
	return 1; /* success */
}


static void movebaddy(int comx, int comy, int loccom, int ienm) {
	int motion, mdist, nsteps, mx, my, nextx, nexty, lookx, looky, ll;
	int irun = 0;
	int krawlx, krawly;
	int success;
	int attempts;
	/* This should probably be just currentq_num_commanders + currentq_has_supercommander */
	int nbaddys = game_skill > SGOOD ?
				  (int)((currentq_num_commanders*2 + currentq_has_supercommander*2+currentq_num_klingons*1.23+currentq_num_romulans*1.5)/2.0):
				  (currentq_num_commanders + currentq_has_supercommander);
	double dist1, forces;

	dist1 = kdist[loccom];
	mdist = (int)( dist1 + 0.5); /* Nearest integer distance */

	/* If SC, check with spy to see if should hi-tail it */
	if (ienm==IH_SUPER_COMMANDER &&
		(kpower[loccom] <= 500.0 || (ship_condition==IHDOCKED && damage[DPHOTON]==0))) {
		irun = 1;
		motion = -10;
	}
	else {
		/* decide whether to advance, retreat, or hold position */
/* Algorithm:
   * Enterprise has "force" based on condition of phaser and photon torpedoes.
     If both are operating full strength, force is 1000. If both are damaged,
	 force is -1000. Having shields down subtracts an additional 1000.

   * Enemy has forces equal to the energy of the attacker plus
     100*(K+R) + 500*(C+S) - 400 for novice through good levels OR
	 346*K + 400*R + 500*(C+S) - 400 for expert and emeritus.

	 Attacker Initial energy levels (nominal):
	          Klingon   Romulan   Commander   Super-Commander
	 Novice    400        700        1200        
	 Fair      425        750        1250
	 Good      450        800        1300        1750
	 Expert    475        850        1350        1875
	 Emeritus  500        900        1400        2000
     VARIANCE   75        200         200         200

	 Enemy vessels only move prior to their attack. In Novice - Good games
	 only commanders move. In Expert games, all enemy vessels move if there
	 is a commander present. In Emeritus games all enemy vessels move.

  *  If Enterprise is not docked, an agressive action is taken if enemy
     forces are 1000 greater than Enterprise.

	 Agressive action on average cuts the distance between the ship and
	 the enemy to 1/4 the original.

  *  At lower energy advantage, movement units are proportional to the
     advantage with a 650 advantage being to hold ground, 800 to move forward
	 1, 950 for two, 150 for back 4, etc. Variance of 100.

	 If docked, is reduced by roughly 1.75*skill, generally forcing a
	 retreat, especially at high skill levels.

  *  Motion is limited to skill level, except for SC hi-tailing it out.
  */

		forces = kpower[loccom]+100.0*currentq_num_enemies+400*(nbaddys-1);
		if (is_shield_up==0) forces += 1000; /* Good for enemy if shield is down! */
		if (damage[DPHASER] == 0.0 || damage[DPHOTON] == 0.0) {
			if (damage[DPHASER] != 0) /* phasers damaged */
				forces += 300.0;
			else
				forces -= 0.2*(ship_energy - 2500.0);
			if (damage[DPHOTON] != 0) /* photon torpedoes damaged */
				forces += 300.0;
			else
				forces -= 50.0*torps;
		}
		else {
			/* phasers and photon tubes both out! */
			forces += 1000.0;
		}
		motion = 0;
		if (forces <= 1000.0 && ship_condition != IHDOCKED) /* Typical situation */
			motion = (int) (((forces+200.0*Rand())/150.0) - 5.0);
		else {
			if (forces > 1000.0) /* Very strong -- move in for kill */
				motion = (int) ((1.0-square(Rand()))*dist1 + 1.0);
			if (ship_condition==IHDOCKED) /* protected by base -- back off ! */
				motion -= (int) (game_skill*(2.0-square(Rand())));
		}
#ifdef DEBUG
		if (idebug) {
			proutn("MOTION = ");
			cramf(motion, 1, 2);
            proutn("  FORCES = ");
			cramf(forces, 1, 2);
			skip(1);
		}
#endif
		/* don't move if no motion */
		if (motion==0) return;
		/* Limit motion according to skill */
		if (abs(motion) > game_skill) motion = (motion < 0) ? -game_skill : game_skill;
	}
	/* calcuate preferred number of steps */
	nsteps = motion < 0 ? -motion : motion;
	if (motion > 0 && nsteps > mdist) nsteps = mdist; /* don't overshoot */
	if (nsteps > 10) nsteps = 10; /* This shouldn't be necessary */
	if (nsteps < 1) nsteps = 1; /* This shouldn't be necessary */
#ifdef DEBUG
	if (idebug) {
		proutn("NSTEPS = ");
		crami(nsteps, 1);
		skip(1);
	}
#endif
	/* Compute preferred values of delta X and Y */
	mx = sectx - comx;
	my = secty - comy;
	if (2.0 * abs(mx) < abs(my)) mx = 0;
	if (2.0 * abs(my) < abs(sectx-comx)) my = 0;
	if (mx != 0) mx = mx*motion < 0 ? -1 : 1;
	if (my != 0) my = my*motion < 0 ? -1 : 1;
	nextx = comx;
	nexty = comy;
	quad[comx][comy] = IHDOT;
	/* main move loop */
	for (ll = 1; ll <= nsteps; ll++) {
#ifdef DEBUG
		if (idebug) {
			crami(ll,2);
			skip(1);
		}
#endif
		/* Check if preferred position available */
		lookx = nextx + mx;
		looky = nexty + my;
		krawlx = mx < 0 ? 1 : -1;
		krawly = my < 0 ? 1 : -1;
		success = 0;
		attempts = 0; /* Settle mysterious hang problem */
		while (attempts++ < 20 && !success) {
			if (lookx < 1 || lookx > 10) {
				if (motion < 0 && tryexit(lookx, looky, ienm, loccom, irun))
					return;
				if (krawlx == mx || my == 0) break;
				lookx = nextx + krawlx;
				krawlx = -krawlx;
			}
			else if (looky < 1 || looky > 10) {
				if (motion < 0 && tryexit(lookx, looky, ienm, loccom, irun))
					return;
				if (krawly == my || mx == 0) break;
				looky = nexty + krawly;
				krawly = -krawly;
			}
			else if (quad[lookx][looky] != IHDOT) {
				/* See if we should ram ship */
				if (quad[lookx][looky] == ship_name &&
					(ienm == IH_COMMANDER || ienm == IH_SUPER_COMMANDER)) {
					ram(1, ienm, comx, comy);
					return;
				}
				if (krawlx != mx && my != 0) {
					lookx = nextx + krawlx;
					krawlx = -krawlx;
				}
				else if (krawly != my && mx != 0) {
					looky = nexty + krawly;
					krawly = -krawly;
				}
				else break; /* we have failed */
			}
			else success = 1;
		}
		if (success) {
			nextx = lookx;
			nexty = looky;
#ifdef DEBUG
			if (idebug) {
				cramlc(0, nextx, nexty);
				skip(1);
			}
#endif
		}
		else break; /* done early */
	}
	/* Put commander in place within same quadrant */
	quad[nextx][nexty] = ienm;
	if (nextx != comx || nexty != comy) {
		/* it moved */
		kx[loccom] = nextx;
		ky[loccom] = nexty;
		kdist[loccom] = kavgd[loccom] =
					sqrt(square(sectx-nextx)+square(secty-nexty));
		if (damage[DSRSENS] == 0 || ship_condition == IHDOCKED) {
			proutn("***");
			cramen(ienm);
			if (kdist[loccom] < dist1) proutn(" advances to");
			else proutn(" retreats to");
			cramlc(2, nextx, nexty);
			skip(1);
		}
	}
}

void movcom(void) {
	int ix, iy, i;

#ifdef DEBUG
	if (idebug) prout("MOVCOM");
#endif

	/* Figure out which Klingon is the commander (or Supercommander)
	   and do move */
	if (currentq_num_commanders) for (i = 1; i <= currentq_num_enemies; i++) {
		ix = kx[i];
		iy = ky[i];
		if (quad[ix][iy] == IH_COMMANDER) {
			movebaddy(ix, iy, i, IH_COMMANDER);
			break;
		}
	}
	if (currentq_has_supercommander) for (i = 1; i <= currentq_num_enemies; i++) {
		ix = kx[i];
		iy = ky[i];
		if (quad[ix][iy] == IH_SUPER_COMMANDER) {
			movebaddy(ix, iy, i, IH_SUPER_COMMANDER);
			break;
		}
	}
	/* if skill level is high, move other Klingons and Romulans too!
	   Move these last so they can base their actions on what the
       commander(s) do. */
	if (game_skill > SGOOD) for (i = 1; i <= currentq_num_enemies; i++) {
		ix = kx[i];
		iy = ky[i];
		if (quad[ix][iy] == IH_KLINGON || quad[ix][iy] == IH_ROMULAN)
			movebaddy(ix, iy, i, quad[ix][iy]);
	}

	sortkl();
}

static int checkdest(int iqx, int iqy, int flag, int *ipage) {
	int i;

	if ((iqx==quadx && iqy==quady) ||
		iqx < 1 || iqx > 8 || iqy < 1 || iqy > 8 ||
		g_d.galaxy[iqx][iqy] > 899) return 1;
	if (flag) {
		/* Avoid quadrants with bases if we want to avoid Enterprise */
		for (i = 1; i <= g_d.remaining_bases; i++)
			if (g_d.qx_base[i]==iqx && g_d.qy_base[i]==iqy) return 1;
	}

	/* do the move */
	g_d.galaxy[g_d.qx_supercommander][g_d.qy_supercommander] -= 100;
	g_d.qx_supercommander = iqx;
	g_d.qy_supercommander = iqy;
	g_d.galaxy[g_d.qx_supercommander][g_d.qy_supercommander] += 100;
	if (currentq_is_supercommander_here) {
		/* SC has scooted, Remove him from current quadrant */
		currentq_is_supercommander_here=0;
		is_supercommander_attacking_base=0;
		currentq_has_supercommander=0;
		ientesc=0;
		future[FSCDBAS]=1e30;
		for (i = 1; i <= currentq_num_enemies; i++) 
			if (quad[kx[i]][ky[i]] == IH_SUPER_COMMANDER) break;
		quad[kx[i]][ky[i]] = IHDOT;
		kx[i] = kx[currentq_num_enemies];
		ky[i] = ky[currentq_num_enemies];
		kdist[i] = kdist[currentq_num_enemies];
		kavgd[i] = kavgd[currentq_num_enemies];
		kpower[i] = kpower[currentq_num_enemies];
		currentq_num_klingons--;
		currentq_num_enemies--;
		if (ship_condition!=IHDOCKED) newcnd();
		sortkl();
	}
	/* check for a helpful planet */
	for (i = 1; i <= inplan; i++) {
		if (g_d.plnets[i].qx==g_d.qx_supercommander && 
            g_d.plnets[i].qy==g_d.qy_supercommander &&
			g_d.plnets[i].crystals == 1) {
			/* destroy the planet */
			g_d.plnets[i] = nulplanet;
			g_d.newstuf[g_d.qx_supercommander][g_d.qy_supercommander] -= 1;
			if (REPORTS) {
				if (*ipage==0) pause(1);
				*ipage = 1;
				prout("Lt. Uhura-  \"Captain, Starfleet Intelligence reports");
				proutn("   a planet in");
				cramlc(1, g_d.qx_supercommander, g_d.qy_supercommander);
				prout(" has been destroyed");
				prout("   by the Super-commander.\"");
			}
			break;
		}
	}
	return 0; /* looks good! */
}
			
		
	


void scom(int *ipage) {
	int i, i2, j, ideltax, ideltay, ibqx, ibqy, sx, sy, ifindit, iwhichb;
	int iqx, iqy;
	int basetbl[6];
	double bdist[6];
	int flag;
#ifdef DEBUG
	if (idebug) prout("SCOM");
#endif

	/* Decide on being active or passive */
	flag = ((g_d.killed_commanders+g_d.killed_klingons)/(g_d.stardate+0.01-game_initial_stardate) < 0.1*game_skill*(game_skill+1.0) ||
			(g_d.stardate-game_initial_stardate) < 3.0);
	if (currentq_is_supercommander_here==0 && flag) {
		/* compute move away from Enterprise */
		ideltax = g_d.qx_supercommander-quadx;
		ideltay = g_d.qy_supercommander-quady;
		if (sqrt(ideltax*(double)ideltax+ideltay*(double)ideltay) > 2.0) {
			/* circulate in space */
			ideltax = g_d.qy_supercommander-quady;
			ideltay = quadx-g_d.qx_supercommander;
		}
	}
	else {
		/* compute distances to starbases */
		if (g_d.remaining_bases <= 0) {
			/* nothing left to do */
			future[FSCMOVE] = 1e30;
			return;
		}
		sx = g_d.qx_supercommander;
		sy = g_d.qy_supercommander;
		for (i = 1; i <= g_d.remaining_bases; i++) {
			basetbl[i] = i;
			ibqx = g_d.qx_base[i];
			ibqy = g_d.qy_base[i];
			bdist[i] = sqrt(square(ibqx-sx) + square(ibqy-sy));
		}
		if (g_d.remaining_bases > 1) {
			/* sort into nearest first order */
			int iswitch;
			do {
				iswitch = 0;
				for (i=1; i < g_d.remaining_bases-1; i++) {
					if (bdist[i] > bdist[i+1]) {
						int ti = basetbl[i];
						double t = bdist[i];
						bdist[i] = bdist[i+1];
						bdist[i+1] = t;
						basetbl[i] = basetbl[i+1];
						basetbl[i+1] =ti;
						iswitch = 1;
					}
				}
			} while (iswitch);
		}
		/* look for nearest base without a commander, no Enterprise, and
		   without too many Klingons, and not already under attack. */
		ifindit = iwhichb = 0;

		for (i2 = 1; i2 <= g_d.remaining_bases; i2++) {
			i = basetbl[i2];	/* bug in original had it not finding nearest*/
			ibqx = g_d.qx_base[i];
			ibqy = g_d.qy_base[i];
			if ((ibqx == quadx && ibqy == quady) ||
				(ibqx == batx && ibqy == baty) ||
				g_d.galaxy[ibqx][ibqy] > 899) continue;
			/* if there is a commander, an no other base is appropriate,
			   we will take the one with the commander */
			for (j = 1; j <= g_d.remaining_commanders; j++) {
				if (ibqx==g_d.qx_commander[j] && ibqy==g_d.qy_commander[j] && ifindit!= 2) {
						ifindit = 2;
						iwhichb = i;
						break;
				}
			}
			if (j > g_d.remaining_commanders) { /* no commander -- use this one */
				ifindit = 1;
				iwhichb = i;
				break;
			}
		}
		if (ifindit==0) return; /* Nothing suitable -- wait until next time*/
		ibqx = g_d.qx_base[iwhichb];
		ibqy = g_d.qy_base[iwhichb];
		/* decide how to move toward base */
		ideltax = ibqx - g_d.qx_supercommander;
		ideltay = ibqy - g_d.qy_supercommander;
	}
	/* Maximum movement is 1 quadrant in either or both axis */
	if (ideltax > 1) ideltax = 1;
	if (ideltax < -1) ideltax = -1;
	if (ideltay > 1) ideltay = 1;
	if (ideltay < -1) ideltay = -1;

	/* try moving in both x and y directions */
	iqx = g_d.qx_supercommander + ideltax;
	iqy = g_d.qy_supercommander + ideltax;
	if (checkdest(iqx, iqy, flag, ipage)) {
		/* failed -- try some other maneuvers */
		if (ideltax==0 || ideltay==0) {
			/* attempt angle move */
			if (ideltax != 0) {
				iqy = g_d.qy_supercommander + 1;
				if (checkdest(iqx, iqy, flag, ipage)) {
					iqy = g_d.qy_supercommander - 1;
					checkdest(iqx, iqy, flag, ipage);
				}
			}
			else {
				iqx = g_d.qx_supercommander + 1;
				if (checkdest(iqx, iqy, flag, ipage)) {
					iqx = g_d.qx_supercommander - 1;
					checkdest(iqx, iqy, flag, ipage);
				}
			}
		}
		else {
			/* try moving just in x or y */
			iqy = g_d.qy_supercommander;
			if (checkdest(iqx, iqy, flag, ipage)) {
				iqy = g_d.qy_supercommander + ideltay;
				iqx = g_d.qx_supercommander;
				checkdest(iqx, iqy, flag, ipage);
			}
		}
	}
	/* check for a base */
	if (g_d.remaining_bases == 0) {
		future[FSCMOVE] = 1e30;
	}
	else for (i=1; i<=g_d.remaining_bases; i++) {
		ibqx = g_d.qx_base[i];
		ibqy = g_d.qy_base[i];
		if (ibqx==g_d.qx_supercommander && ibqy == g_d.qy_supercommander && g_d.qx_supercommander != batx && g_d.qy_supercommander != baty) {
			/* attack the base */
			if (flag) return; /* no, don't attack base! */
			has_seen_attack_report = 0;
			is_supercommander_attacking_base=1;
			future[FSCDBAS] = g_d.stardate + 1.0 +2.0*Rand();
			if (batx != 0) future[FSCDBAS] += future[FCDBAS]-g_d.stardate;
			if (!REPORTS)
				return; /* no warning */
			has_seen_attack_report = 1;
			if (*ipage == 0)  pause(1);
			*ipage=1;
			proutn("Lt. Uhura-  \"Captain, the starbase in");
			cramlc(1, g_d.qx_supercommander, g_d.qy_supercommander);
			skip(1);
			prout("   reports that it is under attack from the Klingon Super-commander.");
			proutn("   It can survive until stardate ");
			cramf(future[FSCDBAS], 0, 1);
			prout(" .\"");
			if (resting==0) return;
			prout("Mr. Spock-  \"Captain, shall we cancel the rest period?\"");
			if (ja()==0) return;
			resting = 0;
			Time = 0.0; /* actually finished */
			return;
		}
	}
	/* Check for intelligence report */
	if (
#ifdef DEBUG
		idebug==0 &&
#endif
		(Rand() > 0.2 ||
		 (!REPORTS) ||
		 starch[g_d.qx_supercommander][g_d.qy_supercommander] > 0))
		return;
	if (*ipage==0) pause(1);
	*ipage = 1;
	prout("Lt. Uhura-  \"Captain, Starfleet Intelligence reports");
	proutn("   the Super-commander is in");
	cramlc(1, g_d.qx_supercommander, g_d.qy_supercommander);
	prout(".\"");
	return;
}

void movetho(void) {
	int idx, idy, im, i, dum, my;
	/* Move the Tholean */
	if (currentq_has_tholian==0 || justin == 1) return;

	if (currentq_tholian_sx == 1 && currentq_tholian_sy == 1) {
		idx = 1; idy = 10;
	}
	else if (currentq_tholian_sx == 1 && currentq_tholian_sy == 10) {
		idx = 10; idy = 10;
	}
	else if (currentq_tholian_sx == 10 && currentq_tholian_sy == 10) {
		idx = 10; idy = 1;
	}
	else if (currentq_tholian_sx == 10 && currentq_tholian_sy == 1) {
		idx = 1; idy = 1;
	}
	else {
		/* something is wrong! */
		currentq_has_tholian = 0;
		return;
	}

	/* Do nothing if we are blocked */
	if (quad[idx][idy]!= IHDOT && quad[idx][idy]!= IH_THOLIAN_WEB) return;
	quad[currentq_tholian_sx][currentq_tholian_sy] = IH_THOLIAN_WEB;

	if (currentq_tholian_sx != idx) {
		/* move in x axis */
		im = (int)(fabs((double)idx - currentq_tholian_sx)/((double)idx - currentq_tholian_sx));
		while (currentq_tholian_sx != idx) {
			currentq_tholian_sx += im;
			if (quad[currentq_tholian_sx][currentq_tholian_sy]==IHDOT) quad[currentq_tholian_sx][currentq_tholian_sy] = IH_THOLIAN_WEB;
		}
	}
	else if (currentq_tholian_sy != idy) {
		/* move in y axis */
		im = (int)fabs(((double)idy - currentq_tholian_sy)/((double)idy - currentq_tholian_sy));
		while (currentq_tholian_sy != idy) {
			currentq_tholian_sy += im;
			if (quad[currentq_tholian_sx][currentq_tholian_sy]==IHDOT) quad[currentq_tholian_sx][currentq_tholian_sy] = IH_THOLIAN_WEB;
		}
	}
	quad[currentq_tholian_sx][currentq_tholian_sy] = IH_THOLIAN;

	/* check to see if all holes plugged */
	for (i = 1; i < 11; i++) {
		if (quad[1][i]!=IH_THOLIAN_WEB && quad[1][i]!=IH_THOLIAN) return;
		if (quad[10][i]!=IH_THOLIAN_WEB && quad[10][i]!=IH_THOLIAN) return;
		if (quad[i][1]!=IH_THOLIAN_WEB && quad[i][1]!=IH_THOLIAN) return;
		if (quad[i][10]!=IH_THOLIAN_WEB && quad[i][10]!=IH_THOLIAN) return;
	}
	/* All plugged up -- Tholian splits */
	quad[currentq_tholian_sx][currentq_tholian_sy]=IH_THOLIAN_WEB;
	dropin(IH_BLACK_HOLE, &dum, &my);
	crmena(1,IH_THOLIAN, 2, currentq_tholian_sx, currentq_tholian_sy);
	prout(" completes web.");
	currentq_has_tholian = currentq_tholian_sx = currentq_tholian_sy = 0;
	return;
}
