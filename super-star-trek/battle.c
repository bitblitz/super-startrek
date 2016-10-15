	#include "sst.h"

#ifdef CLOAKING
void cloak(void) {
	int key;
	enum {NONE, CLON, CLOFF} action = NONE;

	if (ship_name == IH_FAERIE_QUEEN) {
		prout("Ye Faerie Queene has no cloaking device.");
		return;
	}

	key = scan();

	if (key == IHREAL) return;

	if (key == IHALPHA) {
		if (isit("on")) {
			if (iscloaked) {
				prout("The cloaking device has already been switched on.");
				return;
			}
			action = CLON;
		}
		else if (isit("off")) {
			if (!iscloaked) {
				prout("The cloaking device has already been switched off.");
				return;
			}
			action = CLOFF;
		}
		else {
			huh();
			return;
		}
	} else {
		if (!iscloaked) {
			proutn("Switch cloaking device on?");
			if (ja()==0) return;
			action = CLON;
		}
		if (iscloaked) {
			proutn("Switch cloaking device off?");
			if (ja()==0) return;
			action = CLOFF;
		}
		if (action == NONE) return;
	}

    if (action==CLOFF) {
        if (currentq_num_romulans && g_d.stardate >= ALGERON && !is_cloak_violation_reported) {
            prout("Spock- \"Captain, the Treaty of Algeron is in effect.\n   Are you sure this is wise?\"");
            if (ja() == 0) return;
        }
		prout("Engineer Scott- \"Aye, Sir.\"");
        iscloaked = FALSE;
        if (currentq_num_romulans && g_d.stardate >= ALGERON && !is_cloak_violation_reported) {
            prout("The Romulan ship discovers you are breaking the Treaty of Algeron!");
            num_cloak_violations++;
            is_cloak_violation_reported = TRUE;
        }
            
//        if (in_neutral_zone && g_d.stardate >= ALGERON) finish(FCLOAK);
		return;
	}

	if (damage[DCLOAK]!=0) {
		prout("Engineer Scott- \"The cloaking device is damaged, Sir.\"");
		return;
	}

	if (ship_condition==IHDOCKED) {
		prout("You cannot cloak while docked.");
		return;
	}

	if (g_d.stardate >= ALGERON && !is_cloak_violation_reported)
	{
		prout("Spock- \"Captain, using the cloaking device is be a violation");
		prout("  of the Treaty of Algeron. Considering the alternatives,");
		proutn("  are you sure this is wise?");
		if (ja()==0) return;
	}

	prout("Engineer Scott- \"The cloaking device has been engaged, Sir.\"");
	iscloaking = TRUE;
    if (currentq_num_romulans && g_d.stardate >= ALGERON && !is_cloak_violation_reported) {
        prout("The Romulan ship discovers you are breaking the Treaty of Algeron!");
        num_cloak_violations++;
        is_cloak_violation_reported = TRUE;
    }
}
#endif

void sheild(int i) {
	int key;
	enum {NONE, SHUP, SHDN, NRG} action = NONE;

	ididit = 0;

	if (i == 2) action = SHUP;
	else {
		key = scan();
		if (key == IHALPHA) {
			if (isit("transfer"))
				action = NRG;
			else {
				chew();
				if (damage[DSHIELD]) {
					prout("Shields damaged and down.");
					return;
				}
				if (isit("up"))
					action = SHUP;
				else if (isit("down"))
					action = SHDN;
			}
		}
		if (action==NONE) {
			proutn("Do you wish to change shield energy? ");
			if (ja()) {
				proutn("Energy to transfer to shields- ");
				action = NRG;
			}
			else if (damage[DSHIELD]) {
				prout("Shields damaged and down.");
				return;
			}
			else if (is_shield_up) {
				proutn("Shields are up. Do you want them down? ");
				if (ja()) action = SHDN;
				else {
					chew();
					return;
				}
			}
			else {
				proutn("Shields are down. Do you want them up? ");
				if (ja()) action = SHUP;
				else {
					chew();
					return;
				}
			}
		}
	}
	switch (action) {
		case SHUP: /* raise shields */
			if (is_shield_up) {
				prout("Shields already up.");
				return;
			}
			is_shield_up = 1;
			is_shield_changing = 1;
			if (ship_condition != IHDOCKED) ship_energy -= 50.0;
			prout("Shields raised.");
			if (ship_energy <= 0) {
				skip(1);
				prout("Shields raising uses up last of energy.");
				finish(FNRG);
				return;
			}
			ididit=1;
			return;
		case SHDN:
			if (is_shield_up==0) {
				prout("Shields already down.");
				return;
			}
			is_shield_up=0;
			is_shield_changing=1;
			prout("Shields lowered.");
			ididit=1;
			return;
		case NRG:
			while (scan() != IHREAL) {
				chew();
				proutn("Energy to transfer to shields- ");
			}
			chew();
			if (aaitem==0) return;
			if (aaitem > ship_energy) {
				prout("Insufficient ship energy.");
				return;
			}
			ididit = 1;
			if (ship_shield_strength+aaitem >= ship_max_shield) {
				prout("Shield energy maximized.");
				if (ship_shield_strength+aaitem > ship_max_shield) {
					prout("Excess energy requested returned to ship energy");
				}
				ship_energy -= ship_max_shield-ship_shield_strength;
				ship_shield_strength = ship_max_shield;
				return;
			}
			if (aaitem < 0.0 && ship_energy-aaitem > ship_max_energy) {
				/* Prevent shield drain loophole */
				skip(1);
				prout("Engineering to bridge--");
				prout("  Scott here. Power circuit problem, Captain.");
				prout("  I can't drain the shields.");
				ididit = 0;
				return;
			}
			if (ship_shield_strength+aaitem < 0) {
				prout("All shield energy transferred to ship.");
				ship_energy += ship_shield_strength;
				ship_shield_strength = 0.0;
				return;
			}
			proutn("Scotty- \"");
			if (aaitem > 0)
				prout("Transferring energy to shields.\"");
			else
				prout("Draining energy from shields.\"");
			ship_shield_strength += aaitem;
			ship_energy -= aaitem;
			return;
		case NONE: break;
	}
}

void ram(int ibumpd, int ienm, int ix, int iy) {
	double type = 1.0, extradm;
	int icas, l;
	
	prouts("***RED ALERT!  RED ALERT!");
	skip(1);
	prout("***COLLISION IMMINENT.");
	skip(2);
	proutn("***");
	crmshp();
	switch (ienm) {
		case IH_ROMULAN: type = 1.5; break;
		case IH_COMMANDER: type = 2.0; break;
		case IH_SUPER_COMMANDER: type = 2.5; break;
		case IH_THOLIAN: type = 0.5; break;
	}
	proutn(ibumpd ? " rammed by " : " rams ");
	crmena(0, ienm, 2, ix, iy);
	if (ibumpd) proutn(" (original position)");
	skip(1);
	deadkl(ix, iy, ienm, sectx, secty);
	proutn("***");
	crmshp();
	prout(" heavily damaged.");
	icas = (int)(10.0+20.0*Rand());
	proutn("***Sickbay reports ");
	crami(icas, 1);
	prout(" casualties.");
	num_casualties += icas;
	for (l=1; l <= ndevice; l++) {
		if (l == DDRAY) continue; // Don't damage deathray 
		if (damage[l] < 0) continue;
		extradm = (10.0*type*Rand()+1.0)*game_damage_factor;
		damage[l] += Time + extradm; /* Damage for at least time of travel! */
	}
	is_shield_up = 0;
	if (g_d.remaining_klingons) {
		pause(2);
		dreprt();
	}
	else finish(FWON);
	return;
}

void torpedo(double course, double r, int inx, int iny, double *hit) {
	int l, iquad, ix, iy,  jx, jy, shoved=0, ll;
	double ac=course + 0.25*r;
	double angle = (15.0-ac)*0.5235988;
	double bullseye = (15.0 - course)*0.5235988;
	double deltax=-sin(angle), deltay=cos(angle), x=inx, y=iny, bigger;
	double ang, temp, xx, yy, kp, h1;

	bigger = fabs(deltax);
	if (fabs(deltay) > bigger) bigger = fabs(deltay);
	deltax /= bigger;
	deltay /= bigger;

	/* Loop to move a single torpedo */
	for (l=1; l <= 15; l++) {
		x += deltax;
		ix = (int)(x + 0.5);
		if (ix < 1 || ix > 10) break;
		y += deltay;
		iy = (int)(y + 0.5);
		if (iy < 1 || iy > 10) break;
		if (l==4 || l==9) skip(1);
		cramf(x, 0, 1);
		proutn(" - ");
		cramf(y, 0, 1);
		proutn("   ");
		iquad=quad[ix][iy];
		if (iquad==IHDOT) continue;
		/* hit something */
		skip(1);
		switch(iquad) {
			case IH_ENTERPRISE: /* Hit our ship */
			case IH_FAERIE_QUEEN:
				skip(1);
				proutn("Torpedo hits ");
				crmshp();
				prout(".");
				*hit = 700.0 + 100.0*Rand() -
					   1000.0*sqrt(square(ix-inx)+square(iy-iny))*
					   fabs(sin(bullseye-angle));
				*hit = fabs(*hit);
				newcnd(); /* undock */
				/* We may be displaced. */
				if (is_landed==1) return; /* Cheat if on a planet */
				ang = angle + 2.5*(Rand()-0.5);
				temp = fabs(sin(ang));
				if (fabs(cos(ang)) > temp) temp = fabs(cos(ang));
				xx = -sin(ang)/temp;
				yy = cos(ang)/temp;
				jx=(int)(ix+xx+0.5);
				jy=(int)(iy+yy+0.5);
				if (jx<1 || jx>10 || jy<1 ||jy > 10) return;
				if (quad[jx][jy]==IH_BLACK_HOLE) {
					finish(FHOLE);
					return;
				}
				if (quad[jx][jy]!=IHDOT) {
					/* can't move into object */
					return;
				}
				sectx = jx;
				secty = jy;
				crmshp();
				shoved = 1;
				break;
					  
			case IH_COMMANDER: /* Hit a commander */
			case IH_SUPER_COMMANDER:
				if (Rand() <= 0.05) {
					crmena(1, iquad, 2, ix, iy);
					prout(" uses anti-photon device;");
					prout("   torpedo neutralized.");
					return;
				}
			case IH_ROMULAN: /* Hit a regular enemy */
			case IH_KLINGON:
				/* find the enemy */
				for (ll=1; ll <= currentq_num_enemies; ll++)
					if (ix==kx[ll] && iy==ky[ll]) break;
				kp = fabs(kpower[ll]);
				h1 = 700.0 + 100.0*Rand() -
					 1000.0*sqrt(square(ix-inx)+square(iy-iny))*
					 fabs(sin(bullseye-angle));
				h1 = fabs(h1);
				if (kp < h1) h1 = kp;
				kpower[ll] -= (kpower[ll]<0 ? -h1 : h1);
				if (kpower[ll] == 0) {
					deadkl(ix, iy, iquad, ix, iy);
					return;
				}
				crmena(1, iquad, 2, ix, iy);
				/* If enemy damaged but not destroyed, try to displace */
				ang = angle + 2.5*(Rand()-0.5);
				temp = fabs(sin(ang));
				if (fabs(cos(ang)) > temp) temp = fabs(cos(ang));
				xx = -sin(ang)/temp;
				yy = cos(ang)/temp;
				jx=(int)(ix+xx+0.5);
				jy=(int)(iy+yy+0.5);
				if (jx<1 || jx>10 || jy<1 ||jy > 10) {
					prout(" damaged but not destroyed.");
					return;
				}
				if (quad[jx][jy]==IH_BLACK_HOLE) {
					prout(" buffeted into black hole.");
					deadkl(ix, iy, iquad, jx, jy);
					return;
				}
				if (quad[jx][jy]!=IHDOT) {
					/* can't move into object */
					prout(" damaged but not destroyed.");
					return;
				}
				prout(" damaged--");
				kx[ll] = jx;
				ky[ll] = jy;
				shoved = 1;
				break;
			case IH_BASE: /* Hit a base */
				prout("***STARBASE DESTROYED..");
				if (starch[quadx][quady] < 0) starch[quadx][quady] = 0;
				for (ll=1; ll<=g_d.remaining_bases; ll++) {
					if (g_d.qx_base[ll]==quadx && g_d.qy_base[ll]==quady) {
						g_d.qx_base[ll]=g_d.qx_base[g_d.remaining_bases];
						g_d.qy_base[ll]=g_d.qy_base[g_d.remaining_bases];
						break;
					}
				}
				quad[ix][iy]=IHDOT;
				g_d.remaining_bases--;
				currentq_base_sx=currentq_base_sy=0;
				g_d.galaxy[quadx][quady] -= 10;
				g_d.killed_bases++;
				newcnd();
				return;
			case IH_PLANET: /* Hit a planet */
				crmena(1, iquad, 2, ix, iy);
				prout(" destroyed.");
				g_d.killed_planets++;
				g_d.newstuf[quadx][quady] -= 1;
				g_d.plnets[currentq_planet_id] = nulplanet;
				currentq_planet_id = 0;
				currentq_planet_sx = currentq_planet_sy = 0;
				quad[ix][iy] = IHDOT;
				if (is_landed==1) {
					/* captain parishes on planet */
					finish(FDPLANET);
				}
				return;
			case IH_STAR: /* Hit a star */
				if (Rand() > 0.10) {
					nova(ix, iy);
					return;
				}
				crmena(1, IH_STAR, 2, ix, iy);
				prout(" unaffected by photon blast.");
				return;
			case IHQUEST: /* Hit a qy_thing */
				skip(1);
				prouts("AAAAIIIIEEEEEEEEAAAAAAAAUUUUUGGGGGHHHHHHHHHHHH!!!");
				skip(1);
				prouts("    HACK!     HACK!    HACK!        *CHOKE!*  ");
				skip(1);
				proutn("Mr. Spock-");
				prouts("  \"Facinating!\"");
				skip(1);
				quad[ix][iy] = IHDOT;
				return;
			case IH_BLACK_HOLE: /* Black hole */
				skip(1);
				crmena(1, IH_BLACK_HOLE, 2, ix, iy);
				prout(" swallows torpedo.");
				return;
			case IH_THOLIAN_WEB: /* hit the web */
				skip(1);
				prout("***Torpedo absorbed by Tholian web.");
				return;
			case IH_THOLIAN:  /* Hit a Tholian */
				skip(1);
				crmena(1, IH_THOLIAN, 2, ix, iy);
				h1 = 700.0 + 100.0*Rand() -
					 1000.0*sqrt(square(ix-inx)+square(iy-iny))*
					 fabs(sin(bullseye-angle));
				h1 = fabs(h1);
				if (h1 >= 600) {
					prout(" destroyed.");
					quad[ix][iy] = IHDOT;
					currentq_has_tholian = 0;
					currentq_tholian_sx = currentq_tholian_sy = 0;
					return;
				}
				if (Rand() > 0.05) {
					prout(" survives photon blast.");
					return;
				}
				prout(" disappears.");
				quad[ix][iy] = IH_THOLIAN_WEB;
				currentq_has_tholian = currentq_tholian_sx = currentq_tholian_sy = 0;
				{
					int dum, my;
					dropin(IH_BLACK_HOLE, &dum, &my);
				}
				return;
					
			default: /* Problem! */
				skip(1);
				proutn("Don't know how to handle collision with ");
				crmena(1, iquad, 2, ix, iy);
				skip(1);
				return;
		}
		break;
	}
	if (shoved) {
		quad[jx][jy]=iquad;
		quad[ix][iy]=IHDOT;
		proutn(" displaced by blast to");
		cramlc(2, jx, jy);
		skip(1);
		for (ll=1; ll<=currentq_num_enemies; ll++)
			kdist[ll] = kavgd[ll] = sqrt(square(sectx-kx[ll])+square(secty-ky[ll]));
		sortkl();
		return;
	}
	skip(1);
	prout("Torpedo missed.");
	return;
}

static void fry(double hit) {
	double ncrit, extradm;
	int ktr=1, l, ll, j, cdam[6];//, crptr;

	/* a critical hit occured */
	if (hit < (275.0-25.0*game_skill)*(1.0+0.5*Rand())) return;

	ncrit = 1.0 + hit/(500.0+100.0*Rand());
	proutn("***CRITICAL HIT--");
	/* Select devices and cause damage */
	for (l = 1; l <= ncrit; l++) {
		do {
			j = (int)(ndevice*Rand()+1.0);
			/* Cheat to prevent shuttle damage unless on ship */
		} while (damage[j] < 0.0 || (j == DSHUTTL && has_suttlecraft != 1) ||
#ifdef CLOAKING
				 (j == DCLOAK && ship_name != IH_ENTERPRISE) ||
#endif
				 j == DDRAY);
		cdam[l] = j;
		extradm = (hit*game_damage_factor)/(ncrit*(75.0+25.0*Rand()));
		damage[j] += extradm;
		if (l > 1) {
			for (ll=2; ll<=l && j != cdam[ll-1]; ll++) ;
			if (ll<=l) continue;
			ktr += 1;
			if (ktr==3) skip(1);
			proutn(" and ");
		}
		proutn(device[j]);
	}
	prout(" damaged.");
	if (damage[DSHIELD] && is_shield_up) {
		prout("***Shields knocked down.");
		is_shield_up=0;
	}
#ifdef CLOAKING
	if (damage[DCLOAK] && iscloaked)
	{
		prout("***Cloaking device rendered inoperative.");
		iscloaked = FALSE;
	}
#endif
}

void attack(int k) {
	/* k == 0 forces use of phasers in an attack */
	int percent, ihurt=0, l, i=0, jx, jy, iquad, itflag;
	int atackd = 0, attempt = 0;
	double hit;
	double pfac, dustfac, hitmax=0.0, hittot=0.0, chgfac=1.0, r;

#ifdef CLOAKING
    if (iscloaked && !iscloaking) return; // Nothing happens if we are cloaked
#endif
    
	iattak = 1;
	if (alldone) return;
#ifdef DEBUG
	if (idebug) prout("ATTACK!");
#endif

	if (currentq_has_tholian) movetho();

	if (in_neutral_zone) { /* The one chance not to be attacked */
		in_neutral_zone = 0;
		return;
	}
	if (((currentq_num_commanders || currentq_has_supercommander) && (justin == 0)) || game_skill == SEMERITUS) movcom();
	if (currentq_num_enemies==0) return;
	pfac = 1.0/ship_max_shield;
	if (is_shield_changing == 1) chgfac = 0.25+0.5*Rand();
	skip(1);
	if (game_skill <= SFAIR) i = 2;
	for (l=1; l <= currentq_num_enemies; l++) {
		if (kpower[l] < 0) continue;	/* too weak to attack */
		/* compute hit strength and diminsh shield power */
		r = Rand();
		/* Increase chance of photon torpedos if docked or enemy energy low */
		if (ship_condition == IHDOCKED) r *= 0.25;
		if (kpower[l] < 500) r *= 0.25; 
		jx = kx[l];
		jy = ky[l];
		iquad = quad[jx][jy];
		itflag = (iquad == IH_KLINGON && r > 0.0005) || k == 0 ||
			(iquad==IH_COMMANDER && r > 0.015) ||
			(iquad==IH_ROMULAN && r > 0.3) ||
			(iquad==IH_SUPER_COMMANDER && r > 0.07);
		if (itflag) {
			/* Enemy uses phasers */
			if (ship_condition == IHDOCKED) continue; /* Don't waste the effort! */
			attempt = 1; /* Attempt to attack */
			dustfac = 0.8+0.05*Rand();
			hit = kpower[l]*pow(dustfac,kavgd[l]);
			kpower[l] *= 0.75;
		}
		else { /* Enemy used photon torpedo */
			double course = 1.90985*atan2((double)secty-jy, (double)jx-sectx);
			hit = 0;
			proutn("***TORPEDO INCOMING");
			if (damage[DSRSENS] <= 0.0) {
				proutn(" From ");
				crmena(0, iquad, i, jx, jy);
			}
			attempt = 1;
			prout("--");
			r = (Rand()+Rand())*0.5 -0.5;
			r += 0.002*kpower[l]*r;
			torpedo(course, r, jx, jy, &hit);
			if (g_d.remaining_klingons==0) finish(FWON); /* Klingons did themselves in! */
			if (g_d.galaxy[quadx][quady] == 1000 ||
				alldone) return; /* Supernova or finished */
			if (hit == 0) continue;
		}
		if (is_shield_up != 0 || is_shield_changing != 0) {
			/* shields will take hits */
			double absorb, hitsh, propor = pfac*ship_shield_strength;
			if(propor < 0.1) propor = 0.1;
			hitsh = propor*chgfac*hit+1.0;
			atackd=1;
			absorb = 0.8*hitsh;
			if (absorb > ship_shield_strength) absorb = ship_shield_strength;
			ship_shield_strength -= absorb;
			hit -= hitsh;
			if (propor > 0.1 && hit < 0.005*ship_energy) continue;
		}
		/* It's a hit -- print out hit size */
		atackd = 1; /* We weren't going to check casualties, etc. if
		               shields were down for some strange reason. This
					   doesn't make any sense, so I've fixed it */
		ihurt = 1;
		cramf(hit, 0, 2);
		proutn(" unit hit");
		if ((damage[DSRSENS] > 0 && itflag) || game_skill <= SFAIR) {
			proutn(" on the ");
			crmshp();
		}
		if (damage[DSRSENS] <= 0.0 && itflag) {
			proutn(" from ");
			crmena(0, iquad, i, jx, jy);
		}
		skip(1);
		/* Decide if hit is critical */
		if (hit > hitmax) hitmax = hit;
		hittot += hit;
		fry(hit);
		printf("Hit %g energy %g\n", hit, ship_energy);
		ship_energy -= hit;
	}
	if (ship_energy <= 0) {
		/* Returning home upon your shield, not with it... */
		finish(FBATTLE);
		return;
	}
	if (attempt == 0 && ship_condition == IHDOCKED)
		prout("***Enemies decide against attacking your ship.");
	if (atackd == 0) return;
	percent = (int)(100.0*pfac*ship_shield_strength+0.5);
	if (ihurt==0) {
		/* Shields fully protect ship */
		proutn("Enemy attack reduces shield strength to ");
	}
	else {
		/* Print message if starship suffered hit(s) */
		skip(1);
		proutn("Energy left ");
		cramf(ship_energy, 0, 2);
		proutn("    shields ");
		if (is_shield_up) proutn("up, ");
		else if (damage[DSHIELD] == 0) proutn("down, ");
		else proutn("damaged, ");
	}
	crami(percent, 1);
	proutn("%   torpedoes left ");
	crami(torps, 1);
	skip(1);
	/* Check if anyone was hurt */
	if (hitmax >= 200 || hittot >= 500) {
		int icas= (int)(hittot*Rand()*0.015);
		if (icas >= 2) {
			skip(1);
			proutn("Mc Coy-  \"Sickbay to bridge.  We suffered ");
			crami(icas, 1);
			prout(" casualties");
			prout("   in that last attack.\"");
			num_casualties += icas;
		}
	}
	/* After attack, reset average distance to enemies */
	for (l = 1; l <= currentq_num_enemies; l++)
		kavgd[l] = kdist[l];
	sortkl();
	return;
}
		
void deadkl(int ix, int iy, int type, int ixx, int iyy) {
	/* Added ixx and iyy allow enemy to "move" before dying */

	int i,j;
	
	crmena(1, type, 2, ixx, iyy);
	/* Decide what kind of enemy it is and update approriately */
	if (type == IH_ROMULAN) {
		/* chalk up a Romulan */
		g_d.newstuf[quadx][quady] -= 10;
		currentq_num_romulans--;
		g_d.killed_romulans++;
		g_d.remaining_romulans--;
	}
	else if (type == IH_THOLIAN) {
		/* Killed a Tholean */
		currentq_has_tholian = 0;
	}
	else {
		/* Some type of a Klingon */
		g_d.galaxy[quadx][quady] -= 100;
		currentq_num_klingons--;
		g_d.remaining_klingons--;
		switch (type) {
			case IH_COMMANDER:
				currentq_num_commanders = 0;
				for (i=1; i<=g_d.remaining_commanders; i++)
					if (g_d.qx_commander[i]==quadx && g_d.qy_commander[i]==quady) break;
				g_d.qx_commander[i] = g_d.qx_commander[g_d.remaining_commanders];
				g_d.qy_commander[i] = g_d.qy_commander[g_d.remaining_commanders];
				g_d.qx_commander[g_d.remaining_commanders] = 0;
				g_d.qy_commander[g_d.remaining_commanders] = 0;
				g_d.remaining_commanders--;
				future[FTBEAM] = 1e30;
				if (g_d.remaining_commanders != 0)
					future[FTBEAM] = g_d.stardate + expran(1.0*initial_commanders/g_d.remaining_commanders);
				g_d.killed_commanders++;
				break;
			case IH_KLINGON:
				g_d.killed_klingons++;
				break;
			case IH_SUPER_COMMANDER:
				g_d.remaining_supercommanders = currentq_has_supercommander = g_d.qx_supercommander = g_d.qy_supercommander = is_supercommander_attacking_base = currentq_is_supercommander_here = 0;
				g_d.killed_supercommanders = 1;
				future[FSCMOVE] = future[FSCDBAS] = 1e30;
				break;
		}
	}

	/* For each kind of enemy, finish message to player */
	prout(" destroyed.");
	quad[ix][iy] = IHDOT;
	if (g_d.remaining_klingons==0) return;

	g_d.remaining_time = g_d.remaining_resources/(g_d.remaining_klingons + 4*g_d.remaining_commanders);

	if (type == IH_THOLIAN) return;

	/* Remove enemy ship from arrays describing local conditions */

	for (i=1; i<=currentq_num_enemies; i++)
		if (kx[i]==ix && ky[i]==iy) break;
	currentq_num_enemies--;
	if (i <= currentq_num_enemies)  {
		for (j=i; j<=currentq_num_enemies; j++) {
			kx[j] = kx[j+1];
			ky[j] = ky[j+1];
			kpower[j] = kpower[j+1];
			kavgd[j] = kdist[j] = kdist[j+1];
		}
	}
	kx[currentq_num_enemies+1] = 0;
	ky[currentq_num_enemies+1] = 0;
	kdist[currentq_num_enemies+1] = 0;
	kavgd[currentq_num_enemies+1] = 0;
	kpower[currentq_num_enemies+1] = 0;
	return;
}

static int targetcheck(double x, double y, double *course) {
	double deltx, delty;
	/* Return TRUE if target is invalid */
	if (x < 1.0 || x > 10.0 || y < 1.0 || y > 10.0) {
		huh();
		return 1;
	}
	deltx = 0.1*(y - secty);
	delty = 0.1*(sectx - x);
	if (deltx==0 && delty== 0) {
		skip(1);
		prout("Spock-  \"Bridge to sickbay.  Dr. McCoy,");
		prout("  I recommend an immediate review of");
		prout("  the Captain's psychological profile.");
		chew();
		return 1;
	}
	*course = 1.90985932*atan2(deltx, delty);
	return 0;
}

void photon(void) {
	double targ[4][3], course[4];
	double r, dummy;
	int key, n, i, osuabor;

	ididit = 0;

	if (damage[DPHOTON]) {
		prout("Photon tubes damaged.");
		chew();
		return;
	}
	if (torps == 0) {
		prout("No torpedoes left.");
		chew();
		return;
	}
	key = scan();
	for (;;) {
		if (key == IHALPHA) {
			huh();
			return;
		}
		else if (key == IHEOL) {
			crami(torps,1);
			prout(" torpedoes left.");
			proutn("Number of torpedoes to fire- ");
			key = scan();
		}
		else /* key == IHREAL */ {
			n = (int)(aaitem + 0.5);
			if (n <= 0) { /* abort command */
				chew();
				return;
			}
			if (n > 3) {
				prout("Maximum of 3 torpedoes per burst.");
			} else if (n <= torps) break;
			chew();
			key = IHEOL;
		}
	}
	for (i = 1; i <= n; i++) {
		key = scan();
		if (i==1 && key == IHEOL) {
			break;	/* we will try prompting */
		}
		if (i==2 && key == IHEOL) {
			/* direct all torpedoes at one target */
			while (i <= n) {
				targ[i][1] = targ[1][1];
				targ[i][2] = targ[1][2];
				course[i] = course[1];
				i++;
			}
			break;
		}
		if (key != IHREAL) {
			huh();
			return;
		}
		targ[i][1] = aaitem;
		key = scan();
		if (key != IHREAL) {
			huh();
			return;
		}
		targ[i][2] = aaitem;
		if (targetcheck(targ[i][1], targ[i][2], &course[i])) return;
	}
	chew();
	if (i == 1 && key == IHEOL) {
		/* prompt for each one */
		for (i = 1; i <= n; i++) {
			proutn("Target sector for torpedo number");
			crami(i, 2);
			proutn("- ");
			key = scan();
			if (key != IHREAL) {
				huh();
				return;
			}
			targ[i][1] = aaitem;
			key = scan();
			if (key != IHREAL) {
				huh();
				return;
			}
			targ[i][2] = aaitem;
			chew();
			if (targetcheck(targ[i][1], targ[i][2], &course[i])) return;
		}
	}
	ididit = 1;
	/* Loop for moving <n> torpedoes */
	osuabor = 0;
	for (i = 1; i <= n && !osuabor; i++) {
		if (ship_condition != IHDOCKED) torps--;
		r = (Rand()+Rand())*0.5 -0.5;
		if (fabs(r) >= 0.47) {
			/* misfire! */
			r = (Rand()+1.2) * r;
			if (n>1) {
				prouts("***TORPEDO NUMBER");
				crami(i, 2);
				prouts(" MISFIRES.");
			}
			else prouts("***TORPEDO MISFIRES.");
			skip(1);
			if (i < n)
				prout("  Remainder of burst aborted.");
			osuabor=1;
			if (Rand() <= 0.2) {
				prout("***Photon tubes damaged by misfire.");
				damage[DPHOTON] = game_damage_factor*(1.0+2.0*Rand());
				break;
			}
		}
#ifdef CLOAKING
		if (iscloaked) r *= 1.2; /* Torpedoes are less accurate */
		else
#endif
		if (is_shield_up != 0 || ship_condition == IHDOCKED) r *= 1.0 + 0.0001*ship_shield_strength; /* Torpedos are less accurate */

		if (n != 1) {
			skip(1);
			proutn("Track for torpedo number");
			crami(i, 2);
			proutn("-   ");
		}
		else {
			skip(1);
			proutn("Torpedo track- ");
		}
		torpedo(course[i], r, sectx, secty, &dummy);
		if (alldone || g_d.galaxy[quadx][quady]==1000) return;
	}
	if (g_d.remaining_klingons==0) finish(FWON);
}

	

static void overheat(double rpow) {
	if (rpow > 1500) {
		double chekbrn = (rpow-1500.)*0.00038;
		if (Rand() <= chekbrn) {
			prout("Weapons officer Sulu-  \"Phasers overheated, sir.\"");
			damage[DPHASER] = game_damage_factor*(1.0 + Rand()) * (1.0+chekbrn);
		}
	}
}

static int checkshctrl(double rpow) {
	double hit;
	int icas;
	
	skip(1);
	if (Rand() < .998) {
		prout("Shields lowered.");
		return 0;
	}
	/* Something bad has happened */
	prouts("***RED ALERT!  RED ALERT!");
	skip(2);
	hit = rpow*ship_shield_strength/ship_max_shield;
	ship_energy -= rpow+hit*0.8;
	ship_shield_strength -= hit*0.2;
	if (ship_energy <= 0.0) {
		prouts("Sulu-  \"Captain! Shield malf***********************\"");
		skip(1);
		stars();
		finish(FPHASER);
		return 1;
	}
	prouts("Sulu-  \"Captain! Shield malfunction! Phaser fire contained!\"");
	skip(2);
	prout("Lt. Uhura-  \"Sir, all decks reporting damage.\"");
	icas = (int)(hit*Rand()*0.012);
	skip(1);
	fry(0.8*hit);
	if (icas) {
		skip(1);
		prout("McCoy to bridge- \"Severe radiation burns, Jim.");
		proutn("  ");
		crami(icas, 1);
		prout(" casualties so far.\"");
		num_casualties += icas; // Changed from -=, October 2013
	}
	skip(1);
	prout("Phaser energy dispersed by shields.");
	prout("Enemy unaffected.");
	overheat(rpow);
	return 1;
}
	

void phasers(void) {
	double hits[21], rpow, extra, powrem, over, temp;
	int kz = 0, k=1, i; /* Cheating inhibitor */
	int ifast=0, no=0, ipoop=1, msgflag = 1;
	enum {NOTSET, MANUAL, FORCEMAN, AUTOMATIC} automode = NOTSET;
	int key;

	skip(1);
	/* SR sensors and Computer */
	if (damage[DSRSENS]+damage[DCOMPTR] > 0) ipoop = 0;
	if (ship_condition == IHDOCKED) {
		prout("Phasers can't be fired through base shields.");
		chew();
		return;
	}
	if (damage[DPHASER] != 0) {
		prout("Phaser control damaged.");
		chew();
		return;
	}
	if (is_shield_up) {
		if (damage[DSHCTRL]) {
			prout("High speed shield control damaged.");
			chew();
			return;
		}
		if (ship_energy <= 200.0) {
			prout("Insufficient energy to activate high-speed shield control.");
			chew();
			return;
		}
		prout("Weapons Officer Sulu-  \"High-speed shield control enabled, sir.\"");
		ifast = 1;
		
	}
	ididit = 1;
	/* Original code so convoluted, I re-did it all */
	while (automode==NOTSET) {
		key=scan();
		if (key == IHALPHA) {
			if (isit("manual")) {
				if (currentq_num_enemies==0) {
					prout("There is no enemy present to select.");
					chew();
					key = IHEOL;
					automode=AUTOMATIC;
				}
				else {
					automode = MANUAL;
					key = scan();
				}
			}
			else if (isit("automatic")) {
				if ((!ipoop) && currentq_num_enemies != 0) {
					automode = FORCEMAN;
				}
				else {
					if (currentq_num_enemies==0)
						prout("Energy will be expended into space.");
					automode = AUTOMATIC;
					key = scan();
				}
			}
			else if (isit("no")) {
				no = 1;
			}
			else {
				huh();
				ididit = 0;
				return;
			}
		}
		else if (key == IHREAL) {
			if (currentq_num_enemies==0) {
				prout("Energy will be expended into space.");
				automode = AUTOMATIC;
			}
			else if (!ipoop)
				automode = FORCEMAN;
			else
				automode = AUTOMATIC;
		}
		else {
			/* IHEOL */
			if (currentq_num_enemies==0) {
				prout("Energy will be expended into space.");
				automode = AUTOMATIC;
			}
			else if (!ipoop)
				automode = FORCEMAN;
			else 
			proutn("Manual or automatic? ");
		}
	}
				
	switch (automode) {
		case AUTOMATIC:
			if (key == IHALPHA && isit("no")) {
				no = 1;
				key = scan();
			}
			if (key != IHREAL && currentq_num_enemies != 0) {
				proutn("Phasers locked on target. Energy available =");
				cramf(ifast?ship_energy-200.0:ship_energy,1,2);
				skip(1);
			}
			do {
				while (key != IHREAL) {
					chew();
					proutn("Units to fire=");
					key = scan();
				}
				rpow = aaitem;
				if (rpow >= (ifast?ship_energy-200:ship_energy)) {
					proutn("Energy available= ");
					cramf(ifast?ship_energy-200:ship_energy, 1,2);
					skip(1);
					key = IHEOL;
				}
			} while (rpow >= (ifast?ship_energy-200:ship_energy));
			if (rpow<=0) {
				/* chicken out */
				ididit = 0;
				chew();
				return;
			}
			if ((key=scan()) == IHALPHA && isit("no")) {
				no = 1;
			}
			if (ifast) {
				ship_energy -= 200; /* Go and do it! */
				if (checkshctrl(rpow)) return;
			}
			chew();
			ship_energy -= rpow;
			extra = rpow;
			if (currentq_num_enemies) {
				extra = 0.0;
				powrem = rpow;
				for (i = 1; i <= currentq_num_enemies; i++) {
					hits[i] = 0.0;
					if (powrem <= 0) continue;
					hits[i] = fabs(kpower[i])/(PHASER_DAMAGE_FACTOR*pow(0.90,kdist[i]));
					over = (0.01 + 0.05*Rand())*hits[i];
					temp = powrem;
					powrem -= hits[i] + over;
					if (powrem <= 0 && temp < hits[i]) hits[i] = temp;
					if (powrem <= 0) over = 0.0;
					extra += over;
				}
				if (powrem > 0.0) extra += powrem;
				hittem(hits);
			}
			if (extra > 0 && alldone == 0) {
				if (currentq_has_tholian) {
					proutn("*** Tholian web absorbs ");
					if (currentq_num_enemies>0) proutn("excess ");
					prout("phaser energy.");
				}
				else {
					cramf(extra, 0, 2);
					prout(" expended on empty space.");
				}
			}
			break;

		case FORCEMAN:
			chew();
			key = IHEOL;
			if (damage[DCOMPTR]!=0)
				prout("Battle computer damaged, manual file only.");
			else {
				skip(1);
				prouts("---WORKING---");
				skip(1);
				prout("Short-range-sensors-damaged");
				prout("Insufficient-data-for-automatic-phaser-fire");
				prout("Manual-fire-must-be-used");
				skip(1);
			}
		case MANUAL:
			rpow = 0.0;
			for (k = 1; k <= currentq_num_enemies;) {
				int ii = kx[k], jj = ky[k];
				int ienm = quad[ii][jj];
				if (msgflag) {
					proutn("Energy available= ");
					cramf(ship_energy-.006-(ifast?200:0), 0, 2);
					skip(1);
					msgflag = 0;
					rpow = 0.0;
				}
				if (damage[DSRSENS] && !(abs(sectx-ii) < 2 && abs(secty-jj) < 2) &&
					(ienm == IH_COMMANDER || ienm == IH_SUPER_COMMANDER)) {
					cramen(ienm);
					prout(" can't be located without short range scan.");
					chew();
					key = IHEOL;
					hits[k] = 0; /* prevent overflow -- thanks to Alexei Voitenko */
					k++;
					continue;
				}
				if (key == IHEOL) {
					chew();
					if (ipoop && k > kz) {
						int irec=(int)((fabs(kpower[k])/(PHASER_DAMAGE_FACTOR*pow(0.9,kdist[k])))*
								 (1.01+0.05*Rand()) + 1.0);
						kz = k;
						proutn("(");
						crami(irec, 1);
						proutn(")  ");
					}
					proutn("units to fire at ");
					crmena(0, ienm, 2, ii, jj);
					proutn("-  ");
					key = scan();
				}
				if (key == IHALPHA && isit("no")) {
					no = 1;
					key = scan();
					continue;
					}
				if (key == IHALPHA) {
					huh();
					ididit = 0;
					return;
				}
				if (key == IHEOL) {
					if (k==1) { /* Let me say I'm baffled by this */
						msgflag = 1;
					}
					continue;
				}
				if (aaitem < 0) {
					/* abort out */
					ididit = 0;
					chew();
					return;
				}
				hits[k] = aaitem;
				rpow += aaitem;
				/* If total requested is too much, inform and start over */
				
				if (rpow >= (ifast?ship_energy-200:ship_energy)) {
					prout("Available energy exceeded -- try again.");
					chew();
					key = IHEOL;
					k = 1;
					msgflag = 1;
					continue;
				}
				key = scan(); /* scan for next value */
				k++;
			}
			if (rpow == 0.0) {
				/* zero energy -- abort */
				ididit = 0;
				chew();
				return;
			}
			if (key == IHALPHA && isit("no")) {
				no = 1;
			}
			ship_energy -= rpow;
			chew();
			if (ifast) {
				ship_energy -= 200.0;
				if (checkshctrl(rpow)) return;
			}
			hittem(hits);
			ididit=1;
			break;
			case NOTSET: break; // cannot occur
	}
	/* Say shield raised or malfunction, if necessary */
	if (alldone) return;
	if (ifast) {
		skip(1);
		if (no == 0) {
			if (Rand() >= 0.99) {
				prout("Sulu-  \"Sir, the high-speed shield control has malfunctioned . . .");
				prouts("         CLICK   CLICK   POP  . . .");
				prout(" No  response, sir!");
				is_shield_up = 0;
			}
			else
				prout("Shields raised.");
		}
		else
			is_shield_up = 0;
	}
	overheat(rpow);
}

void hittem(double *hits) {
	double kp, kpow, wham, hit, dustfac, kpini;
	int nenhr2=currentq_num_enemies, k=1, kk=1, ii, jj, ienm;

	skip(1);

	for (; k <= nenhr2; k++, kk++) {
		if ((wham = hits[k])==0) continue;
		dustfac = 0.9 + 0.01*Rand();
		hit = wham*pow(dustfac,kdist[kk]);
		kpini = kpower[kk];
		kp = fabs(kpini);
		if (PHASER_DAMAGE_FACTOR*hit < kp) kp = PHASER_DAMAGE_FACTOR*hit;
		kpower[kk] -= (kpower[kk] < 0 ? -kp: kp);
		kpow = kpower[kk];
		ii = kx[kk];
		jj = ky[kk];
		if (hit > 0.005) {
			cramf(hit, 0, 2);
			proutn(" unit hit on ");
		}
		else
			proutn("Very small hit on ");
		ienm = quad[ii][jj];
		crmena(0,ienm,2,ii,jj);
		skip(1);
		if (kpow == 0) {
			deadkl(ii, jj, ienm, ii, jj);
			if (g_d.remaining_klingons==0) finish(FWON);
			if (alldone) return;
			kk--; /* don't do the increment */
		}
		else /* decide whether or not to emasculate klingon */
			if (kpow > 0 && Rand() >= 0.9 &&
				kpow <= ((0.4 + 0.4*Rand())*kpini)) {
				proutn("***Mr. Spock-  \"Captain, the vessel at");
				cramlc(2,ii,jj);
				skip(1);
				prout("   has just lost its firepower.\"");
				kpower[kk] = -kpow;
			}
	}
	return;
}

#ifdef CAPTURE
/*	$NetBSD: capture.c,v 1.6 2003/08/07 09:37:50 agc Exp $	*/

/*
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
**  Ask a Klingon To Surrender
**
**	(Fat chance)
**
**	The Subspace Radio is needed to ask a Klingon if he will kindly
**	surrender.  A random Klingon from the ones in the quadrant is
**	chosen.
**
**	The Klingon is requested to surrender.  The probability of this
**	is a function of that Klingon's remaining power, our power,
**	etc.
*/

int selectklingon(void);

void
   capture(void)
{
	int		i;
	int k;
	double			x;

	ididit = FALSE; // Nothing if we fail
	Time = 0.0;

	/* Make sure there is room in the brig */
	if (brigfree == 0)
	{
		printf("Security reports the brig is already full.\n");
		return;
	}

	if (!REPORTS) {
		printf("Uhura- \"We have no subspace radio communication, sir.\"\n");
		return;
	}

	if (damage[DTRANSP] != 0) {
		printf("Scotty- \"Transporter damaged, sir.\"\n");
		return;
	}

	
	
	/* find out if there are any at all */
	if (currentq_num_klingons < 1)
	{
		printf("Uhura- \"Getting no response, sir.\"\n");
		return;
	}

	/* if there is more than one Klingon, find out which one */
	k = selectklingon();
	Time = 0.05;   // This action will take some time
	ididit = TRUE; //  So any others can strike back

    /* check out that Klingon */
    /* The algorithm isn't that great and could use some more
     * intelligent design */
//	x = 300 + 25*skill;
	x = ship_energy;
	x /= kpower[k] * currentq_num_enemies;
	x *= 2.5;  /* would originally have been equivalent of 1.4, but we want command to work more often, more humanely */
	i = (int)x;
#ifdef DEBUG
	printf("Prob = %d (%.4f)\n", i, x);
//	i = 100; // For testing, of course!
#endif
	if (i > 100*Rand())
	{
		/* guess what, he surrendered!!! */
		printf("Klingon captain at %d,%d surrenders\n", kx[k], ky[k]);
		i = (int)(200*Rand());
		if ( i > 0 )
			printf("%d Klingons commit suicide rather than be taken captive\n", 200 - i);
		if (i > brigfree)
		{
			printf("%d Klingons die because there is no room for them in the brig.\n", i-brigfree);
			i = brigfree;
		}
		brigfree -= i;
		printf("%d captives taken\n", i);
		deadkl(kx[k], ky[k], quad[kx[k]][ky[k]], kx[k], ky[k]);
		if (g_d.remaining_klingons==0) finish(FWON);
		return;
	}

	/* big surprise, he refuses to surrender */
	printf("Fat chance, captain\n");
	return;
}


/*
 **  SELECT A KLINGON
 **
 **	Cruddy, just takes one at random.  Should ask the captain.
 **	Nah, just select the weakest one since it is most likely to
 **	surrender (Tom Almy mod)
 */

int selectklingon()
{
	int		i;

	if (currentq_num_enemies < 2)
		i = 1;
	else
	{	// Select the weakest one
		double pow  = 1e6;
		int j;
		for (j=1; j <= currentq_num_enemies; j++)
		{
			if (quad[kx[j]][ky[j]] == IH_ROMULAN) continue; // No Romulans surrender
			if (kpower[j]< pow)
			{
				pow = kpower[j];
				i = j;
			}
		}
	}
	return i;
}

#endif