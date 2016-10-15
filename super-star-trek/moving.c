#include "sst.h"

static void getcd(int, int);

void lmove(void) {
	double angle, deltax, deltay, bigger, x, y,
    finald, finalx, finaly, stopegy;
    int oldquadx, oldquady;
	int trbeam = 0, n, l, ix, iy, kink, kinks, iquad;

	if (is_inorbit) {
		prout("SULU- \"Leaving standard orbit.\"");
		is_inorbit = 0;
	}

	angle = ((15.0 - direc) * 0.5235988);
	deltax = -sin(angle);
	deltay = cos(angle);
	if (fabs(deltax) > fabs(deltay))
		bigger = fabs(deltax);
	else
		bigger = fabs(deltay);
		
	deltay /= bigger;
	deltax /= bigger;

#ifdef CLOAKING
    if (iscloaked && g_d.stardate+Time >= future[FTBEAM])
    {  /* We can't be tracto beamed if cloaked, so move the event into the future */
        future[FTBEAM] = g_d.stardate + Time +
                         expran(1.5*game_initial_time/g_d.remaining_commanders);
    }
#endif
    
	/* If tractor beam is to occur, don't move full distance */
	if (g_d.stardate+Time >= future[FTBEAM]) {
		trbeam = 1;
		ship_condition = IHRED;
		dist = dist*(future[FTBEAM]-g_d.stardate)/Time + 0.1;
		Time = future[FTBEAM] - g_d.stardate + 1e-5;
	}
	/* Move within the quadrant */
	quad[sectx][secty] = IHDOT;
	x = sectx;
	y = secty;
	n = 10.0*dist*bigger+0.5;

	if (n > 0) {
		for (l = 1; l <= n; l++) {
			ix = (x += deltax) + 0.5;
			iy = (y += deltay) + 0.5;
			if (ix < 1 || ix > 10 || iy < 1 || iy > 10) {
				/* Leaving quadrant -- allow final enemy attack */
				/* Don't do it if being pushed by Nova */
				if (currentq_num_enemies != 0 && iattak != 2
#ifdef CLOAKING
				    && !iscloaked
#endif
				   ) {
					newcnd();
					for (l = 1; l <= currentq_num_enemies; l++) {
						finald = sqrt((ix-kx[l])*(double)(ix-kx[l]) +
									  (iy-ky[l])*(double)(iy-ky[l]));
						kavgd[l] = 0.5 * (finald+kdist[l]);
					}
					if (g_d.galaxy[quadx][quady] != 1000) attack(0);
					if (alldone) return;
				}
				/* compute final position -- new quadrant and sector */
				x = 10*(quadx-1)+sectx;
				y = 10*(quady-1)+secty;
				ix = x+10.0*dist*bigger*deltax+0.5;
				iy = y+10.0*dist*bigger*deltay+0.5;
				/* check for edge of galaxy */
				kinks = FALSE;
				do {
					kink = 0;
					if (ix <= 0) {
						ix = -ix + 1;
						kink = 1;
					}
					if (iy <= 0) {
						iy = -iy + 1;
						kink = 1;
					}
					if (ix > 80) {
						ix = 161 - ix;
						kink = 1;
					}
					if (iy > 80) {
						iy = 161 - iy;
						kink = 1;
					}
					if (kink) kinks = TRUE;
				} while (kink);

				if (kinks) {
					game_num_intergalactic_attempts += 1;
					if (game_num_intergalactic_attempts == 3) {
						/* Three strikes -- you're out! */
						finish(FNEG3);
						return;
					}
					prout("\nYOU HAVE ATTEMPTED TO CROSS THE NEGATIVE ENERGY BARRIER\n"
						 "AT THE EDGE OF THE GALAXY.  THE THIRD TIME YOU TRY THIS,\n"
                          "YOU WILL BE DESTROYED.\n");
                }
				/* Compute final position in new quadrant */
                if (trbeam) return; /* Don't bother if we are to be beamed */
                oldquadx = quadx;
                oldquady = quady;
				quadx = (ix+9)/10;
                quady = (iy+9)/10;
                sectx = ix - 10*(quadx-1);
                secty = iy - 10*(quady-1);
                if (quadx != oldquadx || quady != oldquady) {
                    proutn("\nEntering");
                    cramlc(1, quadx, quady);
                } else {
                    prout("(Negative energy barrier disturbs quadrant.)");
                }
                skip(1);
                quad[sectx][secty] = ship_name;
                newqad(0);
                return;
            }
			iquad = quad[ix][iy];
			if (iquad != IHDOT) {
				/* object encountered in flight path */
				stopegy = 50.0*dist/Time;
				dist=0.1*sqrt((sectx-ix)*(double)(sectx-ix) +
							  (secty-iy)*(double)(secty-iy));
				switch (iquad) {
					case IH_THOLIAN: /* Ram a Tholean */
					case IH_KLINGON: /* Ram enemy ship */
					case IH_COMMANDER:
					case IH_SUPER_COMMANDER:
					case IH_ROMULAN:
						sectx = ix;
						secty = iy;
						ram(0, iquad, sectx, secty);
						finalx = sectx;
						finaly = secty;
						break;
					case IH_BLACK_HOLE:
						skip(1);
						prouts("***RED ALERT!  RED ALERT!");
						skip(1);
						proutn("***");
						crmshp();
						proutn(" pulled into black hole at");
						cramlc(2, ix, iy);
						skip(1);
						finish(FHOLE);
						return;
					default:
						/* something else */
						skip(1);
						crmshp();
						if (iquad == IH_THOLIAN_WEB)
							proutn(" encounters Tholian web at");
						else
							proutn(" blocked by object at");
						cramlc(2, ix,iy);
						prout(";");
						proutn("Emergency stop required ");
						cramf(stopegy, 0, 2);
						prout(" units of energy.");
						ship_energy -= stopegy;
						finalx = x-deltax+0.5;
						sectx = finalx;
						finaly = y-deltay+0.5;
						secty = finaly;
						if (ship_energy <= 0) {
							finish(FNRG);
							return;
						}
						break;
                }
				goto label100;	/* sorry! */ /* ACTUALLY BREAK SHOULD WORK HERE */
            }
        }
		dist = 0.1*sqrt((sectx-ix)*(double)(sectx-ix) +
						(secty-iy)*(double)(secty-iy));
		sectx = ix;
		secty = iy;
	}
	finalx = sectx; /* THESE STATEMENTS DO NOTHING USEFUL */
	finaly = secty;
label100:
	/* No quadrant change -- compute new avg enemy distances */
	quad[sectx][secty] = ship_name;
	if (currentq_num_enemies) {
		for (l = 1; l <= currentq_num_enemies; l++) {
			finald = sqrt((ix-kx[l])*(double)(ix-kx[l]) +
						  (iy-ky[l])*(double)(iy-ky[l]));
			kavgd[l] = 0.5 * (finald+kdist[l]);
			kdist[l] = finald;
		}
		sortkl();
		if (g_d.galaxy[quadx][quady] != 1000 && iattak == 0)
			attack(0);
		for (l = 1 ; l <= currentq_num_enemies; l++) kavgd[l] = kdist[l];
	}
	newcnd();
	iattak = 0;
	return;
}

void dock(void) {
	chew();
	if (ship_condition == IHDOCKED) {
		prout("Already docked.");
		return;
	}
	if (is_inorbit) {
		prout("You must first leave standard orbit.");
		return;
	}
	if (currentq_base_sx==0 || abs(sectx-currentq_base_sx) > 1 || abs(secty-currentq_base_sy) > 1) {
		crmshp();
		prout(" not adjacent to base.");
		return;
	}
#ifdef CLOAKING
	if (iscloaked) {
		prout("You cannot dock while cloaked.");
		return;
	}
#endif
	ship_condition = IHDOCKED;
	prout("Docked.");
	if (ship_energy < ship_max_energy) ship_energy = ship_max_energy;
	ship_shield_strength = ship_max_shield;
	torps = intorps;
    ship_life_support_reserves = game_initial_lifesupport;
#ifdef CAPTURE
    if (brigcapacity-brigfree > 0)
    {
        printf("%d captured Klingons transferred to base.\n", brigcapacity-brigfree);
        captured_klingons += brigcapacity-brigfree;
        brigfree = brigcapacity;
    }
#endif
	if (ship_date_chart_damaged != 1e30 &&
		(future[FCDBAS] < 1e30 || is_supercommander_attacking_base == 1) && has_seen_attack_report == 0) {
		/* get attack report from base */
		prout("Lt. Uhura- \"Captain, an important message from the starbase:\"");
		attakreport();
		has_seen_attack_report = 1;
	}
}

static void getcd(int isprobe, int akey) {
	/* This program originally required input in terms of a (clock)
	   direction and distance. Somewhere in history, it was changed to
	   cartesian coordinates. So we need to convert. I think
	   "manual" input should still be done this way -- it's a real
	   pain if the computer isn't working! Manual mode is still confusing
	   because it involves giving x and y motions, yet the coordinates
	   are always displayed y - x, where +y is downward! */

	
	int irowq=quadx, icolq=quady, irows, icols, itemp=0, iprompt=0, key;
	double xi, xj, xk, xl;
	double deltax, deltay;
	int automatic = -1;

	/* Get course direction and distance. If user types bad values, return
	   with DIREC = -1.0. */

	direc = -1.0;
	
	if (is_landed == 1 && !isprobe) {
		prout("Dummy! You can't leave standard orbit until you");
		proutn("are back aboard the ");
		crmshp();
		prout(".");
		chew();
		return;
	}
	while (automatic == -1) {
		if (damage[DCOMPTR]) {
			if (isprobe)
				prout("Computer damaged; manual navigation only");
			else
				prout("Computer damaged; manual movement only");
			chew();
			automatic = 0;
			key = IHEOL;
			break;
		}
		if (isprobe && akey != -1) {
			/* For probe launch, use pre-scaned value first time */
			key = akey;
			akey = -1;
		}
		else 
			key = scan();

		if (key == IHEOL) {
			proutn("Manual or automatic- ");
			iprompt = 1;
			chew();
		}
		else if (key == IHALPHA) {
			if (isit("manual")) {
				automatic =0;
				key = scan();
				break;
			}
			else if (isit("automatic")) {
				automatic = 1;
				key = scan();
				break;
			}
			else {
				huh();
				chew();
				return;
			}
		}
		else { /* numeric */
			if (isprobe)
				prout("(Manual navigation assumed.)");
			else
				prout("(Manual movement assumed.)");
			automatic = 0;
			break;
		}
	}

	if (automatic) {
		while (key == IHEOL) {
			if (isprobe)
				proutn("Target quadrant or quadrant&sector- ");
			else
				proutn("Destination sector or quadrant&sector- ");
			chew();
			iprompt = 1;
			key = scan();
		}

		if (key != IHREAL) {
			huh();
			return;
		}
		xi = aaitem;
		key = scan();
		if (key != IHREAL){
			huh();
			return;
		}
		xj = aaitem;
		key = scan();
		if (key == IHREAL) {
			/* both quadrant and sector specified */
			xk = aaitem;
			key = scan();
			if (key != IHREAL) {
				huh();
				return;
			}
			xl = aaitem;

			irowq = xi + 0.5;
			icolq = xj + 0.5;
			irows = xk + 0.5;
			icols = xl + 0.5;
		}
		else {
			if (isprobe) {
				/* only quadrant specified -- go to center of dest quad */
				irowq = xi + 0.5;
				icolq = xj + 0.5;
				irows = icols = 5;
			}
			else {
				irows = xi + 0.5;
				icols = xj + 0.5;
			}
			itemp = 1;
		}
		if (irowq<1 || irowq > 8 || icolq<1 || icolq > 8 ||
			irows<1 || irows > 10 || icols<1 || icols > 10) {
				huh();
				return;
			}
		skip(1);
		if (!isprobe) {
			if (itemp) {
				if (iprompt) {
					proutn("Helmsman Sulu- \"Course locked in for");
					cramlc(2, irows, icols);
					prout(".\"");
				}
			}
			else prout("Ensign Chekov- \"Course laid in, Captain.\"");
		}
		deltax = icolq - quady + 0.1*(icols-secty);
		deltay = quadx - irowq + 0.1*(sectx-irows);
	}
	else { /* manual */
		while (key == IHEOL) {
			proutn("X and Y displacements- ");
			chew();
			iprompt = 1;
			key = scan();
		}
		itemp = 2;
		if (key != IHREAL) {
			huh();
			return;
		}
		deltax = aaitem;
        key = scan();
        if (key == IHEOL) {
            deltay = 0.0;
        } else 	if (key != IHREAL) {
			huh();
			return;
		} else {
            deltay = aaitem;
        }
        
        if (coordfixed) {
            double temp = deltax;
            deltax = deltay;
            deltay = -temp;
        }
	}
	/* Check for zero movement */
	if (deltax == 0 && deltay == 0) {
		chew();
		return;
	}
	if (itemp == 2 && !isprobe) {
		skip(1);
		prout("Helmsman Sulu- \"Aye, Sir.\"");
	}
	dist = sqrt(deltax*deltax + deltay*deltay);
	direc = atan2(deltax, deltay)*1.90985932;
	if (direc < 0.0) direc += 12.0;
	chew();
	return;

}
		


void impuls(void) {
	double power;

	ididit = 0;
	if (damage[DIMPULS]) {
		chew();
		skip(1);
		prout("Engineer Scott- \"The impulse engines are damaged, Sir.\"");
		return;
	}

	if (ship_energy > 30.0) {
		getcd(FALSE, 0);
		if (direc == -1.0) return;
		power = 20.0 + 100.0*dist;
	}
	else
		power = 30.0;

	if (power >= ship_energy) {
		/* Insufficient power for trip */
		skip(1);
		prout("First Officer Spock- \"Captain, the impulse engines");
		prout("require 20.0 units to engage, plus 100.0 units per");
		if (ship_energy > 30) {
			proutn("quadrant.  We can go, therefore, a maximum of ");
			cramf(0.01 * (ship_energy-20.0)-0.05, 0, 1);
			prout(" quadrants.\"");
		}
		else {
			prout("quadrant.  They are, therefore, useless.\"");
		}
		chew();
		return;
	}
	/* Make sure enough time is left for the trip */
	Time = dist/0.095;
	if (Time >= g_d.remaining_time) {
		prout("First Officer Spock- \"Captain, our speed under impulse");
		prout("power is only 0.95 sectors per stardate. Are you sure");
		prout("we dare spend the time?\"");
		if (ja() == 0) { Time = 0.0; return;}
	}
	/* Activate impulse engines and pay the cost */
	lmove();
	ididit = 1;
	if (alldone) return;
	power = 20.0 + 100.0*dist;
	ship_energy -= power;
//	Time = dist/0.095; Don't recalculate because lmove may have
//	adjusted it for tractor beaming
	if (ship_energy <= 0) finish(FNRG);
	return;
}


void warp(int i) {
	int blooey=0, twarp=0, iwarp;
	double power;

	if (i!=2) { /* Not WARPX entry */
		ididit = 0;
#ifdef CLOAKING
		if (iscloaked) {
			chew();
			skip(1);
			prout("Engineer Scott- \"The warp engines can better not be used while cloaked, Sir.\"");
			return;
		}
#endif
		if (damage[DWARPEN] > 10.0) {
			chew();
			skip(1);
			prout("Engineer Scott- \"The warp engines are damaged, Sir.\""); // Was "Impulse" 10/2013
			return;
		}
		if (damage[DWARPEN] > 0.0 && warp_factor > 4.0) {
			chew();
			skip(1);
			prout("Engineer Scott- \"Sorry, Captain. Until this damage");
			prout("  is repaired, I can only give you warp 4.\"");
			return;
		}
			
		/* Read in course and distance */
		getcd(FALSE, 0);
		if (direc == -1.0) return;

		/* Make sure starship has enough energy for the trip */
		power = (dist+0.05)*warp_factor*warp_factor*warp_factor*(is_shield_up+1);


		if (power >= ship_energy) {
			/* Insufficient power for trip */
			ididit = 0;
			skip(1);
			prout("Engineering to bridge--");
			if (is_shield_up==0 || 0.5*power > ship_energy) {
				iwarp = pow((ship_energy/(dist+0.05)), 0.333333333);
				if (iwarp <= 0) {
					prout("We can't do it, Captain. We haven't the energy.");
				}
				else {
					proutn("We haven't the energy, but we could do it at warp ");
					crami(iwarp, 1);
					if (is_shield_up)
						prout(",\nif you'll lower the shields.");
					else
						prout(".");
				}
			}
			else
				prout("We haven't the energy to go that far with the shields up.");
			return;
		}
						
		/* Make sure enough time is left for the trip */
		Time = 10.0*dist/warp_factor_squared;
		if (Time >= 0.8*g_d.remaining_time) {
			skip(1);
			prout("First Officer Spock- \"Captain, I compute that such");
			proutn("  a trip would require approximately ");
			cramf(100.0*Time/g_d.remaining_time, 0, 2);
			prout(" percent of our");
			prout("  remaining time.  Are you sure this is wise?\"");
			if (ja() == 0) { Time = 0.0; return;}
		}
	}
	/* Entry WARPX */
	if (warp_factor > 6.0) {
		/* Decide if engine damage will occur */
		double prob = dist*(6.0-warp_factor)*(6.0-warp_factor)/66.666666666;
		if (prob > Rand()) {
			blooey = 1;
			dist = Rand()*dist;
		}
		/* Decide if time warp will occur */
		if (0.5*dist*pow(7.0,warp_factor-10.0) > Rand()) twarp=1;
#ifdef DEBUG
		if (idebug &&warp_factor==10 && twarp==0) {
			blooey=0;
			proutn("Force time warp? ");
			if (ja()==1) twarp=1;
		}
#endif
		if (blooey || twarp) {
			/* If time warp or engine damage, check path */
			/* If it is obstructed, don't do warp or damage */
			double angle = ((15.0-direc)*0.5235998);
			double deltax = -sin(angle);
			double deltay = cos(angle);
			double bigger, x, y;
			int n, l, ix, iy;
			if (fabs(deltax) > fabs(deltay))
				bigger = fabs(deltax);
			else
				bigger = fabs(deltay);
			
			deltax /= bigger;
			deltay /= bigger;
			n = 10.0 * dist * bigger +0.5;
			x = sectx;
			y = secty;
			for (l = 1; l <= n; l++) {
				x += deltax;
				ix = x + 0.5;
				if (ix < 1 || ix > 10) break;
				y += deltay;
				iy = y +0.5;
				if (iy < 1 || iy > 10) break;
				if (quad[ix][iy] != IHDOT) {
					blooey = 0;
					twarp = 0;
				}
			}
		}
	}
				

	/* Activate Warp Engines and pay the cost */
	lmove();
	if (alldone) return;
	ship_energy -= dist*warp_factor*warp_factor*warp_factor*(is_shield_up+1);
	if (ship_energy <= 0) finish(FNRG);
	Time = 10.0*dist/warp_factor_squared;
	if (twarp) timwrp();
	if (blooey) {
		damage[DWARPEN] = game_damage_factor*(3.0*Rand()+1.0);
		skip(1);
		prout("Engineering to bridge--");
		prout("  Scott here.  The warp engines are damaged.");
		prout("  We'll have to reduce speed to warp 4.");
	}
	ididit = 1;
	return;
}



void setwrp(void) {
	int key;
	double oldfac;
	
	while ((key=scan()) == IHEOL) {
		chew();
		proutn("Warp factor-");
	}
	chew();
	if (key != IHREAL) {
		huh();
		return;
	}
	if (damage[DWARPEN] > 10.0) {
		prout("Warp engines inoperative.");
		return;
	}
	if (damage[DWARPEN] > 0.0 && aaitem > 4.0) {
		prout("Engineer Scott- \"I'm doing my best, Captain,\n"
			  "  but right now we can only go warp 4.\"");
		return;
	}
	if (aaitem > 10.0) {
		prout("Helmsman Sulu- \"Our top speed is warp 10, Captain.\"");
		return;
	}
	if (aaitem < 1.0) {
		prout("Helmsman Sulu- \"We can't go below warp 1, Captain.\"");
		return;
	}
	oldfac = warp_factor;
	warp_factor = aaitem;
	warp_factor_squared=warp_factor*warp_factor;
	if (warp_factor <= oldfac || warp_factor <= 6.0) {
		proutn("Helmsman Sulu- \"Warp factor ");
		cramf(warp_factor, 0, 1);
		prout(", Captain.\"");
		return;
	}
	if (warp_factor < 8.00) {
		prout("Engineer Scott- \"Aye, but our maximum safe speed is warp 6.\"");
		return;
	}
	if (warp_factor == 10.0) {
		prout("Engineer Scott- \"Aye, Captain, we'll try it.\"");
		return;
	}
	prout("Engineer Scott- \"Aye, Captain, but our engines may not take it.\"");
	return;
}

void atover(int igrab) {
	double power, distreq;

	chew();
	/* is captain on planet? */
	if (is_landed==1) {
		if (damage[DTRANSP]) {
			finish(FPNOVA);
			return;
		}
		prout("Scotty rushes to the transporter controls.");
		if (is_shield_up) {
			prout("But with the shields up it's hopeless.");
			finish(FPNOVA);
		}
		prouts("His desperate attempt to rescue you . . .");
		if (Rand() <= 0.5) {
			prout("fails.");
			finish(FPNOVA);
			return;
		}
		prout("SUCCEEDS!");
		if (is_mining) {
			is_mining = 0;
			proutn("The crystals mined were ");
			if (Rand() <= 0.25) {
				prout("lost.");
			}
			else {
				prout("saved.");
				have_crystals = 1;
			}
		}
	}
	if (igrab) return;

	/* Check to see if captain in shuttle craft */
	if (is_aboard_shuttle) finish(FSTRACTOR);
	if (alldone) return;

	/* Inform captain of attempt to reach safety */
	skip(1);
	do {
		if (justin) {
			prouts("***RED ALERT!  RED ALERT!");
			skip(1);
			proutn("The ");
			crmshp();
			prout(" has stopped in a quadrant containing");
			prouts("   a supernova.");
			skip(2);
		}
		proutn("***Emergency automatic override attempts to hurl ");
		crmshp();
		skip(1);
		prout("safely out of quadrant.");
		starch[quadx][quady] = damage[DRADIO] > 0.0 ? g_d.galaxy[quadx][quady]+1000:1;

		/* Try to use warp engines */
		if (damage[DWARPEN]) {
			skip(1);
			prout("Warp engines damaged.");
			finish(FSNOVAED);
			return;
		}
		warp_factor = 6.0+2.0*Rand();
		warp_factor_squared = warp_factor * warp_factor;
		proutn("Warp factor set to ");
		cramf(warp_factor, 1, 1);
		skip(1);
		power = 0.75*ship_energy;
		dist = power/(warp_factor*warp_factor*warp_factor*(is_shield_up+1));
		distreq = 1.4142+Rand();
		if (distreq < dist) dist = distreq;
		Time = 10.0*dist/warp_factor_squared;
		direc = 12.0*Rand();	/* How dumb! */
		justin = 0;
		is_inorbit = 0;
		warp(2);
		if (justin == 0) {
			/* This is bad news, we didn't leave quadrant. */
			if (alldone) return;
			skip(1);
			prout("Insufficient energy to leave quadrant.");
			finish(FSNOVAED);
			return;
		}
		/* Repeat if another snova */
	} while (g_d.galaxy[quadx][quady] == 1000);
	if (g_d.remaining_klingons==0) finish(FWON); /* Snova killed remaining enemy. */
}

void timwrp() {
	int l, ll, gotit;
	prout("***TIME WARP ENTERED.");
	if (g_d.snap && Rand() < 0.5) {
		/* Go back in time */
		proutn("You are traveling backwards in time ");
		cramf(g_d.stardate-snapsht.stardate, 0, 2);
		prout(" stardates.");
		g_d = snapsht;
		g_d.snap = 0;
		if (g_d.remaining_commanders) {
			future[FTBEAM] = g_d.stardate + expran(game_initial_time/g_d.remaining_commanders);
			future[FBATTAK] = g_d.stardate + expran(0.3*game_initial_time);
		}
		future[FSNOVA] = g_d.stardate + expran(0.5*game_initial_time);
		future[FSNAP] = g_d.stardate +expran(0.25*g_d.remaining_time); /* next snapshot will
													   be sooner */
		if (g_d.remaining_supercommanders) future[FSCMOVE] = 0.2777;
		is_supercommander_attacking_base = 0;
		future[FCDBAS] = future[FSCDBAS] = 1e30;
		batx = baty = 0;

		/* Make sure Galileo is consistant -- Snapshot may have been taken
		   when on planet, which would give us two Galileos! */
		gotit = 0;
		for (l = 1; l <= inplan; l++) {
			if (g_d.plnets[l].known == 2) {
				gotit = 1;
				if (has_suttlecraft==1 && ship_name==IH_ENTERPRISE) {
					prout("Checkov-  \"Security reports the Galileo has disappeared, Sir!");
					has_suttlecraft = 0;
				}
			}
		}
		/* Likewise, if in the original time the Galileo was abandoned, but
		   was on ship earlier, it would have vanished -- lets restore it */
		if (has_suttlecraft==0 && gotit==0 && damage[DSHUTTL] >= 0.0) {
			prout("Checkov-  \"Security reports the Galileo has reappeared in the dock!\"");
			has_suttlecraft = 1;
		}

		/* Revert star chart to earlier era, if it was known then*/
		if (damage[DRADIO]==0.0 || ship_date_chart_damaged > g_d.stardate) {
			for (l = 1; l <= 8; l++)
				for (ll = 1; ll <= 8; ll++)
					if (starch[l][ll] > 1)
						starch[l][ll]=damage[DRADIO]>0.0 ? g_d.galaxy[l][ll]+1000 :1;
			prout("Spock has reconstructed a correct star chart from memory");
			if (damage[DRADIO] > 0.0) ship_date_chart_damaged = g_d.stardate;
		}
	}
	else {
		/* Go forward in time */
		Time = -0.5*game_initial_time*log(Rand());
		proutn("You are traveling forward in time ");
		cramf(Time, 1, 2);
		prout(" stardates.");
		/* cheat to make sure no tractor beams occur during time warp */
		future[FTBEAM] += Time;
		damage[DRADIO] += Time;
	}
	newqad(0);
}

void probe(void) {
	double angle, bigger;
	int key;
	/* New code to launch a deep space probe */
	if (remaining_probes == 0) {
		chew();
		skip(1);
		if (ship_name == IH_ENTERPRISE) 
			prout("Engineer Scott- \"We have no more deep space probes, Sir.\"");
		else
			prout("Ye Faerie Queene has no deep space probes.");
		return;
	}
	if (damage[DDSP] != 0.0) {
		chew();
		skip(1);
		prout("Engineer Scott- \"The probe launcher is damaged, Sir.\"");
		return;
	}
	if (future[FDSPROB] != 1e30) {
		chew();
		skip(1);
		if (REPORTS) {
			prout("Uhura- \"The previous probe is still reporting data, Sir.\"");
		} else {
			prout("Spock-  \"Records show the previous probe has not yet");
			prout("   reached its destination.\"");
		}
		return;
	}
	key = scan();

	if (key == IHEOL) {
		/* slow mode, so let Kirk know how many probes there are left */
		crami(remaining_probes,1);
		prout(remaining_probes==1 ? " probe left." : " probes left.");
		proutn("Are you sure you want to fire a probe? ");
		if (ja()==0) return;
	}

	is_probe_armed = FALSE;
	if (key == IHALPHA && strcmp(citem,"armed") == 0) {
		is_probe_armed = TRUE;
		key = scan();
	}
	else if (key == IHEOL) {
		proutn("Arm NOVAMAX warhead?");
		is_probe_armed = ja();
	}
	getcd(TRUE, key);
	if (direc == -1.0) return;
	remaining_probes--;
		angle = ((15.0 - direc) * 0.5235988);
	probe_increment_gx = -sin(angle);
	probe_increment_gy = cos(angle);
	if (fabs(probe_increment_gx) > fabs(probe_increment_gy))
		bigger = fabs(probe_increment_gx);
	else
		bigger = fabs(probe_increment_gy);
		
	probe_increment_gy /= bigger;
	probe_increment_gx /= bigger;
	probe_active_sectors_remaining = 10.0*dist*bigger +0.5;
	probe_global_x = quadx*10 + sectx - 1;	// We will use better packing than original
	probe_global_y = quady*10 + secty - 1;
	probe_qx = quadx;
	probe_qy = quady;
	future[FDSPROB] = g_d.stardate + 0.01; // Time to move one sector
	prout("Ensign Chekov-  \"The deep space probe is launched, Captain.\"");
	return;
}

void help(void) {
	/* There's more than one way to move in this game! */
	double ddist, xdist, probf;
	int line, l, ix, iy;

	chew();
	/* Test for conditions which prevent calling for help */
	if (ship_condition == IHDOCKED) {
		prout("Lt. Uhura-  \"But Captain, we're already docked.\"");
		return;
	}
	if (damage[DRADIO] != 0) {
		prout("Subspace radio damaged.");
		return;
	}
	if (g_d.remaining_bases==0) {
		prout("Lt. Uhura-  \"Captain, I'm not getting any response from Starbase.\"");
		return;
	}
	if (is_landed == 1) {
		proutn("You must be aboard the ");
		crmshp();
		prout(".");
		return;
	}
	/* OK -- call for help from nearest starbase */
	game_num_help_calls++;
	if (currentq_base_sx!=0) {
		/* There's one in this quadrant */
		ddist = sqrt(square(currentq_base_sx-sectx)+square(currentq_base_sy-secty));
	}
	else {
		ddist = 1e30;
		for (l = 1; l <= g_d.remaining_bases; l++) {
			xdist=10.0*sqrt(square(g_d.qx_base[l]-quadx)+square(g_d.qy_base[l]-quady));
			if (xdist < ddist) {
				ddist = xdist;
				line = l;
			}
		}
		/* Since starbase not in quadrant, set up new quadrant */
		quadx = g_d.qx_base[line];
		quady = g_d.qy_base[line];
		newqad(1);
	}
	/* dematerialize starship */
	quad[sectx][secty]=IHDOT;
	proutn("Starbase in");
	cramlc(1, quadx, quady);
	proutn(" responds--");
	crmshp();
	prout(" dematerializes.");
	/* Give starbase three chances to rematerialize starship */
	probf = pow((1.0 - pow(0.98,ddist)), 0.33333333);
	for (l = 1; l <= 3; l++) {
		switch (l) {
			case 1: proutn("1st"); break;
			case 2: proutn("2nd"); break;
			case 3: proutn("3rd"); break;
		}
		proutn(" attempt to re-materialize ");
		crmshp();
		prouts(" . . . . . ");
		if (Rand() > probf) break;
		prout("fails.");
	}
	if (l > 3) {
		finish(FMATERIALIZE);
		return;
	}
	/* Rematerialization attempt should succeed if can get adj to base */
	for (l = 1; l <= 5; l++) {
		ix = currentq_base_sx+3.0*Rand()-1;
		iy = currentq_base_sy+3.0*Rand()-1;
		if (ix>=1 && ix<=10 && iy>=1 && iy<=10 && quad[ix][iy]==IHDOT) {
			/* found one -- finish up */
			prout("succeeds.");
			sectx=ix;
			secty=iy;
			quad[ix][iy]=ship_name;
			dock();
			skip(1);
			prout("Lt. Uhura-  \"Captain, we made it!\"");
			return;
		}
	}
	finish(FMATERIALIZE);
	return;
}
