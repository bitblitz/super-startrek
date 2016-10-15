#include "sst.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>

void attakreport(void) {
	if (future[FCDBAS] < 1e30) {
		proutn("Starbase in ");
		cramlc(1, batx, baty);
		prout(" is currently under attack.");
		proutn("It can hold out until Stardate ");
		cramf(future[FCDBAS], 0,1);
		prout(".");
	}
	if (is_supercommander_attacking_base == 1) {
		proutn("Starbase in ");
		cramlc(1, g_d.qx_supercommander, g_d.qy_supercommander);
		prout(" is under Super-commander attack.");
		proutn("It can hold out until Stardate ");
		cramf(future[FSCDBAS], 0, 1);
		prout(".");
	}
}
	

void report(int f) {
	char *s1,*s2,*s3;

	chew();
	s1 = (thawed?"thawed ":"");
	switch (game_length) {
		case 1: s2="short"; break;
		case 2: s2="medium"; break;
		case 4: s2="long"; break;
		default: s2="unknown length"; break;
	}
	switch (game_skill) {
		case SNOVICE: s3="novice"; break;
		case SFAIR: s3="fair"; break;
		case SGOOD: s3="good"; break;
		case SEXPERT: s3="expert"; break;
		case SEMERITUS: s3="emeritus"; break;
		default: s3="skilled"; break;
	}
	printf("\nYou %s playing a %s%s %s game.\n",
		   alldone? "were": "are now", s1, s2, s3);
	if (game_skill>SGOOD && thawed && !alldone) prout("No plaque is allowed.");
	if (is_tournament_game) printf("This is tournament game %d.\n", is_tournament_game);
	if (f) printf("Your secret password is \"%s\"\n",passwd);
	printf("%d of %d Klingon ships have been destroyed",
		   g_d.killed_klingons+g_d.killed_commanders+g_d.killed_supercommanders, initial_klingons);
	if (g_d.killed_commanders) printf(", including %d Commander%s.\n", g_d.killed_commanders, g_d.killed_commanders==1?"":"s");
	else if (g_d.killed_klingons+g_d.killed_supercommanders > 0) prout(", but no Commanders.");
	else prout(".");
	if (game_skill > SFAIR) printf("The Super Commander has %sbeen destroyed.\n",
						  g_d.remaining_supercommanders?"not ":"");
	if (g_d.remaining_bases != initial_bases) {
		proutn("There ");
		if (initial_bases-g_d.remaining_bases==1) proutn("has been 1 base");
		else {
			proutn("have been ");
			crami(initial_bases-g_d.remaining_bases, 1);
			proutn(" bases");
		}
		proutn(" destroyed, ");
		crami(g_d.remaining_bases, 1);
		prout(" remaining.");
	}
	else printf("There are %d bases.\n", initial_bases);
	if (REPORTS || has_seen_attack_report) {
		/* Don't report this if not seen and
			either the radio is dead or not at base! */
		attakreport();
		has_seen_attack_report = 1;
	}
	if (num_casualties) printf("%d casualt%s suffered so far.\n",
					   num_casualties, num_casualties==1? "y" : "ies");
#ifdef CAPTURE
    if (brigcapacity != brigfree) printf("%d Klingon%s in brig.\n",
    							brigcapacity-brigfree, brigcapacity-brigfree>1 ? "s" : "");
    if (captured_klingons > 0) printf("%d captured Klingon%s turned in to Star Fleet.\n", 
                               captured_klingons, captured_klingons>1 ? "s" : "");
#endif
	if (game_num_help_calls) printf("There were %d call%s for help.\n",
					  game_num_help_calls, game_num_help_calls==1 ? "" : "s");
	if (ship_name == IH_ENTERPRISE) {
		proutn("You have ");
		if (remaining_probes) crami(remaining_probes,1);
		else proutn("no");
		proutn(" deep space probe");
		if (remaining_probes!=1) proutn("s");
		prout(".");
	}
	if (REPORTS && future[FDSPROB] != 1e30) {
		if (is_probe_armed) 
			proutn("An armed deep space probe is in");
		else
			proutn("A deep space probe is in");
		cramlc(1, probe_qx, probe_qy);
		prout(".");
	}
	if (have_crystals) {
		if (crystal_prob_fail <= .05)
			prout("Dilithium crystals aboard ship...not yet used.");
		else {
			int i=0;
			double ai = 0.05;
			while (crystal_prob_fail > ai) {
				ai *= 2.0;
				i++;
			}
			printf("Dilithium crystals have been used %d time%s.\n",
				   i, i==1? "" : "s");
		}
	}
	skip(1);
}
	
void lrscan(void) {
	int x, y;
	chew();
	if (damage[DLRSENS] != 0.0) {
		/* Now allow base's sensors if docked */
		if (ship_condition != IHDOCKED) {
			prout("LONG-RANGE SENSORS DAMAGED.");
			return;
		}
		skip(1);
		proutn("Starbase's long-range scan for");
	}
	else {
		skip(1);
		proutn("Long-range scan for");
	}
	cramlc(1, quadx, quady);
	skip(1);
	if (coordfixed)
	for (y = quady+1; y >= quady-1; y--) {
		for (x = quadx-1; x <= quadx+1; x++) {
			if (x == 0 || x > 8 || y == 0 || y > 8)
				printf("   -1");
			else {
				printf("%5d", g_d.galaxy[x][y]);
				// If radio works, mark star chart so
				// it will show current information.
				// Otherwise mark with current
				// value which is fixed. 
				starch[x][y] = damage[DRADIO] > 0 ? g_d.galaxy[x][y]+1000 :1;
			}
		}
		putchar('\n');
	}
	else
	for (x = quadx-1; x <= quadx+1; x++) {
		for (y = quady-1; y <= quady+1; y++) {
			if (x == 0 || x > 8 || y == 0 || y > 8)
				printf("   -1");
			else {
				printf("%5d", g_d.galaxy[x][y]);
				// If radio works, mark star chart so
				// it will show current information.
				// Otherwise mark with current
				// value which is fixed. 
				starch[x][y] = damage[DRADIO] > 0 ? g_d.galaxy[x][y]+1000 :1;
			}
		}
		putchar('\n');
	}

}

void dreprt(void) {
	int jdam = FALSE, i;
	chew();

	for (i = 1; i <= ndevice; i++) {
		if (damage[i] > 0.0) {
			if (!jdam) {
				skip(1);
				prout("DEVICE            -REPAIR TIMES-");
				prout("                IN FLIGHT   DOCKED");
				jdam = TRUE;
			}
			printf("  %16s ", device[i]);
			cramf(damage[i]+0.05, 8, 2);
			proutn("  ");
			cramf(docfac*damage[i]+0.005, 8, 2);
			skip(1);
		}
	}
	if (!jdam) prout("All devices functional.");
}

void chart(int nn) {
	int i,j;

	chew();
	skip(1);
	if (ship_date_chart_damaged != 1e30 && ship_date_chart_damaged != g_d.stardate && ship_condition == IHDOCKED) {
		prout("Spock-  \"I revised the Star Chart from the");
		prout("  starbase's records.\"");
		skip(1);
	}
	if (nn == 0) prout("STAR CHART FOR THE KNOWN GALAXY");
	if (ship_date_chart_damaged != 1e30) {
		if (ship_condition == IHDOCKED) {
			/* We are docked, so restore chart from base information -- these values won't update! */
			ship_date_chart_damaged = g_d.stardate;
			for (i=1; i <= 8 ; i++)
				for (j=1; j <= 8; j++)
					if (starch[i][j] == 1) starch[i][j] = g_d.galaxy[i][j]+1000;
		}
		else {
			proutn("(Last surveillance update ");
			cramf(g_d.stardate-ship_date_chart_damaged, 0, 1);
			prout(" stardates ago.)");
		}
	}
	if (nn ==0) skip(1);

	prout("      1    2    3    4    5    6    7    8");
	prout("    ----------------------------------------");
	if (nn==0) prout("  -");
	if (coordfixed)
	for (j = 8; j >= 1; j--) {
		printf("%d -", j);
		for (i = 1; i <= 8; i++) {
			if (starch[i][j] < 0) // We know only about the bases
				printf("  .1.");
			else if (starch[i][j] == 0) // Unknown
				printf("  ...");
			else if (starch[i][j] > 999) // Memorized value
				printf("%5d", starch[i][j]-1000);
			else
				printf("%5d", g_d.galaxy[i][j]); // What is actually there (happens when value is 1)
		}
		prout("  -");
	}
	else
	for (i = 1; i <= 8; i++) {
		printf("%d -", i);
		for (j = 1; j <= 8; j++) {
			if (starch[i][j] < 0) // We know only about the bases
				printf("  .1.");
			else if (starch[i][j] == 0) // Unknown
				printf("  ...");
			else if (starch[i][j] > 999) // Memorized value
				printf("%5d", starch[i][j]-1000);
			else
				printf("%5d", g_d.galaxy[i][j]); // What is actually there (happens when value is 1)
		}
		prout("  -");
	}
	if (nn == 0) {
		skip(1);
		crmshp();
		proutn(" is currently in");
		cramlc(1, quadx, quady);
		skip(1);
	}
}
		
		
void srscan(int l) {
	static char requests[][3] =
		{"","da","co","po","ls","wa","en","to","sh","kl","ti"};
	char *cp;
	int leftside=TRUE, rightside=TRUE, i, j, /*jj,*/ k=0, nn=FALSE;
	int goodScan=TRUE;
	switch (l) {
		case 1: // SRSCAN
			if (damage[DSRSENS] != 0) {
				/* Allow base's sensors if docked */
				if (ship_condition != IHDOCKED) {
					prout("SHORT-RANGE SENSORS DAMAGED");
					goodScan=FALSE;
				}
				else
					prout("[Using starbase's sensors]");
			}
			if (goodScan)
				starch[quadx][quady] = damage[DRADIO]>0.0 ?
									   g_d.galaxy[quadx][quady]+1000:1;
			scan();
			if (isit("chart")) nn = TRUE;
			if (isit("no")) rightside = FALSE;
			chew();
			prout("\n    1 2 3 4 5 6 7 8 9 10");
			break;
		case 2: // REQUEST
			while (scan() == IHEOL)
				printf("Information desired? ");
			chew();
			for (k = 1; k <= 10; k++)
				if (strncmp(citem,requests[k],min(2,strlen(citem)))==0)
					break;
			if (k > 10) {
				prout("UNRECOGNIZED REQUEST. Legal requests are:\n"
					 "  stardate, condition, position, lsupport, warpfactor,\n"
					 "  energy, torpedoes, shields, klingons, time.");
				return;
			}
			// no "break"
		case 3: // STATUS
			chew();
			leftside = FALSE;
			skip(1);
	}
	for (i = 1; i <= 10; i++) {
		int jj = (k!=0 ? k : i);
		if (leftside) {
			if (coordfixed) {
				printf("%2d  ", 11-i);
				for (j = 1; j <= 10; j++) {
					if (goodScan || (abs((11-i)-secty)<= 1 && abs(j-sectx) <= 1))
						printf("%c ",quad[j][11-i]);
					else
						printf("- ");
				}
			} else {
				printf("%2d  ", i);
				for (j = 1; j <= 10; j++) {
					if (goodScan || (abs(i-sectx)<= 1 && abs(j-secty) <= 1))
						printf("%c ",quad[i][j]);
					else
						printf("- ");
				}
			}
		}
		if (rightside) {
			switch (jj) {
				case 1:
					printf(" Stardate      %.1f", g_d.stardate);
					break;
				case 2:
					if (ship_condition != IHDOCKED) newcnd();
					switch (ship_condition) {
						case IHRED: cp = "RED"; break;
						case IHGREEN: cp = "GREEN"; break;
						case IHYELLOW: cp = "YELLOW"; break;
						case IHDOCKED: cp = "DOCKED"; break;
					}
					printf(" Condition     %s", cp);
#ifdef CLOAKING
				    if (iscloaked) printf(", CLOAKED");
#endif
					break;
				case 3:
					printf(" Position     ");
					cramlc(0, quadx, quady);
					putchar(',');
					cramlc(0, sectx, secty);
					break;
				case 4:
					printf(" Life Support  ");
					if (damage[DLIFSUP] != 0.0) {
						if (ship_condition == IHDOCKED)
							printf("DAMAGED, supported by starbase");
						else
							printf("DAMAGED, reserves=%4.2f", ship_life_support_reserves);
					}
					else
						printf("ACTIVE");
					break;
				case 5:
					printf(" Warp Factor   %.1f", warp_factor);
					break;
				case 6:
					printf(" Energy        %.2f", ship_energy);
					break;
				case 7:
					printf(" Torpedoes     %d", torps);
					break;
				case 8:
					printf(" Shields       ");
					if (damage[DSHIELD] != 0)
						printf("DAMAGED,");
					else if (is_shield_up)
						printf("UP,");
					else
						printf("DOWN,");
					printf(" %d%% %.1f units",
						   (int)((100.0*ship_shield_strength)/ship_max_shield + 0.5), ship_shield_strength);
					break;
				case 9:
					printf(" Klingons Left %d", g_d.remaining_klingons);
					break;
				case 10:
					printf(" Time Left     %.2f", g_d.remaining_time);
					break;
			}
					
		}
		skip(1);
		if (k!=0) return;
	}
	if (nn) chart(1);
}
			
			
void eta(void) {
	int /*key,*/ ix1, ix2, iy1, iy2, prompt=FALSE;
	int wfl;
	double ttime, twarp, tpower;
	if (damage[DCOMPTR] != 0.0) {
		prout("COMPUTER DAMAGED, USE A POCKET CALCULATOR.");
		skip(1);
		return;
	}
	if (scan() != IHREAL) {
		prompt = TRUE;
		chew();
		proutn("Destination quadrant and/or sector? ");
		if (scan()!=IHREAL) {
			huh();
			return;
		}
	}
	iy1 = aaitem +0.5;
	if (scan() != IHREAL) {
		huh();
		return;
	}
	ix1 = aaitem + 0.5;
	if (scan() == IHREAL) {
		iy2 = aaitem + 0.5;
		if (scan() != IHREAL) {
			huh();
			return;
		}
		ix2 = aaitem + 0.5;
	}
	else {	// same quadrant
		ix2 = ix1;
		iy2 = iy1;
		ix1 = quady;	// ya got me why x and y are reversed!
		iy1 = quadx;
	}

	if (ix1 > 8 || ix1 < 1 || iy1 > 8 || iy1 < 1 ||
		ix2 > 10 || ix2 < 1 || iy2 > 10 || iy2 < 1) {
		huh();
		return;
	}
	dist = sqrt(square(iy1-quadx+0.1*(iy2-sectx))+
				square(ix1-quady+0.1*(ix2-secty)));
	wfl = FALSE;

	if (prompt) prout("Answer \"no\" if you don't know the value:");
	while (TRUE) {
		chew();
		proutn("Time or arrival stardate? ");
		if (scan()==IHREAL) {
			ttime = aaitem;
			if (ttime > g_d.stardate) ttime -= g_d.stardate; // Actually a star stardate
			if (ttime <= 1e-10 ||
				(twarp=(floor(sqrt((10.0*dist)/ttime)*10.0)+1.0)/10.0) > 10) {
				prout("We'll never make it, sir.");
				chew();
				return;
			}
			if (twarp < 1.0) twarp = 1.0;
			break;
		}
		chew();
		proutn("Warp factor? ");
		if (scan()== IHREAL) {
			wfl = TRUE;
			twarp = aaitem;
			if (twarp<1.0 || twarp > 10.0) {
				huh();
				return;
			}
			break;
		}
		prout("Captain, certainly you can give me one of these.");
	}
	while (TRUE) {
		chew();
		ttime = (10.0*dist)/square(twarp);
		tpower = dist*twarp*twarp*twarp*(is_shield_up+1);
		if (tpower >= ship_energy) { // Suggestion from Ethan Staffin -- give amount needed
			prout("Insufficient energy, sir: we would need ");
			cramf(tpower, 1, 1);
			proutn (" units.");
			if (is_shield_up==0 || tpower > ship_energy*2.0) {
				if (!wfl) return;
				proutn("New warp factor to try? ");
				if (scan() == IHREAL) {
					wfl = TRUE;
					twarp = aaitem;
					if (twarp<1.0 || twarp > 10.0) {
						huh();
						return;
					}
					continue;
				}
				else {
					chew();
					skip(1);
					return;
				}
			}
			prout("But if you lower your shields,");
			proutn("remaining");
			tpower /= 2;
		}
		else
			proutn("Remaining");
		proutn(" energy will be ");
		cramf(ship_energy-tpower, 1, 1);
		prout(".");
		if (wfl) {
			proutn("And we will arrive at stardate ");
			cramf(g_d.stardate+ttime, 1, 1);
			prout(".");
		}
		else if (twarp==1.0)
			prout("Any warp speed is adequate.");
		else {
			proutn("Minimum warp needed is ");
			cramf(twarp, 1, 2);
			skip(1);
			proutn("and we will arrive at stardate ");
			cramf(g_d.stardate+ttime, 1, 2);
			prout(".");
		}
		if (g_d.remaining_time < ttime)
			prout("Unfortunately, the Federation will be destroyed by then.");
		if (twarp > 6.0)
			prout("You'll be taking risks at that speed, Captain");
		if ((is_supercommander_attacking_base==1 && g_d.qy_supercommander == ix1 && g_d.qx_supercommander == iy1 &&
			 future[FSCDBAS]< ttime+g_d.stardate)||
			(future[FCDBAS]<ttime+g_d.stardate && baty==ix1 && batx == iy1))
			prout("The starbase there will be destroyed by then.");
		proutn("New warp factor to try? ");
		if (scan() == IHREAL) {
			wfl = TRUE;
			twarp = aaitem;
			if (twarp<1.0 || twarp > 10.0) {
				huh();
				return;
			}
		}
		else {
			chew();
			skip(1);
			return;
		}
	}
			
}
