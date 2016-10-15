#include <time.h>
#include "sst.h"

void prelim(void) {
	skip(2);
	prout("-SUPER- STAR TREK");
	skip(1);
	prout("Latest update-21 Sept 78");
	skip(1);
}

void freeze(int boss) {
//	char *x, *y;
	FILE *fp;
	int key;
	if (boss) {
		strcpy(citem, "emsave.trk");
	}
	else {
		if ((key = scan()) == IHEOL) {
			proutn("File name(9 characters maximum): ");
			key = scan();
		}
		if (key != IHALPHA) {
			huh();
			return;
		}
		chew();
		if (strchr(citem, '.') == NULL) {
			strcat(citem, ".trk");
		}
	}
	if ((fp = fopen(citem, "wb")) == NULL) {
		proutn("Can't freeze game as file ");
		proutn(citem);
		skip(1);
		return;
	}
	fwrite(&g_d, sizeof(g_d), 1, fp);
	fwrite(&snapsht, sizeof(snapsht), 1, fp);
	fwrite(quad, sizeof(quad), 1, fp);
	fwrite(kx, sizeof(kx), 1, fp);
	fwrite(ky, sizeof(ky), 1, fp);
	fwrite(starch, sizeof(starch), 1, fp);
	fwrite(kpower, sizeof(kpower), 1, fp);
	fwrite(kdist, sizeof(kdist), 1, fp);
	fwrite(kavgd, sizeof(kavgd), 1, fp);
	fwrite(damage, sizeof(damage), 1, fp);
	fwrite(future, sizeof(future), 1, fp);
	fwrite(&g_a, sizeof(g_a), 1, fp);
	fwrite(passwd, sizeof(passwd), 1, fp);

	fclose(fp);

	/* I hope that's enough! */
}


void thaw(void) {
//	char *x, *y;
	FILE *fp;
	int key;

	passwd[0] = '\0';
	if ((key = scan()) == IHEOL) {
		proutn("File name: ");
		key = scan();
	}
	if (key != IHALPHA) {
		huh();
		return;
	}
	chew();
	if (strchr(citem, '.') == NULL) {
		strcat(citem, ".trk");
	}
	if ((fp = fopen(citem, "rb")) == NULL) {
		proutn("Can't find game file ");
		proutn(citem);
		skip(1);
		return;
	}
	fread(&g_d, sizeof(g_d), 1, fp);
	fread(&snapsht, sizeof(snapsht), 1, fp);
	fread(quad, sizeof(quad), 1, fp);
	fread(kx, sizeof(kx), 1, fp);
	fread(ky, sizeof(ky), 1, fp);
	fread(starch, sizeof(starch), 1, fp);
	fread(kpower, sizeof(kpower), 1, fp);
	fread(kdist, sizeof(kdist), 1, fp);
	fread(kavgd, sizeof(kavgd), 1, fp);
	fread(damage, sizeof(damage), 1, fp);
	fread(future, sizeof(future), 1, fp);
	fread(&g_a, sizeof(g_a), 1, fp);
	fread(passwd, sizeof(passwd), 1, fp);

	fclose(fp);

	/* I hope that's enough! */
}

void abandn(void) {
	int nb, l;

	chew();
	if (ship_condition==IHDOCKED) {
		if (ship_name!=IH_ENTERPRISE) {
			prout("You cannot abandon Ye Faerie Queene.");
			return;
		}
	}
	else {
		/* Must take shuttle craft to exit */
		if (damage[DSHUTTL]==-1) {
			prout("Ye Faerie Queene has no shuttle craft.");
			return;
		}
		if (damage[DSHUTTL]<0) {
			prout("Shuttle craft now serving Big Mac's.");
			return;
		}
		if (damage[DSHUTTL]>0) {
			prout("Shuttle craft damaged.");
			return;
		}
		if (is_landed==1) {
			prout("You must be aboard the Enterprise.");
			return;
		}
		if (has_suttlecraft!=1) {
			prout("Shuttle craft not currently available.");
			return;
		}
		/* Print abandon ship messages */
		skip(1);
		prouts("***ABANDON ship!  ABANDON ship!");
		skip(1);
		prouts("***ALL HANDS ABANDON ship!");
		skip(2);
		prout("Captain and crew escape in shuttle craft.");
		prout("Remainder of ship's complement beam down");
		prout("to nearest habitable planet.");
		if (g_d.remaining_bases==0) {
			/* Ops! no place to go... */
			finish(FABANDN);
			return;
		}
		/* If at least one base left, give 'em the Faerie Queene */
		skip(1);
		have_crystals = 0; /* crystals are lost */
		remaining_probes = 0; /* No probes */
		prout("You are captured by Klingons and released to");
		prout("the Federation in a prisoner-of-war exchange.");
		nb = Rand()*g_d.remaining_bases+1;
		/* Set up quadrant and position FQ adjacient to base */
		if (quadx!=g_d.qx_base[nb] || quady!=g_d.qy_base[nb]) {
			quadx = g_d.qx_base[nb];
			quady = g_d.qy_base[nb];
			sectx = secty = 5;
			newqad(1);
		}
		for (;;) {
			/* position next to base by trial and error */
			quad[sectx][secty] = IHDOT;
			for (l = 1; l <= 10; l++) {
				sectx = 3.0*Rand() - 1.0 + currentq_base_sx;
				secty = 3.0*Rand() - 1.0 + currentq_base_sy;
				if (sectx >= 1 && sectx <= 10 &&
					secty >= 1 && secty <= 10 &&
					quad[sectx][secty] == IHDOT) break;
			}
			if (l < 11) break; /* found a spot */
			sectx=5;
			secty=5;
			newqad(1);
		}
	}
	/* Get new commission */
	quad[sectx][secty] = ship_name = IH_FAERIE_QUEEN;
	prout("Starfleet puts you in command of another ship,");
	prout("the Faerie Queene, which is antiquated but,");
	prout("still useable.");
	if (have_crystals!=0) prout("The dilithium crystals have been moved.");
	is_mining=0;
	has_suttlecraft=0; /* Gallileo disappears */
#ifdef CAPTURE
	brigcapacity = 300; // Less capacity now
	brigfree = brigcapacity;
#endif
#ifdef CLOAKING
    iscloaked = iscloaking = FALSE;
#endif
	/* Resupply ship */
	ship_condition=IHDOCKED;
	for (l = 1; l <= ndevice; l++) damage[l] = 0.0;
	damage[DSHUTTL] = -1;
	ship_energy = ship_max_energy = 3000.0;
	ship_shield_strength = ship_max_shield = 1250.0;
	torps = intorps = 6;
	ship_life_support_reserves=game_initial_lifesupport=3.0;
	is_shield_up=0;
	warp_factor=5.0;
	warp_factor_squared=25.0;
	return;
}
	
void setup(void) {
	int i,j, krem, klumper;
	int ix, iy;
	alldone = gamewon = 0;
#ifdef DEBUG
	idebug = 0;
#endif
	//  Decide how many of everything
	if (choose()) return; // frozen game
	// Prepare the Enterprise
	ship_name = IH_ENTERPRISE;
	ship_energy = ship_max_energy = 5000.0;
	ship_shield_strength = ship_max_shield = 2500.0;
	is_shield_changing = is_shield_up = 0;
	game_initial_lifesupport = 4.0;
	ship_life_support_reserves = 4.0;
	iran8(&quadx, &quady);
	iran10(&sectx, &secty);
	torps = intorps = 10;
	remaining_probes = (int)(3.0*Rand() + 2.0);	/* Give them 2-4 of these wonders */
	warp_factor = 5.0;
	warp_factor_squared = warp_factor * warp_factor;
	for (i=0; i <= ndevice; i++) damage[i] = 0.0;
	// Set up assorted game parameters
	batx = baty = 0;
	g_d.stardate = game_initial_stardate = 100.0*(int)(31.0*Rand()+20.0);
	g_d.killed_klingons = g_d.killed_commanders = game_num_intergalactic_attempts = game_num_help_calls = resting = num_casualties = g_d.killed_romulans = 0;
// Added g_d.killed_stars=0 6/2015
    is_supercommander_attacking_base = currentq_is_supercommander_here = is_mining = have_crystals = is_aboard_shuttle = g_d.killed_supercommanders = g_d.killed_planets = g_d.killed_stars = 0;
	has_suttlecraft = 1;
	is_landed = -1;
	alive = 1;
	docfac = 0.25;
	for (i = 1; i <= 8; i++)
		for (j = 1; j <= 8; j++) g_d.newstuf[i][j] = starch[i][j] = 0;
	// Initialize times for extraneous events
	future[FSNOVA] = g_d.stardate + expran(0.5 * game_initial_time);
	future[FTBEAM] = g_d.stardate + expran(1.5 * (game_initial_time / g_d.remaining_commanders));
	future[FSNAP] = g_d.stardate + 1.0 + Rand(); // Force an early snapshot
	future[FBATTAK] = g_d.stardate + expran(0.3*game_initial_time);
	future[FCDBAS] = 1e30;
	future[FSCMOVE] = g_d.remaining_supercommanders ? g_d.stardate+0.2777 : 1e30;
	future[FSCDBAS] = 1e30;
	future[FDSPROB] = 1e30;
	// Starchart is functional
	ship_date_chart_damaged = 1e30;
	// Put stars in the galaxy
	initial_stars = 0;
	for (i=1; i<=8; i++)
		for (j=1; j<=8; j++) {
			int k = Rand()*9.0 + 1.0;
			initial_stars += k;
			g_d.galaxy[i][j] = k;
		}
	// Locate star bases in galaxy
	for (i = 1; i <= initial_bases; i++) {
		int contflag;
		do {
			do iran8(&ix, &iy);
			while (g_d.galaxy[ix][iy] >= 10);
			contflag = FALSE;
			for (j = i-1; j > 0; j--) {
				/* Improved placement algorithm to spread out bases */
				double distq = square(ix-g_d.qx_base[j]) + square(iy-g_d.qy_base[j]);
				if (distq < 6.0*(6-initial_bases) && Rand() < 0.75) {
					contflag = TRUE;
#ifdef DEBUG
					printf("DEBUG: Abandoning base #%d at %d-%d\n", i, ix, iy);
#endif
					break;
				}
#ifdef DEBUG
				else if (distq < 6.0 * (6-initial_bases)) {
					printf("DEBUG: saving base #%d, close to #%d\n", i, j);
				}
#endif
			}
		} while (contflag);
			
		g_d.qx_base[i] = ix;
		g_d.qy_base[i] = iy;
		starch[ix][iy] = -1;
		g_d.galaxy[ix][iy] += 10;
	}
	// Position ordinary Klingon Battle Cruisers
	krem = initial_klingons - initial_commanders - g_d.remaining_supercommanders;
	klumper = 0.25*game_skill*(9.0-game_length)+1.0;
	if (klumper > 9) klumper = 9; // Can't have more than 9 in quadrant
	do {
		double r = Rand();
		int klump = (1.0 - r*r)*klumper;
		if (klump > krem) klump = krem;
		krem -= klump;
		klump *= 100;
		do iran8(&ix, &iy);
		while (g_d.galaxy[ix][iy] + klump >= 1000);
		g_d.galaxy[ix][iy] += klump;
	} while (krem > 0);
	// Position Klingon Commander Ships
#ifdef DEBUG
	klumper = 1;
#endif
	for (i = 1; i <= initial_commanders; i++) {
		do {
			do { /* IF debugging, put commanders by bases, always! */
#ifdef DEBUG
				if (idebug && klumper <= initial_bases) {
					ix = g_d.qx_base[klumper];
					iy = g_d.qy_base[klumper];
					klumper++;
				}
				else
#endif
					iran8(&ix, &iy);
			}
			while ((g_d.galaxy[ix][iy] < 99 && Rand() < 0.75)||
				   g_d.galaxy[ix][iy]>899);
			// check for duplicate
			for (j = 1; j < i; j++)
				if (g_d.qx_commander[j]==ix && g_d.qy_commander[j]==iy) break;
		} while (j < i);
		g_d.galaxy[ix][iy] += 100;
		g_d.qx_commander[i] = ix;
		g_d.qy_commander[i] = iy;
	}
	// Locate planets in galaxy
	for (i = 1; i <= inplan; i++) {
		do iran8(&ix, &iy);
		while (g_d.newstuf[ix][iy] > 0);
		g_d.newstuf[ix][iy] = 1;
		g_d.plnets[i].qx = ix;
		g_d.plnets[i].qy = iy;
		g_d.plnets[i].pclass = Rand()*3.0 + 1.0; // Planet class M N or O
		g_d.plnets[i].crystals = 1.5*Rand();		// 1 in 3 chance of crystals
		g_d.plnets[i].known = 0;
	}
	// Locate Romulans
	for (i = 1; i <= g_d.remaining_romulans; i++) {
		iran8(&ix, &iy);
		g_d.newstuf[ix][iy] += 10;
	}
	// Locate the Super Commander
	if (g_d.remaining_supercommanders > 0) {
		do iran8(&ix, &iy);
		while (g_d.galaxy[ix][iy] >= 900);
		g_d.qx_supercommander = ix;
		g_d.qy_supercommander = iy;
		g_d.galaxy[ix][iy] += 100;
	}
	// Place thing (in tournament game, qx_thing == -1, don't want one!)
	if (Rand() < 0.1 && qx_thing != -1) {
		iran8(&qx_thing, &qy_thing);
	}
	else {
		qx_thing = qy_thing = 0;
	}

//	idate = stardate;
	skip(3);
	g_d.snap = 0;
		
	if (game_skill == SNOVICE) {
		printf("It is stardate %d. The Federation is being attacked by\n",
			   (int)g_d.stardate);
		printf("a deadly Klingon invasion force. As captain of the United\n"
			   "Starship U.S.S. Enterprise, it is your mission to seek out\n"
			   "and destroy this invasion force of %d battle cruisers.\n",
			   initial_klingons);
		printf("You have an initial allotment of %d stardates to complete\n"
			   "your mission.  As you proceed you may be given more time.\n\n"
			   "You will have %d supporting starbases.\n"
			   "Starbase locations-  ",
			   (int)game_initial_time, initial_bases);
	}
	else {
		printf("Stardate %d.\n\n"
			   "%d Klingons,\nan unknown number of Romulans\n",
			   (int)g_d.stardate, initial_klingons);
		if (g_d.remaining_supercommanders) printf("and one (GULP) Super-Commander.\n");
		printf("%d stardates\n%d starbases in  ",(int)game_initial_time, initial_bases);
	}
	for (i = 1; i <= initial_bases; i++) {
		cramlc(0, g_d.qx_base[i], g_d.qy_base[i]);
		if (i < initial_bases) proutn("  ");
	}
	skip(2);
	proutn("The Enterprise is currently in");
	cramlc(1, quadx, quady);
	proutn(" ");
	cramlc(2, sectx, secty);
	skip(2);
	prout("Good Luck!");
	if (g_d.remaining_supercommanders) proutn("  YOU'LL NEED IT.");
	skip(1);
	newqad(0);
	if (currentq_num_enemies) is_shield_up=1.0;
	if (in_neutral_zone) attack(0);	// bad luck to start in a Romulan Neutral Zone
}

int choose(void) {
	is_tournament_game = 0;
	thawed = 0;
	game_skill = 0;
	game_length = 0;
	while (TRUE) {
		if (fromcommandline) /* Can start with command line options */
			fromcommandline = 0;
		else
			proutn("Would you like a regular, tournament, or frozen game?");
		scan();
		if (strlen(citem)==0) continue; // Try again
		if (isit("tournament")) {
			while (scan() == IHEOL) {
				proutn("Type in tournament number-");
			}
			if (aaitem == 0) {
				chew();
				continue; // We don't want a blank entry
			}
			is_tournament_game = (int)aaitem;
			qx_thing = -1;
			srand((unsigned int)(int)aaitem);
			break;
		}
		if (isit("frozen")) {
			thaw();
			chew();
			if (*passwd==0) continue;
			randomize();
			Rand(); Rand(); Rand(); Rand();
			if (!alldone) thawed = 1; // No plaque if not finished
			report(1);
			return TRUE;
		}
		if (isit("regular")) {
			skip(2);
			randomize();
			Rand(); Rand(); Rand(); Rand();
			break;
		}
		proutn("What is \"");
		proutn(citem);
		prout("\"?");
		chew();
	}
	while (game_length==0 || game_skill==0) {
		if (scan() == IHALPHA) {
			if (isit("short")) game_length = 1;
			else if (isit("medium")) game_length = 2;
			else if (isit("long")) game_length = 4;
			else if (isit("novice")) game_skill = SNOVICE;
			else if (isit("fair")) game_skill = SFAIR;
			else if (isit("good")) game_skill = SGOOD;
			else if (isit("expert")) game_skill = SEXPERT;
			else if (isit("emeritus")) game_skill = SEMERITUS;
			else {
				proutn("What is \"");
				proutn(citem);
				prout("\"?");
			}
		}
		else {
			chew();
			if (game_length==0) proutn("Would you like a Short, Medium, or Long game? ");
			else if (game_skill == 0) proutn("Are you a Novice, Fair, Good, Expert, or Emeritus player?");
		}
	}
	while (TRUE) {
		scan();
		strcpy(passwd, citem);
		chew();
		if (*passwd != 0) break;
		proutn("Please type in a secret password (9 characters maximum)-");
	}
#ifdef DEBUG
	if (strcmp(passwd, "debug")==0) idebug = 1;
#endif

	// Use parameters to generate initial values of things
	game_damage_factor = 0.5 * game_skill;
	g_d.remaining_bases = 3.0*Rand()+2.0;
	initial_bases = g_d.remaining_bases;
	inplan = (PLNETMAX/2) + (PLNETMAX/2+1)*Rand();
	g_d.remaining_romulans = (2.0+Rand())*game_skill;
	g_d.remaining_supercommanders = (game_skill > SFAIR? 1 : 0);
	g_d.remaining_time = 7.0 * game_length;
	game_initial_time = g_d.remaining_time;
	g_d.remaining_klingons = 2.0*game_initial_time*((game_skill+1 - 2*Rand())*game_skill*0.1+.15); // g_d.remaining_klingons and initial_klingons includes commanders and SC
	initial_klingons = g_d.remaining_klingons;
	initial_commanders = game_skill + 0.0625*initial_klingons*Rand();
	g_d.remaining_commanders= min(10, initial_commanders);
	initial_commanders = g_d.remaining_commanders;
	g_d.remaining_resources = (initial_klingons+4*initial_commanders)*game_initial_time;
	game_initial_resources = g_d.remaining_resources;
	if (initial_klingons > 50) {
		initial_bases = (g_d.remaining_bases += 1);
    }
#ifdef CAPTURE
	brigcapacity = 400;
    brigfree = brigcapacity;
    captured_klingons = 0; // TAA fix 6/2015
#endif
#ifdef CLOAKING
    num_cloak_violations = 0; // TAA fix 6/2015
    iscloaked = FALSE;
    iscloaking = FALSE;
#endif
	return FALSE;
}

void dropin(int iquad, int *ix, int *iy) {
	do iran10(ix, iy);
	while (quad[*ix][*iy] != IHDOT);
	quad[*ix][*iy] = iquad;
}

void newcnd(void) {
	ship_condition = IHGREEN;
	if (ship_energy < 1000.0) ship_condition = IHYELLOW;
	if (g_d.galaxy[quadx][quady] > 99 || g_d.newstuf[quadx][quady] > 9)
		ship_condition = IHRED;
}


void newqad(int shutup) {
	int quadnum = g_d.galaxy[quadx][quady];
	int newnum = g_d.newstuf[quadx][quady];
	int i, j, ix, iy, nplan;

	iattak = 1;
	justin = 1;
	currentq_base_sx = currentq_base_sy = 0;
	currentq_num_klingons = 0;
	currentq_num_commanders = 0;
	currentq_planet_sx = currentq_planet_sy = 0;
	currentq_has_supercommander = 0;
	currentq_num_romulans = 0;
	currentq_planet_id = 0;
	currentq_num_enemies = 0;
	in_neutral_zone = 0;
	is_inorbit = 0;
	is_landed = -1;
	ientesc = 0;
	currentq_has_tholian = 0;
    has_seen_attack_report = 0;

#ifdef CLOAKING
    is_cloak_violation_reported = FALSE;
#endif
	if (currentq_is_supercommander_here) {
		// Attempt to escape Super-commander, so tbeam back!
		currentq_is_supercommander_here = 0;
		ientesc = 1;
	}
	// Clear quadrant
	for (i=1; i <= 10; i++)
		for (j=1; j <= 10; j++) quad[i][j] = IHDOT;
	// cope with supernova
	if (quadnum > 999) {
		return;
	}
	currentq_num_klingons = quadnum/100;
	currentq_num_romulans = newnum/10;
	nplan = newnum%10;
	currentq_num_enemies = currentq_num_klingons + currentq_num_romulans;

	// Position Starship
	quad[sectx][secty] = ship_name;

	// Decide if quadrant needs a Tholian
	if ((game_skill <  SGOOD && Rand() <= 0.02) ||   /* Lighten up if game_skill is low */
		(game_skill == SGOOD && Rand() <= 0.05) ||
		(game_skill > SGOOD && Rand() <= 0.08)
#ifdef DEBUG
		|| strcmp(passwd, "tholianx")==0
#endif
		) {
		do {
			currentq_tholian_sx = Rand() > 0.5 ? 10 : 1;
			currentq_tholian_sy = Rand() > 0.5 ? 10 : 1;
		} while (quad[currentq_tholian_sx][currentq_tholian_sy] != IHDOT);
		quad[currentq_tholian_sx][currentq_tholian_sy] = IH_THOLIAN;
		currentq_has_tholian = 1;
		/* Reserve unocupied corners */
		if (quad[1][1]==IHDOT) quad[1][1] = 'X';
		if (quad[1][10]==IHDOT) quad[1][10] = 'X';
		if (quad[10][1]==IHDOT) quad[10][1] = 'X';
		if (quad[10][10]==IHDOT) quad[10][10] = 'X';
	}

	if (quadnum >= 100) {
		// Position ordinary Klingons
		quadnum -= 100*currentq_num_klingons;
		for (i = 1; i <= currentq_num_klingons; i++) {
			dropin(IH_KLINGON, &ix, &iy);
			kx[i] = ix;
			ky[i] = iy;
			kdist[i] = kavgd[i] = sqrt(square(sectx-ix) + square(secty-iy));
			kpower[i] = Rand()*150.0 +300.0 +25.0*game_skill;
		}
		// If we need a commander, promote a Klingon
		for (i = 1; i <= g_d.remaining_commanders ; i++) 
			if (g_d.qx_commander[i]==quadx && g_d.qy_commander[i]==quady) break;
			
		if (i <= g_d.remaining_commanders) {
			quad[ix][iy] = IH_COMMANDER;
			kpower[currentq_num_klingons] = 950.0+400.0*Rand()+50.0*game_skill;
			currentq_num_commanders = 1;
		}

		// If we need a super-commander, promote a Klingon
		if (quadx == g_d.qx_supercommander && quady == g_d.qy_supercommander) {
			quad[kx[1]][ky[1]] = IH_SUPER_COMMANDER;
			kpower[1] = 1175.0 + 400.0*Rand() + 125.0*game_skill;
			currentq_is_supercommander_here = 1;
			currentq_has_supercommander = 1;
		}
	}
	// Put in Romulans if needed
	for (i = currentq_num_klingons+1; i <= currentq_num_enemies; i++) {
		dropin(IH_ROMULAN, &ix, &iy);
		kx[i] = ix;
		ky[i] = iy;
		kdist[i] = kavgd[i] = sqrt(square(sectx-ix) + square(secty-iy));
		kpower[i] = Rand()*400.0 + 450.0 + 50.0*game_skill;
	}
	sortkl();
	// If quadrant needs a starbase, put it in
	if (quadnum >= 10) {
		quadnum -= 10;
		dropin(IH_BASE, &currentq_base_sx, &currentq_base_sy);
	}
	
	if (nplan) {
		// If quadrant needs a planet, put it in
		for (i=1; i <= inplan; i++)
			if (g_d.plnets[i].qx == quadx && g_d.plnets[i].qy == quady) break;
		if (i <= inplan) {
			currentq_planet_id = i;
			dropin(IH_PLANET, &currentq_planet_sx, &currentq_planet_sy);
		}
	}
	// Check for condition
	newcnd();
	// And finally the stars
	for (i = 1; i <= quadnum; i++) dropin(IH_STAR, &ix, &iy);

	// Check for RNZ
	if (currentq_num_romulans > 0 && currentq_num_klingons == 0 && currentq_base_sx == 0) {
		in_neutral_zone = 1;
		if (REPORTS) { 
			skip(1);
			prout("LT. UHURA- \"Captain, an urgent message.");
			prout("  I'll put it on audio.\"  CLICK");
			skip(1);
			prout("INTRUDER! YOU HAVE VIOLATED THE ROMULAN NEUTRAL ZONE.");
			prout("LEAVE AT ONCE, OR YOU WILL BE DESTROYED!");
		}
	}

	if (shutup==0) {
		// Put in THING if needed
		if (qx_thing == quadx && qy_thing == quady) {
			dropin(IHQUEST, &ix, &iy);
			qx_thing = qy_thing = 0; // Transient
			if (damage[DSRSENS] == 0.0) {
				skip(1);
				prout("MR. SPOCK- \"Captain, this is most unusual.");
				prout("    Please examine your short-range scan.\"");
			}
		}
	}

	// Put in a few black holes
	for (i = 1; i <= 3; i++)
		if (Rand() > 0.5) dropin(IH_BLACK_HOLE, &ix, &iy);

	// Take out X's in corners if Tholian present
	if (currentq_has_tholian) {
		if (quad[1][1]=='X') quad[1][1] = IHDOT;
		if (quad[1][10]=='X') quad[1][10] = IHDOT;
		if (quad[10][1]=='X') quad[10][1] = IHDOT;
		if (quad[10][10]=='X') quad[10][10] = IHDOT;
	}		
}

void sortkl(void) {
	double t;
	int sw, j, k;

	// The author liked bubble sort. So we will use it. :-(

	if (currentq_num_enemies < 2) return;

	do {
		sw = FALSE;
		for (j = 1; j < currentq_num_enemies; j++)
			if (kdist[j] > kdist[j+1]) {
				sw = TRUE;
				t = kdist[j];
				kdist[j] = kdist[j+1];
				kdist[j+1] = t;
				t = kavgd[j];
				kavgd[j] = kavgd[j+1];
				kavgd[j+1] = t;
				k = kx[j];
				kx[j] = kx[j+1];
				kx[j+1] = k;
				k = ky[j];
				ky[j] = ky[j+1];
				ky[j+1] = k;
				t = kpower[j];
				kpower[j] = kpower[j+1];
				kpower[j+1] = t;
			}
	} while (sw);
}
