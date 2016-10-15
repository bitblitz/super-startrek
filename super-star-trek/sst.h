#ifdef WINDOWS
#pragma warning(disable: 4244)
#endif

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#ifndef INCLUDED
#define EXTERN extern
#else
#define EXTERN
#endif

#ifdef WINDOWS
#define DEBUG
#define SCORE
#define CLOAKING
#define CAPTURE
#endif

#ifdef CLOAKING
#define ndevice (16)
#else
#define ndevice (15)	// Number of devices
#endif
#define PHASER_DAMAGE_FACTOR (2.0)
#define PLNETMAX (10)
#define NEVENTS (8)

typedef struct {
	int qx;	/* Quadrant location of planet */
	int qy;
	int pclass; /* class M, N, or O (1, 2, or 3) */
	int crystals; /* has crystals */
	int known;   /* =1 contents known, =2 shuttle on this planet */
} PLANETS;

EXTERN struct foo {
		int snap,		// snapshot taken
		remaining_klingons,			// remaining klingons
	    remaining_commanders,			// remaining commanders
		remaining_bases,		// remaining bases
		killed_stars,			// destroyed stars
		killed_bases,			// destroyed bases
		killed_klingons,			// Klingons killed
		killed_commanders,			// commanders killed
		galaxy[9][9], 	// The Galaxy (subscript 0 not used)
		qx_commander[11],qy_commander[11],	// Commander quadrant coordinates
		qx_base[6],		// Base quadrant X
		qy_base[6],		// Base quadrant Y
		newstuf[9][9],	// Extended galaxy goodies
		qx_supercommander, qy_supercommander,		// Coordinate of Super Commander
		remaining_supercommanders,			// remaining super commanders
		killed_romulans,			// Romulans killed
		remaining_romulans,		// Romulans remaining
		killed_supercommanders,		// super commanders killed
		killed_planets;		// destroyed planets
	    PLANETS plnets[PLNETMAX+1];  // Planet information
#ifdef CAPTURE
        int captured_klingons, brigfree;
#endif
	double stardate,		// stardate
		remaining_resources,			// remaining resources
	    remaining_time;		// remaining time
} g_d, snapsht;			// Data that is snapshot

EXTERN char
		quad[11][11];	// contents of our quadrant

// Scalar variables that are needed for freezing the game
// are placed in a structure. #defines are used to access by their
// original names. Gee, I could have done this with the d structure,
// but I just didn't think of it back when I started.

EXTERN struct foo2 {
	int initial_klingons,
	initial_bases,
	initial_commanders,
	initial_stars,
	intorps,
	ship_condition,
	torps,
	ship_name,
	quadx,
	quady,
	sectx,
	secty,
	game_length,
	game_skill,
	currentq_base_sx,
	currentq_base_sy,
	currentq_num_klingons,
	currentq_num_commanders,
	num_casualties,
	game_num_help_calls,
	game_num_intergalactic_attempts,
	ididit,
	gamewon,
	alive,
	justin,
	alldone,
	is_shield_changing,
	qx_thing,
	qy_thing,
	currentq_planet_sx,
	currentq_planet_sy,
	is_inorbit,
	is_landed,
	currentq_planet_id,
	is_mining,
	inplan,
	currentq_num_enemies,
	currentq_has_supercommander,
	in_neutral_zone,
	currentq_num_romulans,
	is_aboard_shuttle,
	ientesc,
	has_suttlecraft,
	is_supercommander_attacking_base,
	currentq_is_supercommander_here,
#ifdef DEBUG
	idebug,
#endif
#ifdef CLOAKING
    iscloaked,
    iscloaking,
    num_cloak_violations,
    is_cloak_violation_reported,
#endif
#ifdef CAPTURE
    brigcapacity,
#endif
	iattak,
	have_crystals,
	is_tournament_game,
	thawed,
	batx,
	baty,
	currentq_has_tholian,
	currentq_tholian_sx,
	currentq_tholian_sy,
	has_seen_attack_report,
	probe_qx,
	probe_qy,
	probe_active_sectors_remaining,
	is_probe_armed,
	remaining_probes;

	double game_initial_resources,
	game_initial_time,
	ship_max_energy,
	ship_max_shield,
	game_initial_lifesupport,
	game_initial_stardate,
	ship_energy,
	ship_shield_strength,
	is_shield_up,
	warp_factor,
	warp_factor_squared,
	ship_life_support_reserves,
	dist,
	direc,
	Time,
	docfac,
	resting,
	game_damage_factor,
	ship_date_chart_damaged,
	crystal_prob_fail,
    probe_global_x,
	probe_global_y,
	probe_increment_gx,
	probe_increment_gy;
} g_a;

#define initial_klingons g_a.initial_klingons		// Initial number of klingons
#define initial_bases g_a.initial_bases			// Initial number of bases
#define initial_commanders g_a.initial_commanders			// Initian number of commanders
#define initial_stars g_a.initial_stars			// Initial stars
#define intorps g_a.intorps		// Initial/Max torpedoes
#define ship_condition g_a.ship_condition			// Condition (red, yellow, green docked)
#define torps g_a.torps			// number of torpedoes
#define ship_name g_a.ship_name				// ship type -- 'E' is Enterprise
#define quadx g_a.quadx			// where we are
#define quady g_a.quady			//
#define sectx g_a.sectx			// where we are
#define secty g_a.secty			//
#define game_length g_a.game_length			// length of game
#define game_skill g_a.game_skill			// skill level
#define currentq_base_sx g_a.currentq_base_sx			// position of base in current quad
#define currentq_base_sy g_a.currentq_base_sy			//
#define currentq_num_klingons g_a.currentq_num_klingons			// klingons here
#define currentq_num_commanders g_a.currentq_num_commanders		// commanders here
#define num_casualties g_a.num_casualties			// causalties
#define game_num_help_calls g_a.game_num_help_calls			// calls for help
#define game_num_intergalactic_attempts g_a.game_num_intergalactic_attempts			//
#define ididit g_a.ididit			// Action taken -- allows enemy to attack
#define gamewon g_a.gamewon		// Finished!
#define alive g_a.alive			// We are alive (not killed)
#define justin g_a.justin			// just entered quadrant
#define alldone g_a.alldone		// game is now finished
#define is_shield_changing g_a.is_shield_changing		// shield is changing (affects efficiency)
#define qx_thing g_a.qx_thing			// location of strange object in galaxy
#define qy_thing g_a.qy_thing			//
#define currentq_planet_sx g_a.currentq_planet_sx			// location of planet in quadrant
#define currentq_planet_sy g_a.currentq_planet_sy			//
#define is_inorbit g_a.is_inorbit		// orbiting
#define is_landed g_a.is_landed			// party on planet (1), on ship (-1)
#define currentq_planet_id g_a.currentq_planet_id			// planet # in quadrant
#define is_mining g_a.is_mining			// mining
#define inplan g_a.inplan			// initial planets
#define currentq_num_enemies g_a.currentq_num_enemies		// Number of enemies in quadrant
#define currentq_has_supercommander g_a.currentq_has_supercommander			// Super-commander in quandrant
#define in_neutral_zone g_a.in_neutral_zone			// Romulan Neutral Zone
#define currentq_num_romulans g_a.currentq_num_romulans			// Romulans in quadrant
#define is_aboard_shuttle g_a.is_aboard_shuttle			// Kirk in Galileo
#define ientesc g_a.ientesc		// Attempted escape from supercommander
#define has_suttlecraft g_a.has_suttlecraft		// =1 if craft on ship, -1 if removed from game
#define is_supercommander_attacking_base g_a.is_supercommander_attacking_base			// =1 if SuperCommander is attacking base
#define currentq_is_supercommander_here g_a.currentq_is_supercommander_here			// Super Commander is here
#ifdef DEBUG
#define idebug g_a.idebug			// Debug mode
#endif
#ifdef CLOAKING
#define iscloaked g_a.iscloaked  // Cloaking is enabled
#define iscloaking g_a.iscloaking // However if iscloaking is TRUE then in process of cloaking and can be attacked
#define num_cloak_violations g_a.num_cloak_violations		// Treaty violations
#define is_cloak_violation_reported g_a.is_cloak_violation_reported // Violation reported by Romulan in quadrant
#endif
#ifdef CAPTURE
#define captured_klingons g_d.captured_klingons   // number of captured Klingons                  
#define brigfree g_d.brigfree     // room in the brig
#define brigcapacity g_a.brigcapacity        // How many Klingons the brig will hold
#endif
#define iattak g_a.iattak			// attack recursion elimination (was cracks[4])
#define have_crystals g_a.have_crystals		// dilithium crystals aboard
#define is_tournament_game g_a.is_tournament_game			// Tournament number
#define thawed g_a.thawed			// Thawed game
#define batx g_a.batx				// Base coordinates being attacked
#define baty g_a.baty				//
#define currentq_has_tholian g_a.currentq_has_tholian			// Tholean is here 
#define currentq_tholian_sx g_a.currentq_tholian_sx				// coordinates of tholean
#define currentq_tholian_sy g_a.currentq_tholian_sy
#define has_seen_attack_report g_a.has_seen_attack_report		// Seen base attack report
#define game_initial_resources g_a.game_initial_resources		// initial resources
#define game_initial_time g_a.game_initial_time			// initial time
#define ship_max_energy g_a.ship_max_energy			// Initial/Max ship_energy
#define ship_max_shield g_a.ship_max_shield			// Initial/Max Shield
#define game_initial_lifesupport g_a.game_initial_lifesupport			// initial life support resources
#define game_initial_stardate g_a.game_initial_stardate			// Initial stardate
#define ship_energy g_a.ship_energy			// ship_energy level
#define ship_shield_strength g_a.ship_shield_strength			// Shield level
#define is_shield_up g_a.is_shield_up			// Shields are up
#define warp_factor g_a.warp_factor		// Warp speed
#define warp_factor_squared g_a.warp_factor_squared			// squared warp factor
#define ship_life_support_reserves g_a.ship_life_support_reserves		// life support reserves
#define dist g_a.dist				// movement distance
#define direc g_a.direc			// movement direction
#define Time g_a.Time				// time taken by current operation
#define docfac g_a.docfac			// repair factor when docking (constant?)
#define resting g_a.resting		// rest time
#define game_damage_factor g_a.game_damage_factor			// damage factor
#define ship_date_chart_damaged g_a.ship_date_chart_damaged		// time that star chart was damaged
#define crystal_prob_fail g_a.crystal_prob_fail		// probability that crystal will fail
#define probe_global_x g_a.probe_global_x			// location of probe
#define probe_global_y g_a.probe_global_y
#define probe_qx g_a.probe_qx		// current probe quadrant
#define probe_qy g_a.probe_qy	
#define probe_increment_gx g_a.probe_increment_gx		// Probe x,y increment
#define probe_increment_gy g_a.probe_increment_gy		
#define probe_active_sectors_remaining g_a.probe_active_sectors_remaining			// number of moves for probe
#define is_probe_armed g_a.is_probe_armed		// Probe is armed
#define remaining_probes g_a.remaining_probes		// number of probes available


EXTERN int
		kx[21],			// enemy sector locations
		ky[21],
		starch[9][9];	// star chart

EXTERN int fromcommandline; // Game start from command line options
EXTERN int coordfixed; // Fix those dumb coordinates. 

EXTERN char	passwd[10],		// Self Destruct password
		*device[ndevice+1];

EXTERN PLANETS nulplanet;	// zeroed planet structure

EXTERN double
		kpower[21],		// enemy energy levels
		kdist[21],		// enemy distances
		kavgd[21],		// average distances
		damage[ndevice+1],		// damage encountered
		future[NEVENTS+1];		// future events

EXTERN int iscore, iskill; // Common PLAQ
EXTERN double perdate;

typedef enum {FWON, FDEPLETE, FLIFESUP, FNRG, FBATTLE,
              FNEG3, FNOVA, FSNOVAED, FABANDN, FDILITHIUM,
			  FMATERIALIZE, FPHASER, FLOST, FMINING, FDPLANET,
			  FPNOVA, FSSC, FSTRACTOR, FDRAY, FTRIBBLE,
			  FHOLE
#ifdef CLOAKING
   , FCLOAK
#endif
} FINTYPE ;

/* Skill levels */
typedef enum {SNOVICE=1, SFAIR, SGOOD, SEXPERT, SEMERITUS} SKILLTYPE;

EXTERN double aaitem;
EXTERN char citem[24];


/* Define devices */
#define DSRSENS 1
#define DLRSENS 2
#define DPHASER 3
#define DPHOTON 4
#define DLIFSUP 5
#define DWARPEN 6
#define DIMPULS 7
#define DSHIELD 8
#define DRADIO  9
#define DSHUTTL 10
#define DCOMPTR 11
#define DTRANSP 12
#define DSHCTRL 13
#define DDRAY   14  // Added deathray
#define DDSP    15  // Added deep space probe
#define DCLOAK  16  // Added cloaking device

/* Define future events */
#define FSPY	0	// Spy event happens always (no future[] entry)
					// can cause SC to tractor beam Enterprise
#define FSNOVA  1   // Supernova
#define FTBEAM  2   // Commander tractor beams Enterprise
#define FSNAP   3   // Snapshot for time warp
#define FBATTAK 4   // Commander attacks base
#define FCDBAS  5   // Commander destroys base
#define FSCMOVE 6   // Supercommander moves (might attack base)
#define FSCDBAS 7   // Supercommander destroys base
#define FDSPROB 8   // Move deep space probe

#ifdef INCLUDED
PLANETS nulplanet = {0};
char *device[ndevice+1] = {
	"",
	"S. R. Sensors",
	"L. R. Sensors",
	"Phasers",
	"Photon Tubes",
	"Life Support",
	"Warp Engines",
	"Impulse Engines",
	"Shields",
	"Subspace Radio",
	"Shuttle Craft",
	"Computer",
	"Transporter",
	"Shield Control",
	"Death Ray",
	"D. S. Probe"
#ifdef CLOAKING
	,"Cloaking Device"
#endif
};									
#endif

#define ALGERON (2311) /* stardate of the Treaty of Algeron */

#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif

#define IH_ROMULAN 'R'
#define IH_KLINGON 'K'
#define IH_COMMANDER 'C'
#define IH_SUPER_COMMANDER 'S'
#define IH_STAR '*'
#define IH_PLANET 'P'
#define IH_BASE 'B'
#define IH_BLACK_HOLE '@'
#define IHDOT '.'
#define IHQUEST '?'
#define IH_ENTERPRISE 'E'
#define IH_FAERIE_QUEEN 'F'
#define IH_THOLIAN 'T'
#define IH_THOLIAN_WEB '#'
#define IHGREEN 'G'
#define IHYELLOW 'Y'
#define IHRED 'R'
#define IHDOCKED 'D'


/* Function prototypes */
void prelim(void);
void attack(int);
int choose(void);
void setup(void);
void score(int);
void atover(int);
void srscan(int);
void lrscan(void);
void phasers(void);
void photon(void);
void warp(int);
void sheild(int);
void dock(void);
void dreprt(void);
void chart(int);
void impuls(void);
void waiting(void);
void setwrp(void);
void events(void);
void report(int);
void eta(void);
void help(void);
void abandn(void);
void finish(FINTYPE);
void dstrct(void);
void kaboom(void);
void freeze(int);
void thaw(void);
void plaque(void);
int scan(void);
#define IHEOL (0)
#define IHALPHA (1)
#define IHREAL (2)
void chew(void);
void chew2(void);
void skip(int);
void prout(char *s);
void proutn(char *s);
void stars(void);
void newqad(int);
int ja(void);
void cramen(int);
void crmshp(void);
void cramlc(int, int, int);
double expran(double);
double Rand(void);
void iran8(int *, int *);
void iran10(int *, int *);
double square(double);
void dropin(int, int*, int*);
void newcnd(void);
void sortkl(void);
void lmove(void);
void ram(int, int, int, int);
void crmena(int, int, int, int, int);
void deadkl(int, int, int, int, int);
void timwrp(void);
void movcom(void);
void torpedo(double, double, int, int, double *);
void cramf(double, int, int);
void crami(int, int);
void huh(void);
void pause(int);
void nova(int, int);
void snova(int, int);
void scom(int *);
void hittem(double *);
void prouts(char *);
int isit(char *);
void preport(void);
void orbit(void);
void sensor(void);
void beam(void);
void mine(void);
void usecrystals(void);
void shuttle(void);
void deathray(void);
void debugme(void);
void attakreport(void);
void movetho(void);
void probe(void);

#ifndef WINDOWS
int min(int, int);
int max(int, int);
#endif
void randomize(void);
int getch(void);

#ifdef CLOAKING
void cloak(void);
#endif
#ifdef CAPTURE
void capture(void);
#endif

#ifdef CLOAKING
#define REPORTS ((ship_condition==IHDOCKED || damage[DRADIO]<=0.0) && !iscloaked)
#else
#define REPORTS (ship_condition==IHDOCKED || damage[DRADIO]<=0.0)
#endif
