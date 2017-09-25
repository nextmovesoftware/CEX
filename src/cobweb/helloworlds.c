/*****************************************************************************
*  helloworlds.c -- a test program
*
*----------------------------------------------------------------------------
*  This sets up a solar system as $Star<Sol>data<>$Planet<>data<>Moon<data>|
*
*  Datatypes are set for identifiers (objects stars, planets, moons)
*  Property names are set for all data.
*
*  root .............. Star (Sol)
*    properties ...... Mass, Gravity, Diameter
*
*  children .......... Planet (Mercury, Venus, ..., Pluto)
*    properties ...... Mass, Orbit
*
*  grandchildren ..... Moon (Luna, Io, Titan, ...)
*    properties ...... Position, Diameter
*
*  Objects have datatypes iff they are identifiers.
*  Properties inherit datatypes via property names.
*
*----------------------------------------------------------------------------
*  Contributing author and institution: Dave Weininger, Daylight CIS, Inc.
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*****************************************************************************/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "cx_cobweb.h"

/*============================================================================
 *  makestar
 */

static cx_Object makestar(cx_Object parent, cx_String name,
		          cx_Real   mass,   cx_Real   grav, cx_Integer dia,
			  cx_Object table)
{
   static cx_Object stardt = NULL;
   cx_Object        star;

   if (NULL == stardt) {
      stardt = cx_create_datatype(table,
	"STAR", "star",     "Star",       "1",  "STRING", "Star"             );
      cx_create_datatype(table,
	"MASS", "mass",     "Mass, EU",    "1", "REAL",   "Mass, Earth units");
      cx_create_datatype(table,
	"GRAV", "gravity",  "Gravity, g",  "1", "REAL",   "Gravity, g"       );
      cx_create_datatype(table,
       "DIA",   "diameter", "Diameter, mi","1", "STRING", "Diameter, mi"     );
   }

   star = cx_create_string(parent, name);
   cx_set_datatype(star, stardt);

   cx_set_rprop(star, "mass",     mass);
   cx_set_rprop(star, "gravity",  grav);
   cx_set_iprop(star, "diameter", dia );

   return star;
}

/*============================================================================
 *  addplaenet
 */

static cx_Object addplanet(cx_Object star, cx_String name,
		           cx_Real   orb,  cx_Real   mass, cx_Real grav,
			   cx_Object table)
{
   static cx_Object plandt = NULL;
   cx_Object        planet;

   if (NULL == plandt) {
      plandt = cx_create_datatype(table,
        "PLAN", "planet",  "Planet",     "1", "STRING", "Planet"            );
      cx_create_datatype(table,
	"ORB",  "orbit",   "Orbit, AU",  "1", "REAL",   "Orbit, Earth units");
      cx_create_datatype(table,
	"MASS", "mass",    "Mass, EU",   "1", "REAL",   "Mass, Earth units" );
      cx_create_datatype(table,
	"GRAV", "gravity", "Gravity, g", "1", "REAL",   "Gravity, g"        );
   }

   planet = cx_create_string(star, name);
   cx_set_datatype(planet, plandt);

   cx_set_rprop(planet, "orbit",   orb );
   cx_set_rprop(planet, "mass",    mass);
   cx_set_rprop(planet, "gravity", grav);

   return planet;
}

/*============================================================================
 *  addmoon
 */

static cx_Object addmoon(cx_Object planet, cx_String  name,
		         cx_String pos,    cx_Integer dia,
			 cx_Object table)
{
   static cx_Object moondt = NULL;
   cx_Object        moon;

   if (NULL == moondt) {
      moondt = cx_create_datatype(table,
       "MOON",  "moon",     "Moon",         "1", "STRING", "Moon"            );
      cx_create_datatype(table,
       "POS",   "position", "Position",     "1", "STRING", "Ordinal position");
      cx_create_datatype(table,
       "DIA",   "diameter", "Diameter, mi", "1", "STRING", "Diameter, mi");
   }
   moon = cx_create_string(planet, name);
   cx_set_datatype(moon, moondt);
   cx_set_sprop(moon, "position", pos);
   cx_set_iprop(moon, "diameter", dia);

   return moon;
}

/*============================================================================
 *  main() standard test program
 */

int main(int argc, char **argv)
{
   int       verb = CX_SPEW_DEFAULT;
   cx_Object table, sol, earth;
   cx_Object mars, jupiter, saturn;

   /*** Interpret argument. ***/

   if (2 == argc && '-' == *argv[1]) {
      if (isdigit(argv[1][1])) verb = atoi(argv[1] + 1);
      else { fprintf(stderr, "usage: %s [-#]\n", *argv); exit(1); }
   }

   /*** Initialize. ***/

   /* cx_init(); */

   /*** Create datatype table. ***/

   table = cx_create_datatypetable(NULL, "Astronomical datatype table");

   /*** Create "root" star object. ***/

   sol = makestar(NULL, "Sol", 333434.0, 28.0, 864000, table);

   /*** Create planets. ***/

              addplanet(sol, "Mercury",  0.39,   0.056, 0.93, table);
              addplanet(sol, "Venus",    0.72,   0.810, 0.90, table);
   earth    = addplanet(sol, "Earth",    1.00,   1.000, 1.00, table);
   mars     = addplanet(sol, "Mars",     1.52,   0.108, 0.38, table);
   saturn   = addplanet(sol, "Saturn",   9.54,  95.200, 1.14, table);
   jupiter  = addplanet(sol, "Jupiter",  5.20, 318.000, 2.66, table);
              addplanet(sol, "Uranus",  19.20,  14.600, 1.07, table);
              addplanet(sol, "Neptune", 30.10,  17.300, 1.40, table);
              addplanet(sol, "Pluto",   39.10,   1.100, 0.30, table);

   /*** Create moons. ***/

   addmoon(earth,   "Luna",      "I",    2160, table);
   addmoon(mars,    "Phobos",    "I",      14, table);
   addmoon(mars,    "Deimos",    "II",      6, table);
   addmoon(saturn,  "Mimas",     "I",     300, table);
   addmoon(saturn,  "Enceladus", "II",    400, table);
   addmoon(saturn,  "Tethys",    "III",   600, table);
   addmoon(saturn,  "Dione",     "IV",    600, table);
   addmoon(saturn,  "Rhea",      "V",     810, table);
   addmoon(saturn,  "Titan",     "VI",   2980, table);
   addmoon(saturn,  "Hyperion",  "VII",   100, table);
   addmoon(saturn,  "Iapetus",   "VIII",  500, table);
   addmoon(saturn,  "Pheobe",    "IX",    100, table);
   addmoon(saturn,  "Janus",     "X",     250, table);
   addmoon(jupiter, "Io",        "I",    2020, table);
   addmoon(jupiter, "Europa",    "II",   1790, table);
   addmoon(jupiter, "Ganymede",  "III",  3120, table);
   addmoon(jupiter, "Callisto",  "IV",   2970, table);

   /*** Create a stream. ***/

   cx_spewob(stdout, sol,   verb);
   cx_spewob(stdout, table, verb);
   cx_error_spew(stderr, NULL);

   /*** Clean up. ***/

   cx_destroy(sol);
   cx_destroy(table);
   return 0;
}
