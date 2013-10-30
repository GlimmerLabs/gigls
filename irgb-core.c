/**
 * irgb-core.c
 *   The core integer RGB functions both in C and in Racket.
 *   (We don't want to send those over D-Bus because we often want
 *   them to be very very fast.)
 *
 * Copyright (c) 2012-13 Samuel A. Rebelsky.  All rights reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the Lesser GNU General Public License as published 
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


// +---------+---------------------------------------------------------
// | Headers |
// +---------+

#include <escheme.h>
#include <scheme.h>


// +-----------------------+-------------------------------------------
// | Integer RGB Utilities |
// +-----------------------+

#include "irgb-funs.c"


// +---------+---------------------------------------------------------
// | Helpers |
// +---------+


// +------------+------------------------------------------------------
// | Extensions |
// +------------+

static Scheme_Object *
irgb_alpha_scheme (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgb-alpha", "integer-encoded rgb color", 0, 1, argv);
  return scheme_make_integer (irgb_alpha (SCHEME_INT_VAL (argv[0])));
} // irgb_alpha_scheme

Scheme_Object *
irgb_blue_scheme (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgb-blue", "integer-encoded rgb color", 0, 1, argv);
  return scheme_make_integer (irgb_blue (SCHEME_INT_VAL (argv[0])));
} // irgb_blue_scheme

Scheme_Object *
irgb_green_scheme (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgb-green", "integer-encoded rgb color", 0, 1, argv);
  return scheme_make_integer (irgb_green (SCHEME_INT_VAL (argv[0])));
} // irgb_green_scheme

Scheme_Object *
irgb_new_scheme (int argc, Scheme_Object **argv)
{
  int red = 0;
  int green = 0;
  int blue = 0;

  if (SCHEME_INTP (argv[0])) 
    red = SCHEME_INT_VAL (argv[0]);
  else if (SCHEME_DBLP (argv[0]))
    red = (int) SCHEME_DBL_VAL (argv[0]);
  else if (SCHEME_RATIONALP (argv[0]))
    red = (int) scheme_rational_to_double (argv[0]);
  else
    scheme_wrong_type ("irgb-new", "integer", 0, 3, argv);
  
  if (SCHEME_INTP (argv[1]))
    green = SCHEME_INT_VAL (argv[1]);
  else if (SCHEME_DBLP (argv[1])) 
    green = (int) SCHEME_DBL_VAL (argv[1]);
  else if (SCHEME_RATIONALP (argv[1]))
    green = (int) scheme_rational_to_double (argv[1]);
  else
    scheme_wrong_type ("irgb-new", "integer", 1, 3, argv);

  if (SCHEME_INTP (argv[2]))
    blue = SCHEME_INT_VAL (argv[2]);
  else if (SCHEME_DBLP (argv[2])) 
    blue = (int) SCHEME_DBL_VAL (argv[2]);
  else if (SCHEME_RATIONALP (argv[2]))
    blue = (int) scheme_rational_to_double (argv[2]);
  else
    scheme_wrong_type ("irgb-new", "integer", 2, 3, argv);

  return scheme_make_integer (irgb_new (red, green, blue));
} // irgb_new_scheme

Scheme_Object *
irgb_red_scheme (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgb-red", "integer-encoded rgb color", 0, 1, argv);
  return scheme_make_integer (irgb_red (SCHEME_INT_VAL (argv[0])));
} // irgb_red_scheme

Scheme_Object *
irgba_new_scheme (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgba-new", "integer", 0, 4, argv);
  if (! SCHEME_INTP (argv[1]))
    scheme_wrong_type ("irgba-new", "integer", 1, 4, argv);
  if (! SCHEME_INTP (argv[2]))
    scheme_wrong_type ("irgba-new", "integer", 2, 4, argv);
  if (! SCHEME_INTP (argv[2]))
    scheme_wrong_type ("irgba-new", "integer", 3, 4, argv);

  return scheme_make_integer (irgba_new (SCHEME_INT_VAL (argv[0]),
                                         SCHEME_INT_VAL (argv[1]),
                                         SCHEME_INT_VAL (argv[2]),
                                         SCHEME_INT_VAL (argv[3])));
} // irgba_new_scheme


// +---------------------+---------------------------------------------
// | Standard Procedures |
// +---------------------+

Scheme_Object *
scheme_initialize (Scheme_Env *env)
{
  return scheme_reload (env);
} // scheme_initialize

Scheme_Object *
scheme_module_name (void)
{
  return scheme_intern_symbol ("irgb-core");
} // scheme_module_name

Scheme_Object *
scheme_reload (Scheme_Env *env)
{
  Scheme_Env *menv = NULL;      // The module's environment.
  Scheme_Object *proc = NULL;      // A procedure that we're adding.

  // Annotations for the garbage collector
  MZ_GC_DECL_REG (2);
  MZ_GC_VAR_IN_REG (0, env);
  MZ_GC_VAR_IN_REG (1, menv);
  MZ_GC_REG ();

  // Build the module environment
  menv = scheme_primitive_module (scheme_intern_symbol ("irgb"), env);

  // Add the procedures
  proc = scheme_make_prim_w_arity (irgb_alpha_scheme, "irgb-alpha", 1, 1);
  scheme_add_global ("irgb-alpha", proc, menv);
  proc = scheme_make_prim_w_arity (irgb_blue_scheme, "irgb-blue", 1, 1);
  scheme_add_global ("irgb-blue", proc, menv);
  proc = scheme_make_prim_w_arity (irgb_green_scheme, "irgb-green", 1, 1);
  scheme_add_global ("irgb-green", proc, menv);
  proc = scheme_make_prim_w_arity (irgb_new_scheme, "irgb-new", 3, 3);
  scheme_add_global ("irgb-new", proc, menv);
  proc = scheme_make_prim_w_arity (irgb_red_scheme, "irgb-red", 1, 1);
  scheme_add_global ("irgb-red", proc, menv);
  proc = scheme_make_prim_w_arity (irgba_new_scheme, "irgba-new", 4, 4);
  scheme_add_global ("irgba-new", proc, menv);

  // Clean up
  scheme_finish_primitive_module (menv);
  MZ_GC_UNREG ();

  // And we're done
  return scheme_void;
} // scheme_reload

