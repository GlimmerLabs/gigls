/**
 * irgb.c
 *   A simple Racket extension that provides the irgb functions.
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


// +-----------------+-------------------------------------------------
// | Local Utilities |
// +-----------------+

/**
 * Shorten an integer to one byte.
 */
static int
byte (int x)
{
  if (x < 0) return 0;
  if (x > 255) return 255;
  return x;
} // byte


// +------------+------------------------------------------------------
// | Extensions |
// +------------+

Scheme_Object *
irgb_alpha (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgb-alpha", "integer", 0, 1, argv);
  int color = SCHEME_INT_VAL (argv[0]);
  return scheme_make_integer ((color >> 24) & 255);
} // irgb_alpha

Scheme_Object *
irgb_blue (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgb-blue", "integer", 0, 1, argv);
  int color = SCHEME_INT_VAL (argv[0]);
  return scheme_make_integer (color & 255);
} // irgb_blue

Scheme_Object *
irgb_green (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgb-green", "integer", 0, 1, argv);
  int color = SCHEME_INT_VAL (argv[0]);
  return scheme_make_integer ((color >> 8) & 255);
} // irgb_green

Scheme_Object *
irgb_new (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgb-new", "integer", 0, 3, argv);
  if (! SCHEME_INTP (argv[1]))
    scheme_wrong_type ("irgb-new", "integer", 1, 3, argv);
  if (! SCHEME_INTP (argv[2]))
    scheme_wrong_type ("irgb-new", "integer", 2, 3, argv);

  int r = byte (SCHEME_INT_VAL (argv[0]));
  int g = byte (SCHEME_INT_VAL (argv[1]));
  int b = byte (SCHEME_INT_VAL (argv[2]));

  return scheme_make_integer ((r << 16) | (g << 8) | b);
} // irgb_new

Scheme_Object *
irgb_red (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgb-red", "integer", 0, 1, argv);
  int color = SCHEME_INT_VAL (argv[0]);
  return scheme_make_integer ((color >> 16) & 255);
} // irgb_red

Scheme_Object *
irgba_new (int argc, Scheme_Object **argv)
{
  if (! SCHEME_INTP (argv[0]))
    scheme_wrong_type ("irgba-new", "integer", 0, 4, argv);
  if (! SCHEME_INTP (argv[1]))
    scheme_wrong_type ("irgba-new", "integer", 1, 4, argv);
  if (! SCHEME_INTP (argv[2]))
    scheme_wrong_type ("irgba-new", "integer", 2, 4, argv);
  if (! SCHEME_INTP (argv[2]))
    scheme_wrong_type ("irgba-new", "integer", 3, 4, argv);

  int r = byte (SCHEME_INT_VAL (argv[0]));
  int g = byte (SCHEME_INT_VAL (argv[1]));
  int b = byte (SCHEME_INT_VAL (argv[2]));
  int a = byte (SCHEME_INT_VAL (argv[3]));

  return scheme_make_integer ((a << 24) | (r << 16) | (g << 8) | b);
} // irgba_new


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
  return scheme_intern_symbol ("irgb");
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
  proc = scheme_make_prim_w_arity (irgb_alpha, "irgb-alpha", 1, 1);
  scheme_add_global ("irgb-alpha", proc, menv);
  proc = scheme_make_prim_w_arity (irgb_blue, "irgb-blue", 1, 1);
  scheme_add_global ("irgb-blue", proc, menv);
  proc = scheme_make_prim_w_arity (irgb_green, "irgb-green", 1, 1);
  scheme_add_global ("irgb-green", proc, menv);
  proc = scheme_make_prim_w_arity (irgb_new, "irgb-new", 3, 3);
  scheme_add_global ("irgb-new", proc, menv);
  proc = scheme_make_prim_w_arity (irgb_red, "irgb-red", 1, 1);
  scheme_add_global ("irgb-red", proc, menv);
  proc = scheme_make_prim_w_arity (irgba_new, "irgba-new", 4, 4);
  scheme_add_global ("irgba-new", proc, menv);

  // Clean up
  scheme_finish_primitive_module (menv);
  MZ_GC_UNREG ();

  // And we're done
  return scheme_void;
} // scheme_reload


