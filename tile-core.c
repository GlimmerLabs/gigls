/**
 * tile-core.c
 *   A Racket extension that provides the core of the various functions
 *   that operation on image tiles.
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


// +-------+-----------------------------------------------------------
// | Notes |
// +-------+

/*

  It looks like it's difficult enough to get the current environment
  that I'll want to write wrapper functions that pass in important things
  (such as loudbus_call).

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

/**
 * Extract information from a tile.
 */
static int
unpack_tile (Scheme_Object *tile, 
             int *size, unsigned char **bytes, int *bpp, int *rowstride,
	     int *x, int *y, int *w, int *h)
{
  Scheme_Object *tmp = tile;

  // Step through the list, extracting values
  *size = SCHEME_INT_VAL (SCHEME_CAR (tmp));
  tmp = SCHEME_CDR (tmp);
  *bytes = (unsigned char *) SCHEME_BYTE_STR_VAL (SCHEME_CAR (tmp));
  tmp = SCHEME_CDR (tmp);
  *bpp = SCHEME_INT_VAL (SCHEME_CAR (tmp));
  tmp = SCHEME_CDR (tmp);
  *rowstride = SCHEME_INT_VAL (SCHEME_CAR (tmp));
  tmp = SCHEME_CDR (tmp);
  *x = SCHEME_INT_VAL (SCHEME_CAR (tmp));
  tmp = SCHEME_CDR (tmp);
  *y = SCHEME_INT_VAL (SCHEME_CAR (tmp));
  tmp = SCHEME_CDR (tmp);
  *w = SCHEME_INT_VAL (SCHEME_CAR (tmp));
  tmp = SCHEME_CDR (tmp);
  *h = SCHEME_INT_VAL (SCHEME_CAR (tmp));

  return 1;
} // unpack_tile


// +------------+------------------------------------------------------
// | Extensions |
// +------------+

/**
 * Fill an image with a single color.  Intended primarily as a test
 * of tiles.
 * 
 * Parameters:
 *   1: loudbus-call
 *   2: gimp
 *   3: image-id
 *   4: drawable-id
 *   5: color
 */
Scheme_Object *
image_fill_core (int argc, Scheme_Object **argv)
{
  Scheme_Object *params[16];	// Handy array of parameters
  Scheme_Object *loudbus_call;	// The call function
  Scheme_Object *stream = NULL;	// The tile stream
  Scheme_Object *tile = NULL;	// Info on the current tile
  int more_tiles = 1;		// Are there more tiles?

  int tsize;
  unsigned char *tdata;
  int tbpp;
  int trowstride;
  int tx;
  int ty;
  int tw;
  int th;

  // Check parameters
  // STUB

  // Grab the call function
  loudbus_call = argv[0];

  // Set up the default parameters
  params[0] = argv[1];	// gimp

  // Get a stream
  params[1] = scheme_make_symbol ("tile_stream_new");
  params[2] = argv[2];	// image
  params[3] = argv[3];	// drawable
  params[4] = NULL;	// May not be necessary
  stream = SCHEME_CAR (scheme_apply (loudbus_call, 4, params));
  
  // Check the validity of the stream
  // STUB

  // Get all the tiles 
  do {
    // Get a tile
    params[1] = scheme_make_symbol ("tile_stream_get");
    params[2] = stream;
    params[3] = NULL;
    tile = scheme_apply (loudbus_call, 3, params);
    unpack_tile (tile, &tsize, &tdata, &tbpp, &trowstride, &tx, &ty, &tw, &th);
    // Process the tile
    fprintf (stderr, "size:%d, bpp:%d, rowstride:%d, x:%d, y:%d, w:%d, h:%d\n",
             tsize, tbpp, trowstride, tx, ty, tw, th);
    // Advance to the next tile
    params[1] = scheme_make_symbol ("tile_stream_advance");
    params[2] = stream;
    params[3] = NULL;
    more_tiles = SCHEME_INT_VAL (SCHEME_CAR (scheme_apply (loudbus_call, 3, params)));
  } while (more_tiles);

  // Close the stream
  params[1] = scheme_make_symbol ("tile_stream_close");
  params[2] = stream;
  params[3] = NULL;
  tile = scheme_apply (loudbus_call, 3, params);

  // And we're done
  return argv[2];
} // image_fill_core

/**
 * Build a new image using a function from positions to colors.
 *
 * Parameters:
 *   1: loudbus-call
 *   2: function
 *   3: width
 *   4: height
 */
Scheme_Object *
image_compute_core (int argc, Scheme_Object **argv)
{
  return scheme_void;
} // image_cmpute_core

/**
 * Transform an image using a function from positions to colors.
 *
 * Parameter:
 *   1: loudbus-call
 *   2: image
 *   3: drawable
 *   4: function
 */
Scheme_Object *
drawable_transform_core (int argc, Scheme_Object **argv)
{
  return scheme_void;
} // image_transform_core


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
  return scheme_intern_symbol ("tile-core");
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
  menv = scheme_primitive_module (scheme_intern_symbol ("tile-core"), env);

  // Add the procedures
  proc = scheme_make_prim_w_arity (image_fill_core, "image-fill-core", 5, 5);
  scheme_add_global ("image-fill-core", proc, menv);

  // Clean up
  scheme_finish_primitive_module (menv);
  MZ_GC_UNREG ();

  // And we're done
  return scheme_void;
} // scheme_reload


