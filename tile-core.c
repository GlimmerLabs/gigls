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


// +---------+---------------------------------------------------------
// | Globals |
// +---------+

/**
 * The loudbus_call function.  
 */
static Scheme_Object *loudbus_call = NULL;

/**
 * The gimpplus object.
 */
static Scheme_Object *gimpplus = NULL;


// +-----------------+-------------------------------------------------
// | Local Utilities |
// +-----------------+

/**
 * Convert a Scheme object to an iRGB.  Does appropriate rounding
 * (I hope).  Returns a negative number for an invalid Scheme
 * object.
 */
static int
scheme_object_to_irgb (Scheme_Object *obj)
{
  if (SCHEME_INTP (obj)) 
    return (int) SCHEME_INT_VAL (obj);
  else if (SCHEME_DBLP (obj)) 
    return (int) SCHEME_DBL_VAL (obj);
  else if (SCHEME_FLTP (obj))
    return (int) SCHEME_FLT_VAL (obj);
  else if (SCHEME_RATIONALP (obj))
    return (int)  scheme_rational_to_double (obj);
  else
    return -1;
} // scheme_object_to_irgb

/**
 * Add a procedure
 */
static void
register_scheme_function (Scheme_Prim *prim, char *name,
                          int minarity, int maxarity,
                          Scheme_Env *menv)
{
  Scheme_Object *proc = 
    scheme_make_prim_w_arity (prim, name, minarity, maxarity);
  scheme_add_global (name, proc, menv);
} // register_scheme_function

/**
 * Extract information from a tile.
 */
static int
unpack_tile (Scheme_Object *tile, 
             int *size, unsigned char **bytes, int *bpp, int *rowstride,
	     int *x, int *y, int *w, int *h)
{
  Scheme_Object *tmp = tile;
  char *oldbytes;
  char *newbytes;

  // Step through the list, extracting values
  *size = SCHEME_INT_VAL (SCHEME_CAR (tmp));
  tmp = SCHEME_CDR (tmp);

  // We're going to copy the bytes to save them from the evil Scheme
  // garbage collector.  (Or maybe just because I'm not competent enough
  // to tell the garbage collector that they are not garbage.)
  oldbytes = (unsigned char *) SCHEME_BYTE_STR_VAL (SCHEME_CAR (tmp));
  newbytes = malloc(*size);
  bcopy(oldbytes, newbytes, *size);
  *bytes = newbytes;

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


// +-----------------------+-------------------------------------------
// | Integer RGB Functions |
// +-----------------------+

#include "irgb-funs.c"


// +----------------+--------------------------------------------------
// | Tile Functions |
// +----------------+

int
tile_stream_advance (Scheme_Object *stream)
{
  static Scheme_Object *params[4];
  params[0] = gimpplus;
  params[1] = scheme_make_symbol ("tile_stream_advance");
  params[2] = stream;
  params[3] = NULL;
  Scheme_Object *result = scheme_apply (loudbus_call, 3, params);
  return SCHEME_INT_VAL (SCHEME_CAR (result));
} // tile_stream_advance

int
tile_stream_close (Scheme_Object *stream)
{
  static Scheme_Object *params[4];
  params[0] = gimpplus;
  params[1] = scheme_make_symbol ("tile_stream_close");
  params[2] = stream;
  params[3] = NULL;
  return SCHEME_INT_VAL (SCHEME_CAR (scheme_apply (loudbus_call, 3, params)));
} // tile_stream_close

Scheme_Object *
tile_stream_get (Scheme_Object *stream)
{
  static Scheme_Object *params[4];
  params[0] = gimpplus;
  params[1] = scheme_make_symbol ("tile_stream_get");
  params[2] = stream;
  params[3] = NULL;
  return scheme_apply (loudbus_call, 3, params);
} // tile_stream_get

Scheme_Object *
tile_stream_new (Scheme_Object *image, Scheme_Object *drawable)
{
  static Scheme_Object *params[5];
  params[0] = gimpplus;
  params[1] = scheme_make_symbol ("tile_stream_new");
  params[2] = image;
  params[3] = drawable;
  params[4] = NULL;	// May not be necessary
  return SCHEME_CAR (scheme_apply (loudbus_call, 4, params));
} // tile_stream_new

int
tile_update (Scheme_Object *stream, int size, unsigned char *data)
{
  static Scheme_Object *params[6];
  params[0] = gimpplus;
  params[1] = scheme_make_symbol ("tile_update");
  params[2] = stream;
  params[3] = scheme_make_integer (size);
  params[4] = scheme_make_sized_byte_string ((char *) data, size, 1);
  params[5] = NULL;
  return SCHEME_INT_VAL (SCHEME_CAR (scheme_apply (loudbus_call, 5, params)));
} // tile_update


// +------------+------------------------------------------------------
// | Extensions |
// +------------+

/**
 * Initialize.
 */
Scheme_Object *
tile_core_init (int argc, Scheme_Object **argv)
{
  loudbus_call = argv[0];
  gimpplus = argv[1];
  scheme_register_static (&loudbus_call, sizeof (Scheme_Object *));
  scheme_register_static (&gimpplus, sizeof (Scheme_Object *));
  return scheme_void;
} // tile_core_init

/**
 * Fill a drawable with a single color.  Intended primarily as a test
 * of tiles.
 * 
 * Parameters:
 *   0: image-id
 *   1: drawable-id
 *   2: color
 */
Scheme_Object *
drawable_fill_core (int argc, Scheme_Object **argv)
{
  Scheme_Object *stream = NULL;	// The tile stream
  Scheme_Object *tile = NULL;	// Info on the current tile
  int color;                    // The color as an iRGB
  unsigned char r, g, b;                  // The color components

  int tsize;
  unsigned char *tdata;
  int tbpp;
  int trowstride;
  int tx;
  int ty;
  int tw;
  int th;

  // Sanity check
  if (! SCHEME_INTP (argv[0]))
    {
      scheme_wrong_type ("drawable-fill!", "image", 0, argc, argv);
      return scheme_void;
    }
  if (! SCHEME_INTP (argv[1]))
    {
      scheme_wrong_type ("drawable-fill!", "drawable", 0, argc, argv);
      return scheme_void;
    }
  if (! SCHEME_INTP (argv[2]))
    {
      scheme_wrong_type ("drawable-fill!", "iRGB", 0, argc, argv);
      return scheme_void;
    }

  // Set up for garbage collection
  MZ_GC_DECL_REG (1);
  MZ_GC_VAR_IN_REG (0, stream);
  MZ_GC_REG ();

  // Get the color
  color = SCHEME_INT_VAL (argv[2]);
  r = (unsigned char) irgb_red (color);
  g = (unsigned char) irgb_green (color);
  b = (unsigned char) irgb_blue (color);

  // Get a stream
  stream = tile_stream_new (argv[0], argv[1]);

  // Check the validity of the stream
  // STUB

  // Get all the tiles 
  do {
    // Get the data from the current tile 
    tile = tile_stream_get (stream);
    unpack_tile (tile, &tsize, &tdata, &tbpp, &trowstride, &tx, &ty, &tw, &th);

    // Process the tile
    unsigned char *rowdata = tdata;
    // Do each row
    int row;
    for (row = 0; row < th; row++)
      {
        // Grab a pointers to the start of the data
        unsigned char *t = rowdata;

        // Do each column in the row
        int col;
        for (col = 0; col < tw; col++)
          {
            t[0] = r;
            t[1] = g;
            t[2] = b;

            // Advance to the next pixel
            t += tbpp;
          } // for each column

        // Advance to the next row
        rowdata += trowstride;
      } // for each row

    // Push the modified tile back to the server
    tile_update (stream, tsize, tdata);
    // We've allocated the data, so free it!
    free (tdata);
  } while (tile_stream_advance (stream));

  // Close the stream
  tile_stream_close (stream);

  // Clean up
  MZ_GC_UNREG ();

  // And we're done
  return argv[0];
} // drawable_fill_core

/**
 * Recompute the pixels in a drawable.  (Inner code for image-compute! and
 * other functions.)
 * 
 * Parameters:
 *   0: image-id
 *   1: drawable-id
 *   2: lambda (x,y) -> color
 */
Scheme_Object *
drawable_recompute_core (int argc, Scheme_Object **argv)
{
  Scheme_Object *stream = NULL;	// The tile stream
  Scheme_Object *tile = NULL;	// Info on the current tile
  Scheme_Object *fun  = NULL;   // The color function
  Scheme_Object *pos[2] = { NULL, NULL };
                                // The current position
  Scheme_Object *color_object;  // The color as a Scheme value
  int color;                    // The color as an iRGB
  unsigned char r, g, b;        // The color components

  int tsize;
  unsigned char *tdata;
  int tbpp;
  int trowstride;
  int tx;
  int ty;
  int tw;
  int th;

  // Set up for garbage collection
  MZ_GC_DECL_REG (5);
  MZ_GC_VAR_IN_REG (0, stream);
  MZ_GC_VAR_IN_REG (1, fun);
  MZ_GC_VAR_IN_REG (2, pos[0]);
  MZ_GC_VAR_IN_REG (3, pos[1]);
  MZ_GC_VAR_IN_REG (4, tile);
  MZ_GC_REG ();

  // Get the function
  fun = argv[2];

  // Get a stream
  stream = tile_stream_new (argv[0], argv[1]);
  int streamid = SCHEME_INT_VAL(stream);

  // Check the validity of the stream
  // STUB

  // Get all the tiles 
  do {
    // Get the data from the current tile 
    tile = tile_stream_get (stream);
    unpack_tile (tile, &tsize, &tdata, &tbpp, &trowstride, &tx, &ty, &tw, &th);

    // Process the tile
    unsigned char *rowdata = tdata;
    // Do each row
    int row;

    for (row = 0; row < th; row++)
      {
        // Grab a pointer to the start of the data
        unsigned char *t = rowdata;
        // Set up half of the position
        pos[1] = scheme_make_integer (row + ty);

        // Do each column in the row
        int col;
        for (col = 0; col < tw; col++)
          {
            pos[0] = scheme_make_integer (col + tx);
            color_object = scheme_apply (fun, 2, pos);
            color = scheme_object_to_irgb (color_object);
            if (color < 0) 
              {
                scheme_signal_error ("At position (%d,%d), a color function "
                                     "returned %V, instead of an "
                                     "integer-encoded RGB color.",
                                     col+tx, row+ty, color_object);
              } // invalid color
/*
            t[0] = (unsigned char) irgb_red (color);
            t[1] = (unsigned char) irgb_green (color);
            t[2] = (unsigned char) irgb_blue (color);
 */
            t[0] = IRGB_RED (color);
            t[1] = IRGB_GREEN (color);
            t[2] = IRGB_BLUE (color);

            // Advance to the next pixel
            t += tbpp;
          } // for each column

        // Advance to the next row
        rowdata += trowstride;
      } // for each row

    // Push the modified tile back to the server
    tile_update (stream, tsize, tdata);
    free (tdata);
  } while (tile_stream_advance (stream));

  // Close the stream
  tile_stream_close (stream);

  // Clean up
  MZ_GC_UNREG ();

  // And we're done
  return argv[0];
} // drawable_recompute_core

/**
 * Transform the pixels in a drawable.  (Inner code for image-transform! and
 * other functions.)
 * 
 * Parameters:
 *   0: image-id
 *   1: drawable-id
 *   2: lambda (x,y) -> color
 */
Scheme_Object *
drawable_transform_core (int argc, Scheme_Object **argv)
{
  Scheme_Object *stream = NULL;	// The tile stream
  Scheme_Object *tile = NULL;	// Info on the current tile
  Scheme_Object *fun  = NULL;   // The color function
  Scheme_Object *params[1] = { NULL };
                                // Parameters for the inner fun
  Scheme_Object *color_object;  // The color as a Scheme value
  int color;                    // The color as an iRGB
  unsigned char r, g, b;        // The color components

  int tsize;
  unsigned char *tdata;
  int tbpp;
  int trowstride;
  int tx;
  int ty;
  int tw;
  int th;

  // Set up for garbage collection
  MZ_GC_DECL_REG (3);
  MZ_GC_VAR_IN_REG (0, stream);
  MZ_GC_VAR_IN_REG (1, fun);
  MZ_GC_VAR_IN_REG (2, params[0]);
  MZ_GC_REG ();

  // Get the function
  fun = argv[2];

  // Get a stream
  stream = tile_stream_new (argv[0], argv[1]);

  // Check the validity of the stream
  // STUB

  // Get all the tiles 
  do {
    // Get the data from the current tile 
    tile = tile_stream_get (stream);
    unpack_tile (tile, &tsize, &tdata, &tbpp, &trowstride, &tx, &ty, &tw, &th);

    // Process the tile
    unsigned char *rowdata = tdata;
    // Do each row
    int row;
    for (row = 0; row < th; row++)
      {
        // Grab a pointer to the start of the data
        unsigned char *t = rowdata;

        // Do each column in the row
        int col;
        for (col = 0; col < tw; col++)
          {
            color = irgb_new (t[0], t[1], t[2]);
            params[0] = scheme_make_integer (color); 
            color_object = scheme_apply (fun, 1, params);
            color = scheme_object_to_irgb (color_object);
            if (color < 0) 
              {
                scheme_signal_error ("At position (%d,%d), a color function "
                                     "returned %V, instead of an "
                                     "integer-encoded RGB color.",
                                     col+tx, row+ty, color_object);
              } // invalid color
/*
            t[0] = (unsigned char) irgb_red (color);
            t[1] = (unsigned char) irgb_green (color);
            t[2] = (unsigned char) irgb_blue (color);
 */
            t[0] = IRGB_RED (color);
            t[1] = IRGB_GREEN (color);
            t[2] = IRGB_BLUE (color);

            // Advance to the next pixel
            t += tbpp;
          } // for each column

        // Advance to the next row
        rowdata += trowstride;
      } // for each row

    // Push the modified tile back to the server
    tile_update (stream, tsize, tdata);
    free (tdata);
  } while (tile_stream_advance (stream));

  // Close the stream
  tile_stream_close (stream);

  // Clean up
  MZ_GC_UNREG ();

  // And we're done
  return argv[0];
} // drawable_transform_core


// +-------------+-----------------------------------------------------
// | Experiments |
// +-------------+

/**
 * Fill a drawable with a single color.  Intended primarily as a test
 * of tiles.  (This extended version takes loudbus_call and gimp as
 * parameters so that I can check where errors might occur.

 * 
 * Parameters:
 *   0: loudbus-call
 *   1: gimp
 *   2: image-id
 *   3: drawable-id
 *   4: color
 */
Scheme_Object *
drawable_fill_extended (int argc, Scheme_Object **argv)
{
  Scheme_Object *result = scheme_void;

  // Check parameters
  // STUB

  // Grab the call function
  loudbus_call = argv[0];
  gimpplus = argv[1];
 
  // Set up for garbage collection
  MZ_GC_DECL_REG (2);
  MZ_GC_VAR_IN_REG (0, loudbus_call);
  MZ_GC_VAR_IN_REG (1, gimpplus);
  MZ_GC_REG ();

  // Do the real computation
  result = drawable_fill_core (argc-2, argv+2);

  // Clean up
  MZ_GC_UNREG ();

  // And we're done
  return result;
} // drawable_fill_extended


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
  register_scheme_function (drawable_fill_extended, "drawable-fill-extended", 5, 5, menv);
  register_scheme_function (drawable_fill_core, "_drawable-fill!", 3, 3, menv);
  register_scheme_function (drawable_recompute_core, "_drawable-recompute!", 3, 3, menv);
  register_scheme_function (drawable_transform_core, "_drawable-transform!", 3, 3, menv);
  register_scheme_function (tile_core_init, "tile-core-init", 2, 2, menv);

  // Clean up
  scheme_finish_primitive_module (menv);
  MZ_GC_UNREG ();

  // And we're done
  return scheme_void;
} // scheme_reload


