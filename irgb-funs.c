#ifndef __IRGB_FUNS_C__
#define __IRGB_FUNS_C__

/**
 * irgb-funs.c
 *   The basic irgb functions.  Designed as a file that can be
 *   included because using additional libraries with raco is
 *   a bit of a pain.
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


// +--------+----------------------------------------------------------
// | Macros |
// +--------+

#define IRGB_RED(COLOR) (unsigned char) ((COLOR >> 16) & 255)
#define IRGB_GREEN(COLOR) (unsigned char) ((COLOR >> 8) & 255)
#define IRGB_BLUE(COLOR) (unsigned char) (COLOR & 255)


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


// +-----------------------+-------------------------------------------
// | Integer RGB Utilities |
// +-----------------------+

#include "irgb-funs.c"

int
irgb_alpha (int color)
{
  return ((color >> 24) & 255);
} // irgb_alpha

int
irgb_blue (int color)
{
  return (color & 255);
} // irgb_blue

int
irgb_green (int color)
{
  return ((color >> 8) & 255);
} // irgb_green

int
irgb_new (int r, int g, int b)
{
  return ((byte (r) << 16) | (byte (g) << 8) | byte (b));
} // irgb_new

int
irgb_red (int color)
{
  return ((color >> 16) & 255);
} // irgb_red

int
irgba_new (int r, int g, int b, int a)
{
  return ((byte (a) << 24) | (byte (r) << 16) | (byte (g) << 8) | byte (b));
} // irgb_new

#endif // __IRGB_FUNS_C__
