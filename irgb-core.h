#ifndef __IRGB_CORE_H__
#define __IRGB_CORE_H__

/**
 * irgb-core.h
 *   The core integer RGB functions in C.
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


// +--------------------+----------------------------------------------
// | Exported Functions |
// +--------------------+

int irgb_alpha (int color);
int irgb_blue (int color);
int irgb_green (int color);
int irgb_new (int r, int g, int b);
int irgb_red (int color);
int irgba_new (int r, int g, int b, int a);

#endif // __IRGB_CORE_H__
