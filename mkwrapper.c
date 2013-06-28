/**
 * makewrapper.c
 *   Makes a wrapper for a D-Bus service.  One of SamR's temporary
 *   hacks.
 *
 *   Usage:
 *     makewrapper file
 *   where
 *     file lists all of the functions.
 *
 *   [Insert GPL 3.0]
 */

// +---------+--------------------------------------------------------
// | Headers |
// +---------+

#include <ctype.h>              // For isalpha and isdigit
#include <stdio.h>              // For doing input and output
#include <string.h>             // For strcpy and such


// +-----------+------------------------------------------------------
// | Constants |
// +-----------+

/**
 * The maximum number of characters in a line.
 */
#define LINESIZE 128


// +---------+--------------------------------------------------------
// | Helpers |
// +---------+

/**
 * Replace each instance of source in str with target. 
 * Also, convert quatation marks to spaces
 */
void
replace_character (char *str, char source, char target)
{
  int i = 0;
  while (str[i] != '\0')
    {
      if (str[i] == source)
        {  
          str[i] = target;
        } // if we matched the source
       i++;
    } // while
} // replace_character

/**
 * Read a line of characters, ignoring things that aren't alphanumeric or _.
 */
int
read_line_hack (FILE *stream, char *line, int MAXLEN)
{
  int i = 0;
  int tmp;
  while ((i < MAXLEN-1) && ((tmp = getc (stream)) != '\n') && (tmp != EOF))
    { 
      if (isalpha (tmp) || isdigit (tmp) || (tmp == '_'))
        {
          line[i++] = (char) tmp;
        }
    } // while
  line[i] = '\0';
  return (i > 0) || (tmp != EOF);
} // read_line_hack


// +------+-----------------------------------------------------------
// | Main |
// +------+

int
main (int argc, char *argv[])
{
  char line[LINESIZE];
  char dash[LINESIZE];

  // Verfify usage
  if (argc != 2)
    {
      fprintf (stderr, "Usage: makewrapper FILENAME\n");
      return 1;
    }


  // Print preliminary racket code.
  printf (
  	"#lang racket\n"
  	"(require louDBus/unsafe)\n"
        "(provide (all-defined-out))\n\n"
	);

  // Add the utility functions.  (Should some move to unsafe.rkt?)
  printf (
"(define gimp (loudbus-proxy \"edu.grinnell.cs.glimmer.GimpDBus\"\n"
"                            \"/edu/grinnell/cs/glimmer/gimp\"\n"
"                            \"edu.grinnell.cs.glimmer.pdb\"))\n"
"(define loudbus-helper\n"
"  (lambda (fun)\n"
"    (lambda args\n"
"      (apply loudbus-call (cons gimp (cons fun args))))))\n\n"
    );

  FILE *file = fopen (argv[1], "r");
  while (read_line_hack (file, line, LINESIZE))
    {
      // Verify that the line is nonempty.
      if (line[0] != '\0')
        {
          strncpy (dash, line, LINESIZE);
          replace_character (dash, '_', '-');
          printf  ("(define %s (loudbus-helper '%s))\n", dash, line);
        }
    } // while
  fclose (file);

  return 0;
} // main
