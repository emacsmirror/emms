#include <stdlib.h>
#include <stdio.h>
#include <tag_c.h>

int
main (int argc, char **argv)
{
  TagLib_File *file;
  TagLib_Tag *tag;

  if (argc != 2)
    {
      fprintf (stderr, "usage: emms-print-metadata file.{mp3,ogg,flac}\n");
      exit (1);
    }

  file = taglib_file_new (argv[1]);
  tag = taglib_file_tag (file);

  printf ("info-artist=%s\n", taglib_tag_artist (tag));
  printf ("info-title=%s\n", taglib_tag_title (tag));
  printf ("info-album=%s\n", taglib_tag_album (tag));
  printf ("info-tracknumber=%d\n", taglib_tag_track (tag));
  printf ("info-year=%d\n", taglib_tag_year (tag));
  printf ("info-genre=%s\n", taglib_tag_genre (tag));
  printf ("info-note=%s\n", taglib_tag_comment (tag));
  printf ("info-playing-time=%d\n", taglib_audioproperties_length (taglib_file_audioproperties (file)));

  taglib_tag_free_strings ();
  taglib_file_free (file);

  return 0;
}
