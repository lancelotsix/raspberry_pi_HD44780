#include <poll.h>
#include <stdio.h>
#include <errno.h>

int _c_write_to_file (char* file_path, char* img)
{
   FILE* f = fopen (file_path, "w");
   if (f)
   {
      fprintf (f, "%s", img);
      fclose (f);
      return 0;
   } else
   {
      if (errno == EACCES) return -1;
      return -2;
   }
}
