#include <string.h>

void memcpy_off( char *dst, int doff, char *src, int soff, int len )
{
  memcpy( dst + doff, src + soff, len );
}

