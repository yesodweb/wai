#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>

#include <stdio.h>

#include "warp.h"

ssize_t warp_recv(int fd, void* buf, size_t len, int flags) {
  ssize_t bytes = recv(fd, buf, len, flags);
  if (bytes == -1) {
    return -errno;
  }
  return bytes;
}
