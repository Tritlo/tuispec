#include <sys/ioctl.h>

int tuispec_enable_packet_mode(int fd) {
    int packet_mode = 1;
    return ioctl(fd, TIOCPKT, &packet_mode);
}
