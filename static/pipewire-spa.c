#if defined(__linux__)

#define _GNU_SOURCE
#include <features.h>
#undef _GNU_SOURCE

#if defined(__USE_GNU)
#if defined(__x86_64__)
__asm__(".symver memcpy,memcpy@GLIBC_2.2.5");
__asm__(".symver strlen,strlen@GLIBC_2.2.5");
#endif
#if defined(__aarch64__) || defined(_M_ARM64)
__asm__(".symver memcpy,memcpy@GLIBC_2.17");
__asm__(".symver strlen,strlen@GLIBC_2.17");
#endif
#endif
#endif

#include<spa/param/audio/format-utils.h>

struct spa_pod *_spa_format_audio_raw_build(struct spa_pod_builder *builder, uint32_t id, struct spa_audio_info_raw *info){
  return spa_format_audio_raw_build(builder, id, info);
}

// cc -shared -o pipewire-spa-amd64.so -I/usr/include/spa-0.2 -O3 -fPIC pipewire-spa.c
