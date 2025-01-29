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
#include<pipewire/loop.h>

struct spa_pod *_spa_format_audio_raw_build(struct spa_pod_builder *builder, uint32_t id, struct spa_audio_info_raw *info){
  return spa_format_audio_raw_build(builder, id, info);
}

int _spa_format_parse(const struct spa_pod *format, uint32_t *media_type, uint32_t *media_subtype){
  return spa_format_parse(format, media_type, media_subtype);
}

int _spa_format_audio_raw_parse(const struct spa_pod *format, struct spa_audio_info_raw *info){
  return spa_format_audio_raw_parse(format, info);
}

int _pw_loop_enter(struct pw_loop *loop){
  return pw_loop_enter(loop);
}

int _pw_loop_leave(struct pw_loop *loop){
  return pw_loop_leave(loop);
}

int _pw_loop_iterate(struct pw_loop *loop, int count){
  return pw_loop_iterate(loop, count);
}

// cc -shared -o pipewire-spa-amd64.so -I/usr/include/spa-0.2 -I/usr/include/pipewire-0.3 -O3 -fPIC pipewire-spa.c
