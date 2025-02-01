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
#include<mixed.h>

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

struct pw_drain_data{
  struct mixed_pack *pack;
  struct pw_stream *stream;
};

int _pw_drain_mix(struct mixed_segment *segment){
  struct pw_drain_data *data = (struct pw_drain_data *)segment->data;
  uint32_t octets = UINT32_MAX;
  void *area;
  mixed_pack_request_read(&area, &octets, data->pack);
  uint16_t framesize = mixed_samplesize(pack->format)*pack->channels;
  struct pw_buffer *buf = pw_stream_dequeue_buffer(data->stream);
  if(!buf) return 1;
  struct pw_data_buffer *b = buf->buffer;
  struct spa_data *d = b->data;
  if(!d->data) return 1;

  struct spa_chunk *c = d->chunk;
  octets = min(min(samples, d->max_size), buf->requested*framesize);
  memcpy(d->data, area, octets);
  c->offset = 0;
  c->stride = framesize;
  c->size = octets;
  pw_stream_queue_buffer(data->stream, buf);
  mixed_pack_finish_read(octets, data->pack);
}

// cc -shared -o pipewire-spa-amd64.so -I/usr/include/spa-0.2 -I/usr/include/pipewire-0.3 -I../lib/src -O3 -fPIC pipewire-spa.c
