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

int _spa_format_parse(const struct spa_pod *format, uint32_t *media_type, uint32_t *media_subtype){
  return spa_format_parse(format, media_type, media_subtype);
}

int _spa_format_audio_raw_parse(const struct spa_pod *format, struct spa_audio_info_raw *info){
  return spa_format_audio_raw_parse(format, info);
}

// cc -shared -o pipewire-spa-amd64.so -I/usr/include/spa-0.2 -O3 -fPIC pipewire-spa.c

if (spa_format_parse(param, &data->format.media_type, &data->format.media_subtype) < 0)
                return;
 
        /* only accept raw audio */
        if (data->format.media_type != SPA_MEDIA_TYPE_audio ||
            data->format.media_subtype != SPA_MEDIA_SUBTYPE_raw)
                return;
 
        /* call a helper function to parse the format for us. */
        spa_format_audio_raw_parse(param, &data->format.info.raw);
 
        fprintf(stdout, "capturing rate:%d channels:%d\n",
                        data->format.info.raw.rate, data->format.info.raw.channels);
