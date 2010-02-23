#ifndef CODEBUF_H_100223
#define CODEBUF_H_100223

#include <unistd.h>
#include <stdint.h>

struct codebuf {
	int8_t *addr;
	size_t length;
};

int codebuf_ctor(struct codebuf *, size_t length, char const *filename);
void codebuf_dtor(struct codebuf *);
void codebuf_poke(struct codebuf *, size_t offset, int8_t value);
void codebuf_exec(struct codebuf *, size_t offset, unsigned nb_params, intptr_t *params);

#endif
