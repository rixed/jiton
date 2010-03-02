#ifndef CODEBUF_H_100223
#define CODEBUF_H_100223

#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>

struct codebuf {
	int8_t *addr;
	size_t length;
	bool need_flush;
};

int codebuf_ctor(struct codebuf *, size_t length, char const *filename);
void codebuf_dtor(struct codebuf *);
void codebuf_poke(struct codebuf *, size_t offset, int8_t value);
uint8_t codebuf_peek(struct codebuf *, size_t offset);
void codebuf_exec(struct codebuf *, size_t offset, unsigned nb_params, intptr_t *params);

#endif
