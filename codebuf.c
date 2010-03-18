#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#ifdef HAVE_CACHECTL
#	include <asm/cachectl.h>
#	include <sys/cachectl.h>
#endif
#include "codebuf.h"

static size_t filesize(int fd)
{
	off_t end = lseek(fd, 0, SEEK_END);
	if (end == (off_t) -1) {
		perror("lseek");
		return 0;
	}
	(void)lseek(fd, 0, SEEK_SET);
	return end;
}

int codebuf_ctor(struct codebuf *codebuf, size_t length, char const *filename)
{
	int fd = -1;

	codebuf->addr = NULL;
	codebuf->length = length;
	codebuf->need_flush = true;

	if (filename) {
		fd = open(filename, O_RDWR);
		if (fd == -1 && errno == ENOENT) {
			fd = open(filename, O_RDWR | O_CREAT, 0774);
		}
		if (fd == -1) {
			fprintf(stderr, "Cannot open %s : %s\n", filename, strerror(errno));
			return -1;
		} else {
			codebuf->length = filesize(fd);
		}
		if (length > codebuf->length) {
			if (0 != ftruncate(fd, length)) {
				fprintf(stderr, "Cannot ftruncate %s : %s\n", filename, strerror(errno));
				return -1;
			}
			codebuf->length = length;
		}
	}

	fprintf(stdout, "MMapping file '%s' (fd=%d), required length=%zu, actual length=%zu\n",
		filename, fd, length, codebuf->length);
	void *addr = mmap(NULL, codebuf->length, PROT_EXEC|PROT_READ|PROT_WRITE,
			(fd == -1 ? MAP_PRIVATE|MAP_ANONYMOUS : MAP_SHARED) | MAP_EXECUTABLE, fd, 0);

	if (fd) (void)close(fd);
	
	if (addr == MAP_FAILED) {
		fprintf(stderr, "Cannot mmap file '%s' : %s\n", filename, strerror(errno));
		return -1;
	}

	codebuf->addr = addr;

	return 0;
}

void codebuf_dtor(struct codebuf *codebuf)
{
	if (codebuf->addr) {
		munmap(codebuf->addr, codebuf->length);
	}
}

void codebuf_poke(struct codebuf *codebuf, size_t offset, int8_t value)
{
	assert(offset < codebuf->length);
	codebuf->addr[offset] = value;
	codebuf->need_flush = true;
}

uint8_t codebuf_peek(struct codebuf *codebuf, size_t offset)
{
	assert(offset < codebuf->length);
	return codebuf->addr[offset];
}

static int codebuf_flush(struct codebuf *codebuf)
{
#	ifdef HAVE_CACHECTL
	printf("Flushing pages from %p\n", codebuf->addr);

	if (0 != cacheflush(codebuf->addr, codebuf->length, ICACHE|DCACHE)) {
		fprintf(stderr, "Cannot flush caches : %s\n", strerror(errno));
		return -1;
	}
#	endif
	codebuf->need_flush = false;
	return 0;
}

typedef void proc0(void);
typedef void proc1(intptr_t);
typedef void proc2(intptr_t, intptr_t);
typedef void proc3(intptr_t, intptr_t, intptr_t);
typedef void proc4(intptr_t, intptr_t, intptr_t, intptr_t);
typedef void proc5(intptr_t, intptr_t, intptr_t, intptr_t, intptr_t);
void codebuf_exec(struct codebuf *codebuf, size_t offset, unsigned nb_params, intptr_t *params)
{
	if (codebuf->need_flush) (void)codebuf_flush(codebuf);

	switch (nb_params) {
		case 0:
			{
				proc0 *func = (proc0 *)(codebuf->addr + offset);
				func();
			}
			break;
		case 1:
			{
				proc1 *func = (proc1 *)(codebuf->addr + offset);
				func(params[0]);
			}
			break;
		case 2:
			{
				proc2 *func = (proc2 *)(codebuf->addr + offset);
				func(params[0], params[1]);
			}
			break;
		case 3:
			{
				proc3 *func = (proc3 *)(codebuf->addr + offset);
				func(params[0], params[1], params[2]);
			}
			break;
		case 4:
			{
				proc4 *func = (proc4 *)(codebuf->addr + offset);
				func(params[0], params[1], params[2], params[3]);
			}
			break;
		case 5:
			{
				proc5 *func = (proc5 *)(codebuf->addr + offset);
				func(params[0], params[1], params[2], params[3], params[4]);
			}
			break;
	}
}

