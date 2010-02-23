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
	}

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
}

void codebuf_exec(struct codebuf *codebuf, size_t offset, unsigned nb_params, intptr_t *params)
{
	void (*func)(void) = (void (*)(void))(codebuf->addr + offset);
	func();
}
