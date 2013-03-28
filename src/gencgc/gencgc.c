/* gencgc.c                                               -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: $
 */
/* we need to use internal mutex and so */
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/gencgc.h"
#include "gc-internal.h"
#include <sagittarius/core.h>
#include <sagittarius/pair.h>
#include <sagittarius/thread.h>
#include <sagittarius/vm.h>

/* some parameters for debugging */
static int debug_flag = 0;

size_t bytes_consed_between_gcs = 12*1024*1024;

/* probably enough for Cygwin ... */
#define MAX_STATIC_ROOT_COUNT 256
typedef struct static_roots {
  void *start;
  void *end;
} static_roots_t;
static int    current_static_space_index = 0;
static static_roots_t static_spaces[MAX_STATIC_ROOT_COUNT] = {{NULL,},};

void add_static_root(void *start, void *end)
{
  int index = current_static_space_index++;
  static_spaces[index].start = start;
  static_spaces[index].end = end;
}

static GC_thread_context_t root_context = {}; /* dummy context */
GC_thread_context_t *all_threads = NULL;

static page_index_t gc_find_freeish_pages(page_index_t *restart_page_ptr,
					  size_t nbytes, int page_type_flag);
/* GC parameters */

/* Generations 0-5 are normal collected generations, 6 is only used as
   scratch space by the collector, and should never get collected.
 */
enum {
  SCRATCH_GENERATION = GENERATIONS + 1,
  NUM_GENERATIONS,
};

#ifndef __CYGWIN__
#define enable_page_protection TRUE
#else
/* Probably it's better to check the card bytes on Cygwin. but for now */
#define enable_page_protection FALSE
#endif

#if (CARD_BYTES >= PAGE_BYTES)
#define large_object_size (4 * CARD_BYTES)
#else
#define large_object_size (4 * PAGE_BYTES)
#endif

int need_gc = FALSE;
size_t dynamic_space_size;

/* Largest allocation seen since last GC. */
static size_t large_allocation = 0;

/* TODO remove this */
#define verify_gens GENERATIONS

size_t bytes_allocated = 0;
size_t auto_gc_trigger = 0;
/* The source and destination generations. These are set before a GC starts
   scavenging. */
generation_index_t from_space;
generation_index_t new_space;

/* Set to TRUE when in GC */
static int gc_active_p = FALSE;

/* An array of page structures is allocated on gc initialization.
   This helps to quickly map between an address and its page structure.
   page_table_pages is set from the size of the dynamic space. */
page_index_t page_table_pages;
page_t *page_table;

static inline int page_allocated_p(page_index_t page) {
  return (page_table[page].allocated != FREE_PAGE_FLAG);
}

static inline int page_no_region_p(page_index_t page) {
  return !(page_table[page].allocated & OPEN_REGION_PAGE_FLAG);
}

static inline int page_allocated_no_region_p(page_index_t page) {
  return ((page_table[page].allocated & (UNBOXED_PAGE_FLAG | BOXED_PAGE_FLAG))
	  && page_no_region_p(page));
}

static inline int page_free_p(page_index_t page) {
  return (page_table[page].allocated == FREE_PAGE_FLAG);
}

static inline int page_boxed_p(page_index_t page) {
  return (page_table[page].allocated & BOXED_PAGE_FLAG);
}


static inline int page_boxed_no_region_p(page_index_t page) {
  return page_boxed_p(page) && page_no_region_p(page);
}

static inline int page_unboxed_p(page_index_t page) {
  /* Both flags set == boxed code page */
  return ((page_table[page].allocated & UNBOXED_PAGE_FLAG)
	  && !page_boxed_p(page));
}
/*
static inline int page_array_p(page_index_t page) {
  return ((page_table[page].allocated & ARRAY_PAGE_FLAG));
}
*/
static inline int protect_page_p(page_index_t page,
				 generation_index_t generation) {
  return (page_boxed_no_region_p(page)
	  && (page_table[page].bytes_used != 0)
	  && !page_table[page].dont_move
	  && (page_table[page].gen == generation));
}

/* To map addresses to page structures the address of the first page
   is needed */
static void *heap_base = NULL;

/* Calculate the start address for the given page number */
inline void * page_address(page_index_t page_num)
{
  return (heap_base + (page_num * CARD_BYTES));
}

/* Calculate the address where the allocation region associated with
   the page starts. */
static inline void * page_region_start(page_index_t index)
{
  return page_address(index) - page_table[index].region_start_offset;
}

/* Find the page index within the page_table for the given address.
   Return -1 on failure. */
inline page_index_t find_page_index(void *addr)
{
  if (addr >= heap_base) {
    page_index_t index = ((uintptr_t)addr - (uintptr_t)heap_base) / CARD_BYTES;
    if (index < page_table_pages) return index;
  }
  return -1;
}

static size_t npage_bytes(page_index_t npages)
{
  return ((size_t)npages) * CARD_BYTES;
}

/* Check that X is a higher address than Y and return offset from Y to
 * X in bytes. */
static inline size_t void_diff(void *x, void *y)
{
    return (uintptr_t)x - (uintptr_t)y;
}

/* a structure to hold the state of a generation */
typedef struct generation {

  /* the first page that gc_alloc() checks on its next call */
  page_index_t alloc_start_page;

  /* the first page that gc_alloc_unboxed() checks on its next call */
  page_index_t alloc_unboxed_start_page;

  /* the first page that gc_alloc_array() checks on its next call */
  /* page_index_t alloc_array_start_page; */

  /* the first page that gc_alloc_large() (boxed) considers on its next
     call (Although it always allocates after the boxed_region.) */
  page_index_t alloc_large_start_page;

  /* the first page that gc_alloc_large() (unboxed) considers on its next
     call (Although it always allocates after the 
     current_unboxed_region.) */
  page_index_t alloc_large_unboxed_start_page;

  /* the bytes allocated to this generation */
  size_t bytes_allocated;

  /* the number of bytes at which to trigger a GC */
  size_t gc_trigger;

  /* to calculate a new level for gc_trigger */
  size_t bytes_consed_between_gc;

  /* the number of GCs since the last raise */
  int num_gc;

  /* the number of GCs to run the generations before raising objects to the
     next generation */
  int number_of_gcs_before_promotion;

  /* the cumulative sum of the bytes allocated to this generation. It is
     cleared after a GC on this generations, and update before new objects
     are added from a GC of a younger generation. Dividing by the
     bytes_allocated will give the average age of the memory in this
     generation since its last GC. */
  size_t cum_sum_bytes_allocated;

  /* a minimum average memory age before a GC will occur helps prevent
     a GC when a large number of new live objects have been added, in which
     case a GC could be a waste of time. */
  double minimum_age_before_gc;
} generation_t;

/* an array of generation structure. There needs to be one more generation
   structure than actual generations as the oldest generation is temporarily
   raised then lowered. */
static generation_t generations[NUM_GENERATIONS];

#define gencgc_oldest_gen_to_gc (GENERATIONS-1)

/* The maximum free page in the heap in maintained and used to update
   ALLOCATION_POINTER which is used by the room function to limit its
   search of the heap. */
page_index_t last_free_page;
/* mutexes */
static SgInternalMutex free_pages_lock;
static SgInternalMutex allocation_lock;
static SgInternalMutex context_lock;

/* should these be parameters? */
#define gencgc_release_granularity PAGE_BYTES
#define gencgc_alloc_granularity   0

static double generation_average_age(generation_index_t gen)
{
  if (generations[gen].bytes_allocated == 0) return 0.0;
  else return (((double)generations[gen].cum_sum_bytes_allocated)
	       / ((double)generations[gen].bytes_allocated));
}

/* Count the number of pages which are write-protected within the
 * given generation. */
static page_index_t 
count_write_protect_generation_pages(generation_index_t generation)
{
  page_index_t i, count = 0;

  for (i = 0; i < last_free_page; i++)
    if (page_allocated_p(i)
	&& (page_table[i].gen == generation)
	&& (page_table[i].write_protected == 1))
      count++;
  return count;
}

/* Count the number of pages within the given generation. */
static page_index_t count_generation_pages(generation_index_t generation)
{
  page_index_t i;
  page_index_t count = 0;

  for (i = 0; i < last_free_page; i++)
    if (page_allocated_p(i)
	&& (page_table[i].gen == generation))
      count++;
  return count;
}


static void write_generation_stats(FILE *file)
{
  generation_index_t i;

  /* Print the heap stats. */
  fprintf(file,
	  ";; Gen    StaPg   UbSta LaSta LUbSt   Boxed Unboxed    LB   LUB"
	  " !move    Alloc    Waste     Trig   WP GCs Mem-age\n");

  for (i = 0; i < SCRATCH_GENERATION; i++) {
    page_index_t j;
    page_index_t boxed_cnt = 0;
    page_index_t unboxed_cnt = 0;
    page_index_t large_boxed_cnt = 0;
    page_index_t large_unboxed_cnt = 0;
    page_index_t pinned_cnt=0;

    for (j = 0; j < last_free_page; j++)
      if (page_table[j].gen == i) {

	/* Count the number of boxed pages within the given
	 * generation. */
	if (page_boxed_p(j)) {
	  if (page_table[j].large_object)
	    large_boxed_cnt++;
	  else
	    boxed_cnt++;
	}
	if(page_table[j].dont_move) pinned_cnt++;
	/* Count the number of unboxed pages within the given
	 * generation. */
	if (page_unboxed_p(j)) {
	  if (page_table[j].large_object)
	    large_unboxed_cnt++;
	  else
	    unboxed_cnt++;
	}
      }

    /* ASSERT(generations[i].bytes_allocated  */
    /* 	   == count_generation_bytes_allocated(i)); */
    fprintf(file,
	    ";;   %1d: %7d %7d %5d %5d",
	    i,
	    generations[i].alloc_start_page,
	    generations[i].alloc_unboxed_start_page,
	    generations[i].alloc_large_start_page,
	    generations[i].alloc_large_unboxed_start_page);
    fprintf(file,
	    " %7d %7d %5d %5d %5d",
	    boxed_cnt, unboxed_cnt, large_boxed_cnt,
	    large_unboxed_cnt, pinned_cnt);
    fprintf(file,
	    " %8u %8u %8u %4d"" %3d %7.4f\n",
	    generations[i].bytes_allocated,
	    (npage_bytes(count_generation_pages(i)) 
	     - generations[i].bytes_allocated),
	    generations[i].gc_trigger,
	    count_write_protect_generation_pages(i),
	    generations[i].num_gc,
	    generation_average_age(i));
  }
  fprintf(file,";;   Total bytes allocated    = %u\n", bytes_allocated);
  fprintf(file,";;   Dynamic-space-size bytes = %u\n", dynamic_space_size);
}

static void 
write_heap_exhaustion_report(FILE *file, long available, long requested)
{
  fprintf(file,
	  "Heap exhausted during %s: %ld bytes available, %ld requested.\n",
	  gc_active_p ? "garbage collection" : "allocation",
	  available,
	  requested);
  write_generation_stats(file);
}

static void report_heap_exhaustion(long available, long requested)
{
  write_heap_exhaustion_report(stderr, available, requested);
}

/* Zero the pages from start to end (inclusive), but use os_invalidate. */
static void zero_pages_with_mmap(page_index_t start, page_index_t end)
{
  page_index_t i;
  void *addr = page_address(start), *new_addr;
  size_t length = npage_bytes(1 + end - start);

  if (start > end) return;
#ifndef __CYGWIN__
  os_invalidate(addr, length);
  new_addr = os_validate(addr, length);
  if (new_addr == NULL || new_addr != addr) {
    Sg_Panic("remap_free_pages: page moved, 0x%08x ==> 0x%08x",
             start, new_addr);
  }
#else
  /* for some reason Cygwin doesn't allow me to do above trick
     because of munmap.
     TODO probably we can resolve it changing CARD_BYTES.
  */
  memset(addr, 0, length);
#endif
  for (i = start; i <= end; i++) {
    page_table[i].need_to_zero = FALSE;
  }
}

/* Zero the pages from start to end (inclusive), Generally done just after
   a new region has been allocated. */
static void zero_pages(page_index_t start, page_index_t end)
{
  if (start > end) return;
  memset(page_address(start), 0, npage_bytes(1 + end - start));
}

static void zero_and_mark_pages(page_index_t start, page_index_t end)
{
  page_index_t i;
  zero_pages(start, end);
  for (i = start; i <= end; i++) page_table[i].need_to_zero = FALSE;
}

/* Zero the pages from start to end (inclusive), except for those pages
   that are known to already zeroed. Mark all pages in the ranges as
   non h-zeroed. */
static void zero_dirty_pages(page_index_t start, page_index_t end)
{
  page_index_t i, j;
  for (i = start; i <= end; i++) {
    if (!page_table[i].need_to_zero) continue;
    for (j = i+1; (j <= end) && (page_table[j].need_to_zero); j++);
    zero_pages(i, j-1);
    i = j;
  }
  for (i = start; i <= end; i++) page_table[i].need_to_zero = TRUE;
}

/* comment from SBCL gencgc.c */
/*
 * To support quick and inline allocation, regions of memory can be
 * allocated and then allocated from with just a free pointer and a
 * check against an end address.
 *
 * Since objects can be allocated to spaces with different properties
 * e.g. boxed/unboxed, generation, ages; there may need to be many
 * allocation regions.
 *
 * Each allocation region may start within a partly used page. Many
 * features of memory use are noted on a page wise basis, e.g. the
 * generation; so if a region starts within an existing allocated page
 * it must be consistent with this page.
 *
 * During the scavenging of the newspace, objects will be transported
 * into an allocation region, and pointers updated to point to this
 * allocation region. It is possible that these pointers will be
 * scavenged again before the allocation region is closed, e.g. due to
 * trans_list which jumps all over the place to cleanup the list. It
 * is important to be able to determine properties of all objects
 * pointed to when scavenging, e.g to detect pointers to the oldspace.
 * Thus it's important that the allocation regions have the correct
 * properties set when allocated, and not just set when closed. The
 * region allocation routines return regions with the specified
 * properties, and grab all the pages, setting their properties
 * appropriately, except that the amount used is not known.
 *
 * These regions are used to support quicker allocation using just a
 * free pointer. The actual space used by the region is not reflected
 * in the pages tables until it is closed. It can't be scavenged until
 * closed.
 *
 * When finished with the region it should be closed, which will
 * update the page tables for the actual space used returning unused
 * space. Further it may be noted in the new regions which is
 * necessary when scavenging the newspace.
 *
 * Large objects may be allocated directly without an allocation
 * region, the page tables are updated immediately.
 *
 * Unboxed objects don't contain pointers to other objects and so
 * don't need scavenging. Further they can't contain pointers to
 * younger generations so WP is not needed. By allocating pages to
 * unboxed objects the whole page never needs scavenging or
 * write-protecting. */

/* We are only using two regions at present. Both are for the current
   newspace generation.*/
/* TODO probably we want to add region for array later. */
alloc_region_t boxed_region;
alloc_region_t unboxed_region;

/* The generation currently being allocated to. */
static generation_index_t gc_alloc_generation;

static inline page_index_t
generation_alloc_start_page(generation_index_t generation, int page_type_flag,
			    int large)
{
  if (large) {
    if (UNBOXED_PAGE_FLAG == page_type_flag) {
      return generations[generation].alloc_large_unboxed_start_page;
    } else if (BOXED_PAGE_FLAG & page_type_flag) {
      return generations[generation].alloc_large_start_page;
    } else {
      Sg_Panic("bad page type flag: %d", page_type_flag);
    }
  } else {
    if (UNBOXED_PAGE_FLAG == page_type_flag) {
      return generations[generation].alloc_unboxed_start_page;
    } else if (BOXED_PAGE_FLAG & page_type_flag) {
      return generations[generation].alloc_start_page;
    } else {
      Sg_Panic("bad page_type_flag: %d", page_type_flag);
    }
  }
  return -1;		/* dummy */
}

static inline void
set_generation_alloc_start_page(generation_index_t generation,
				int page_type_flag, int large,
                                page_index_t page)
{
  if (large) {
    if (UNBOXED_PAGE_FLAG == page_type_flag) {
      generations[generation].alloc_large_unboxed_start_page = page;
    } else if (BOXED_PAGE_FLAG & page_type_flag) {
      generations[generation].alloc_large_start_page = page;
    } else {
      Sg_Panic("bad page type flag: %d", page_type_flag);
    }
  } else {
    if (UNBOXED_PAGE_FLAG == page_type_flag) {
      generations[generation].alloc_unboxed_start_page = page;
    } else if (BOXED_PAGE_FLAG & page_type_flag) {
      generations[generation].alloc_start_page = page;
    } else {
      Sg_Panic("bad page type flag: %d", page_type_flag);
    }
  }
}

/* comment from SBCL gencgc.c */
/* Find a new region with room for at least the given number of bytes.
 *
 * It starts looking at the current generation's alloc_start_page. So
 * may pick up from the previous region if there is enough space. This
 * keeps the allocation contiguous when scavenging the newspace.
 *
 * The alloc_region should have been closed by a call to
 * gc_alloc_update_page_tables(), and will thus be in an empty state.
 *
 * To assist the scavenging functions write-protected pages are not
 * used. Free pages should not be write-protected.
 *
 * It is critical to the conservative GC that the start of regions be
 * known. To help achieve this only small regions are allocated at a
 * time.
 *
 * During scavenging, pointers may be found to within the current
 * region and the page generation must be set so that pointers to the
 * from space can be recognized. Therefore the generation of pages in
 * the region are set to gc_alloc_generation. To prevent another
 * allocation call using the same pages, all the pages in the region
 * are allocated, although they will initially be empty.
 */
static void gc_alloc_new_region(size_t nbytes, int page_type_flag, 
				alloc_region_t *alloc_region)
{
  page_index_t first_page;
  page_index_t last_page;
  size_t bytes_found;
  page_index_t i;

  /* Check that the region is in a reset state. */
  ASSERT((alloc_region->first_page == 0)
	 && (alloc_region->last_page == -1)
	 && (alloc_region->free_pointer == alloc_region->end_addr));
  Sg_LockMutex(&free_pages_lock);
  first_page = generation_alloc_start_page(gc_alloc_generation, page_type_flag,
					   0);
  last_page = gc_find_freeish_pages(&first_page, nbytes, page_type_flag);
  bytes_found = (CARD_BYTES - page_table[first_page].bytes_used)
    + npage_bytes(last_page-first_page);

  /* Set up the alloc_region. */
  alloc_region->first_page = first_page;
  alloc_region->last_page = last_page;
  alloc_region->start_addr = page_table[first_page].bytes_used
    + page_address(first_page);
  alloc_region->free_pointer = alloc_region->start_addr;
  alloc_region->end_addr = alloc_region->start_addr + bytes_found;

  /* Set up the pages. */

  /* The first page may have already been in use. */
  if (page_table[first_page].bytes_used == 0) {
    page_table[first_page].allocated = page_type_flag;
    page_table[first_page].gen = gc_alloc_generation;
    page_table[first_page].large_object = 0;
    page_table[first_page].region_start_offset = 0;
  }

  ASSERT(page_table[first_page].allocated == page_type_flag);
  page_table[first_page].allocated |= OPEN_REGION_PAGE_FLAG;

  ASSERT(page_table[first_page].gen == gc_alloc_generation);
  ASSERT(page_table[first_page].large_object == 0);

  for (i = first_page+1; i <= last_page; i++) {
    page_table[i].allocated = page_type_flag;
    page_table[i].gen = gc_alloc_generation;
    page_table[i].large_object = 0;
    /* This may not be necessary for unboxed regions (think it was
     * broken before!) */
    page_table[i].region_start_offset =
      void_diff(page_address(i),alloc_region->start_addr);
    page_table[i].allocated |= OPEN_REGION_PAGE_FLAG ;
  }
  /* Bump up last_free_page. */
  if (last_page+1 > last_free_page) {
    last_free_page = last_page+1;
    /* do we only want to call this on special occasions? like for
     * boxed_region? */
    /* set_alloc_pointer((void *)page_address(last_free_page)); */
  }
  Sg_UnlockMutex(&free_pages_lock);

/* #ifdef READ_PROTECT_FREE_PAGES */
/*   os_protect(page_address(first_page), */
/* 	     npage_bytes(1+last_page-first_page), */
/* 	     OS_VM_PROT_ALL); */
/* #endif */

  /* If the first page was only partial, don't check whether it's
   * zeroed (it won't be) and don't zero it (since the parts that
   * we're interested in are guaranteed to be zeroed).
   */
  if (page_table[first_page].bytes_used) {
    first_page++;
  }

  zero_dirty_pages(first_page, last_page);

  /* we can do this after releasing free_pages_lock */
  /* TODO make this as a parameter */
#if 0
  if (gencgc_zero_check) {
    size_t *p;
    for (p = (size_t *)alloc_region->start_addr;
	 p < (size_t *)alloc_region->end_addr; p++) {
      if (*p != 0) {
	Sg_Panic("The new region is not zero at %p (start=%p, end=%p).\n",
		 p, alloc_region->start_addr, alloc_region->end_addr);
      }
    }
  }
#endif
}

/* comment from SBCL gencgc.c */
/* If the record_new_objects flag is 2 then all new regions created
 * are recorded.
 *
 * If it's 1 then then it is only recorded if the first page of the
 * current region is <= new_areas_ignore_page. This helps avoid
 * unnecessary recording when doing full scavenge pass.
 *
 * The new_object structure holds the page, byte offset, and size of
 * new regions of objects. Each new area is placed in the array of
 * these structures pointer to by new_areas. new_areas_index holds the
 * offset into new_areas.
 *
 * If new_area overflows NUM_NEW_AREAS then it stops adding them. The
 * later code must detect this and handle it, probably by doing a full
 * scavenge of a generation. */
#define NUM_NEW_AREAS 512
static int record_new_objects = 0;
static page_index_t new_areas_ignore_page;
struct new_area {
    page_index_t page;
    size_t offset;
    size_t size;
};
static struct new_area (*new_areas)[];
static size_t new_areas_index;
size_t max_new_areas;

/* Add a new area to new_areas. */
static void add_new_area(page_index_t first_page, size_t offset, size_t size)
{
  size_t new_area_start, c;
  ssize_t i;

  /* Ignore if full. */
  if (new_areas_index >= NUM_NEW_AREAS) return;

  switch (record_new_objects) {
  case 0: return;
  case 1: 
    if (first_page > new_areas_ignore_page) return;
    break;
  case 2: break;
  default:
    Sg_Panic("GC invariant lost, file \"%s\", line %d\n", __FILE__, __LINE__);
  }

  new_area_start = npage_bytes(first_page) + offset;

  /* Search backwards for a prior area that this follows from. If
     found this will save adding a new area. */
  for (i = new_areas_index-1, c = 0; (i >= 0) && (c < 8); i--, c++) {
    size_t area_end =
      npage_bytes((*new_areas)[i].page)
      + (*new_areas)[i].offset
      + (*new_areas)[i].size;
    if (new_area_start == area_end) {
      (*new_areas)[i].size += size;
      return;
    }
  }

  (*new_areas)[new_areas_index].page = first_page;
  (*new_areas)[new_areas_index].offset = offset;
  (*new_areas)[new_areas_index].size = size;
  new_areas_index++;

  /* Note the max new_areas used. */
  if (new_areas_index > max_new_areas)
    max_new_areas = new_areas_index;
}

void gc_set_region_empty(alloc_region_t *region)
{
  region->first_page = 0;
  region->last_page = -1;
  region->start_addr = page_address(0);
  region->free_pointer = page_address(0);
  region->end_addr = page_address(0);
}

/* Update the tables for the alloc_region. The region may be added to
 * the new_areas.
 *
 * When done the alloc_region is set up so that the next quick alloc
 * will fail safely and thus a new region will be allocated. Further
 * it is safe to try to re-update the page table of this reset
 * alloc_region. */
void gc_alloc_update_page_tables(int page_type_flag, 
				 alloc_region_t *alloc_region)
{
  int more;
  page_index_t first_page;
  page_index_t next_page;
  size_t bytes_used;
  size_t region_size;
  size_t byte_cnt;
  page_bytes_t orig_first_page_bytes_used;

  first_page = alloc_region->first_page;

  /* Catch an unused alloc_region. */
  if ((first_page == 0) && (alloc_region->last_page == -1)) return;

  next_page = first_page+1;

  Sg_LockMutex(&free_pages_lock);

  if (alloc_region->free_pointer != alloc_region->start_addr) {
    /* some bytes were allocated in the region */
    orig_first_page_bytes_used = page_table[first_page].bytes_used;
    
    ASSERT(alloc_region->start_addr == (page_address(first_page)
					+ page_table[first_page].bytes_used));

    /* All the pages used need to be updated */

    /* Update the first page. */

    /* If the page was free then set up the gen, and
     * region_start_offset. */
    if (page_table[first_page].bytes_used == 0)
      ASSERT(page_table[first_page].region_start_offset == 0);
    page_table[first_page].allocated &= ~(OPEN_REGION_PAGE_FLAG);

    ASSERT(page_table[first_page].allocated & page_type_flag);
    ASSERT(page_table[first_page].gen == gc_alloc_generation);
    ASSERT(page_table[first_page].large_object == 0);

    byte_cnt = 0;

    /* Calculate the number of bytes used in this page. This is not
     * always the number of new bytes, unless it was free. */
    more = 0;
    if ((bytes_used = void_diff(alloc_region->free_pointer,
				page_address(first_page))) > CARD_BYTES) {
      bytes_used = CARD_BYTES;
      more = 1;
    }
    page_table[first_page].bytes_used = bytes_used;
    byte_cnt += bytes_used;

    /* All the rest of the pages should be free. We need to set
     * their region_start_offset pointer to the start of the
     * region, and set the bytes_used. */
    while (more) {
      page_table[next_page].allocated &= ~(OPEN_REGION_PAGE_FLAG);
      ASSERT(page_table[next_page].allocated & page_type_flag);
      ASSERT(page_table[next_page].bytes_used == 0);
      ASSERT(page_table[next_page].gen == gc_alloc_generation);
      ASSERT(page_table[next_page].large_object == 0);

      ASSERT(page_table[next_page].region_start_offset ==
	     void_diff(page_address(next_page), alloc_region->start_addr));

      /* Calculate the number of bytes used in this page. */
      more = 0;
      if ((bytes_used = void_diff(alloc_region->free_pointer,
				  page_address(next_page))) > CARD_BYTES) {
	bytes_used = CARD_BYTES;
	more = 1;
      }
      page_table[next_page].bytes_used = bytes_used;
      byte_cnt += bytes_used;
      next_page++;
    }

    region_size = void_diff(alloc_region->free_pointer, 
			    alloc_region->start_addr);
    bytes_allocated += region_size;
    generations[gc_alloc_generation].bytes_allocated += region_size;

    ASSERT((byte_cnt- orig_first_page_bytes_used) == region_size);

    /* Set the generations alloc restart page to the last page of
     * the region. */
    set_generation_alloc_start_page(gc_alloc_generation, page_type_flag,
				    0, next_page-1);

    /* Add the region to the new_areas if requested. */
    if (BOXED_PAGE_FLAG & page_type_flag)
      add_new_area(first_page, orig_first_page_bytes_used, region_size);
  } else {
    /* There are no bytes allocated. Unallocate the first_page if
     * there are 0 bytes_used. */
    page_table[first_page].allocated &= ~(OPEN_REGION_PAGE_FLAG);
    if (page_table[first_page].bytes_used == 0)
      page_table[first_page].allocated = FREE_PAGE_FLAG;
  }

  /* Unallocate any unused pages. */
  while (next_page <= alloc_region->last_page) {
    ASSERT(page_table[next_page].bytes_used == 0);
    page_table[next_page].allocated = FREE_PAGE_FLAG;
    next_page++;
  }
  Sg_UnlockMutex(&free_pages_lock);

  /* alloc_region is per-thread, we're ok to do this unlocked */
  gc_set_region_empty(alloc_region);
}

/* Allocate a possibly large object. */
void * gc_alloc_large(size_t reqbytes, int page_type_flag,
		      alloc_region_t *alloc_region)
{
  int more;
  page_index_t first_page, next_page, last_page;
  page_bytes_t orig_first_page_bytes_used;
  size_t byte_cnt, bytes_used, nbytes;
  block_t *block;

  Sg_LockMutex(&free_pages_lock);

  nbytes = sizeof(memory_header_t) + reqbytes;

  first_page = generation_alloc_start_page(gc_alloc_generation,
					   page_type_flag, TRUE);
  if (first_page <= alloc_region->last_page) {
    first_page = alloc_region->last_page+1;
  }

  last_page = gc_find_freeish_pages(&first_page, nbytes, page_type_flag);

  ASSERT(first_page > alloc_region->last_page);

  set_generation_alloc_start_page(gc_alloc_generation, page_type_flag, 1,
				  last_page);

  /* Set up the pages. */
  orig_first_page_bytes_used = page_table[first_page].bytes_used;

  /* If the first page was free then set up the gen, and
   * region_start_offset. */
  if (page_table[first_page].bytes_used == 0) {
    page_table[first_page].allocated = page_type_flag;
    page_table[first_page].gen = gc_alloc_generation;
    page_table[first_page].region_start_offset = 0;
    page_table[first_page].large_object = 1;
  }

  ASSERT(page_table[first_page].allocated == page_type_flag);
  ASSERT(page_table[first_page].gen == gc_alloc_generation);
  ASSERT(page_table[first_page].large_object == 1);

  byte_cnt = 0;

  /* Calc. the number of bytes used in this page. This is not
   * always the number of new bytes, unless it was free. */
  more = 0;
  if ((bytes_used = nbytes + orig_first_page_bytes_used) > CARD_BYTES) {
    bytes_used = CARD_BYTES;
    more = 1;
  }
  page_table[first_page].bytes_used = bytes_used;
  byte_cnt += bytes_used;

  next_page = first_page+1;

  /* All the rest of the pages should be free. We need to set their
   * region_start_offset pointer to the start of the region, and set
   * the bytes_used. */
  while (more) {
    ASSERT(page_free_p(next_page));
    ASSERT(page_table[next_page].bytes_used == 0);
    page_table[next_page].allocated = page_type_flag;
    page_table[next_page].gen = gc_alloc_generation;
    page_table[next_page].large_object = 1;

    page_table[next_page].region_start_offset =
      npage_bytes(next_page-first_page) - orig_first_page_bytes_used;

    /* Calculate the number of bytes used in this page. */
    more = 0;
    bytes_used=(nbytes + orig_first_page_bytes_used) - byte_cnt;
    if (bytes_used > CARD_BYTES) {
      bytes_used = CARD_BYTES;
      more = 1;
    }
    page_table[next_page].bytes_used = bytes_used;
    page_table[next_page].write_protected=0;
    page_table[next_page].dont_move=0;
    byte_cnt += bytes_used;
    next_page++;
  }

  ASSERT((byte_cnt - orig_first_page_bytes_used) == nbytes);

  bytes_allocated += nbytes;
  generations[gc_alloc_generation].bytes_allocated += nbytes;

  /* Add the region to the new_areas if requested. */
  if (BOXED_PAGE_FLAG & page_type_flag)
    add_new_area(first_page, orig_first_page_bytes_used, nbytes);

  /* Bump up last_free_page */
  if (last_page+1 > last_free_page) {
    last_free_page = last_page+1;
    /* set_alloc_pointer((lispobj)(page_address(last_free_page))); */
  }
  Sg_UnlockMutex(&free_pages_lock);

/* #ifdef READ_PROTECT_FREE_PAGES */
/*   os_protect(page_address(first_page), */
/* 	     npage_bytes(1+last_page-first_page), */
/* 	     OS_VM_PROT_ALL); */
/* #endif */

  zero_dirty_pages(first_page, last_page);

  block = (block_t *)page_address(first_page);
  SET_MEMORY_SIZE(block, reqbytes);
  return (void *)&block->body;
}

static page_index_t gencgc_alloc_start_page = -1;

static GC_oom_func oom_handler = NULL;
void GC_set_oom_fn(GC_oom_func handler)
{
  oom_handler = handler;
}

void gc_heap_exhausted_error(intptr_t available, size_t requested)
{
  /* dump the heap information */
  report_heap_exhaustion(available, requested);
  if (oom_handler) oom_handler(requested);
  /* we don't do anything but exit */
  Sg_Panic("Heap exhausted, game over.");
}


page_index_t
gc_find_freeish_pages(page_index_t *restart_page_ptr, size_t bytes,
                      int page_type_flag)
{
  page_index_t most_bytes_found_from = 0, most_bytes_found_to = 0;
  page_index_t first_page, last_page, restart_page = *restart_page_ptr;
  size_t nbytes = bytes, nbytes_goal = nbytes;
  size_t bytes_found = 0;
  size_t most_bytes_found = 0;
  int small_object = nbytes < CARD_BYTES;

  if (nbytes_goal < gencgc_alloc_granularity)
    nbytes_goal = gencgc_alloc_granularity;

  /* Toggled by gc_and_save for heap compaction, normally -1. */
  if (gencgc_alloc_start_page != -1) {
    restart_page = gencgc_alloc_start_page;
  }

  /* FIXME: This is on bytes instead of nbytes pending cleanup of
   * long from the interface. */
  ASSERT(bytes>=0);
  /* Search for a page with at least nbytes of space. We prefer
   * not to split small objects on multiple pages, to reduce the
   * number of contiguous allocation regions spaning multiple
   * pages: this helps avoid excessive conservativism.
   *
   * For other objects, we guarantee that they start on their own
   * page boundary.
   */
  first_page = restart_page;
  while (first_page < page_table_pages) {
    bytes_found = 0;
    if (page_free_p(first_page)) {
      ASSERT(0 == page_table[first_page].bytes_used);
      bytes_found = CARD_BYTES;
    } else if (small_object &&
	       (page_table[first_page].allocated == page_type_flag) &&
	       (page_table[first_page].large_object == 0) &&
	       (page_table[first_page].gen == gc_alloc_generation) &&
	       (page_table[first_page].write_protected == 0) &&
	       (page_table[first_page].dont_move == 0)) {
      bytes_found = CARD_BYTES - page_table[first_page].bytes_used;
      if (bytes_found < nbytes) {
	if (bytes_found > most_bytes_found)
	  most_bytes_found = bytes_found;
	first_page++;
	continue;
      }
    } else {
      first_page++;
      continue;
    }

    ASSERT(page_table[first_page].write_protected == 0);
    for (last_page = first_page+1;
	 ((last_page < page_table_pages) &&
	  page_free_p(last_page) &&
	  (bytes_found < nbytes_goal));
	 last_page++) {
      bytes_found += CARD_BYTES;
      ASSERT(0 == page_table[last_page].bytes_used);
      ASSERT(0 == page_table[last_page].write_protected);
    }

    if (bytes_found > most_bytes_found) {
      most_bytes_found = bytes_found;
      most_bytes_found_from = first_page;
      most_bytes_found_to = last_page;
    }
    if (bytes_found >= nbytes_goal)
      break;

    first_page = last_page;
  }

  bytes_found = most_bytes_found;
  restart_page = first_page + 1;

  /* Check for a failure */
  if (bytes_found < nbytes) {
    ASSERT(restart_page >= page_table_pages);
    gc_heap_exhausted_error(most_bytes_found, nbytes);
  }

  ASSERT(most_bytes_found_to);
  *restart_page_ptr = most_bytes_found_from;
  return most_bytes_found_to-1;
}


/* Allocate bytes.  All the rest of the special-purpose allocation
 * functions will eventually call this  */

static void *
gc_alloc_with_region(size_t reqbytes,int page_type_flag, 
		     alloc_region_t *my_region, int quick_p)
{
  void *new_free_pointer;
  size_t nbytes;
  if (reqbytes>=large_object_size)
    return gc_alloc_large(reqbytes, page_type_flag, my_region);

  nbytes = sizeof(memory_header_t) + reqbytes;
  /* Check whether there is room in the current alloc region. */
  new_free_pointer = my_region->free_pointer + nbytes;
  if (new_free_pointer <= my_region->end_addr) {
    /* If so then allocate from the current alloc region. */
    block_t *new_obj = (block_t *)my_region->free_pointer;
    my_region->free_pointer = new_free_pointer;

    /* Unless a `quick' alloc was requested, check whether the
       alloc region is almost empty. */
    if (!quick_p &&
	void_diff(my_region->end_addr,my_region->free_pointer) <= 32) {
      /* If so, finished with the current region. */
      gc_alloc_update_page_tables(page_type_flag, my_region);
      /* Set up a new region. */
      gc_alloc_new_region(32 /*bytes*/, page_type_flag, my_region);
    }
    SET_MEMORY_SIZE(new_obj, reqbytes);
    return (void *)&new_obj->body;
  }

  /* Else not enough free space in the current region: retry with a
   * new region. */

  gc_alloc_update_page_tables(page_type_flag, my_region);
  gc_alloc_new_region(nbytes, page_type_flag, my_region);
  return gc_alloc_with_region(nbytes, page_type_flag, my_region,0);
}

static inline void *
gc_general_alloc(size_t nbytes, int page_type_flag, int quick_p)
{
  alloc_region_t *my_region;
  if (UNBOXED_PAGE_FLAG == page_type_flag) {
    my_region = &unboxed_region;
  } else if (BOXED_PAGE_FLAG & page_type_flag) {
    my_region = &boxed_region;
  } else {
    Sg_Panic("bad page type flag: %d", page_type_flag);
  }
  return gc_alloc_with_region(nbytes, page_type_flag, my_region, quick_p);
}


/* these are only used during GC: all allocation from the mutator calls
 * alloc() -> gc_alloc_with_region() with the appropriate per-thread
 * region */
static inline void * gc_quick_alloc(size_t nbytes)
{
  return gc_general_alloc(nbytes, BOXED_PAGE_FLAG, ALLOC_QUICK);
}


static inline void * gc_alloc_unboxed(size_t nbytes)
{
  return gc_general_alloc(nbytes, UNBOXED_PAGE_FLAG, FALSE);
}

static inline void * gc_quick_alloc_unboxed(size_t nbytes)
{
  return gc_general_alloc(nbytes, UNBOXED_PAGE_FLAG, ALLOC_QUICK);
}

enum object_type_t {
  NON_PRECISE = 0,
  BLOCK,
  OBJECT
};

static void * search_dynamic_space(void *pointer, enum object_type_t type);

static void * possibly_valid_dynamic_space_pointer(void *p)
{
  return search_dynamic_space(p, NON_PRECISE);
}


static inline void *
gc_general_copy_object(void *object, size_t nbytes, int page_type_flag)
{
  void *newobj;
  block_t *block;
  /* TODO check */
  ASSERT(possibly_valid_dynamic_space_pointer(object));
  block = POINTER2BLOCK(object);
  /* given object might not be in from space */
  /* ASSERT(from_space_p(object)); */
  /* ASSERT((nwords & 0x01) == 0); */
  /* Allocate space. */
  newobj = gc_general_alloc(nbytes, page_type_flag, ALLOC_QUICK);
  /* Copy the object.
     If the object contains a class as its header, however
     it will be copied anyway. So I *hope* it won't be a problem. */
  memcpy(newobj, object, nbytes);
  /* copy finalizer if there is */
  if (MEMORY_FINALIZER(block)) {
    block_t *new_block = POINTER2BLOCK(newobj);
    MEMORY_FINALIZER(new_block) = MEMORY_FINALIZER(block);
  }
  return newobj;
}

/* Copy a large object. If the object is in a large object region then
 * it is simply promoted, else it is copied. If it's large enough then
 * it's copied to a large object region.
 *
 * Bignums and vectors may have shrunk. If the object is not copied
 * the space needs to be reclaimed, and the page_tables corrected. */
static void *
general_copy_large_object(void *object, size_t nbytes, int boxedp)
{
  page_index_t first_page;
  block_t *block = POINTER2BLOCK(object);
  ASSERT(possibly_valid_dynamic_space_pointer(object));
  /* TODO how to check? */
  /* ASSERT(is_lisp_pointer(object)); */
  /* ASSERT(from_space_p(object)); */
  /* ASSERT((nwords & 0x01) == 0); */

  /* Check whether it's a large object. */
  first_page = find_page_index((void *)object);
  if (first_page < 0 || page_table[first_page].dont_move) {
    /* either not on dynamic space or can't move so just return */
    return object;
  }

  if (page_table[first_page].large_object) {
    /* Promote the object. Note: Unboxed objects may have been
     * allocated to a BOXED region so it may be necessary to
     * change the region to UNBOXED. */
    size_t remaining_bytes;
    size_t bytes_freed;
    page_index_t next_page;
    page_bytes_t old_bytes_used;

    /* FIXME: This comment is somewhat stale.
     *
     * Note: Any page write-protection must be removed, else a
     * later scavenge_newspace may incorrectly not scavenge these
     * pages. This would not be necessary if they are added to the
     * new areas, but let's do it for them all (they'll probably
     * be written anyway?). */

    ASSERT(page_table[first_page].region_start_offset == 0);
    next_page = first_page;
    nbytes += sizeof(memory_header_t); /* add offset bytes */
    remaining_bytes = nbytes; /* nwords * N_WORD_BYTES; */

    while (remaining_bytes > CARD_BYTES) {
      ASSERT(page_table[next_page].gen == from_space);
      ASSERT(page_table[next_page].large_object);
      ASSERT(page_table[next_page].region_start_offset ==
		npage_bytes(next_page-first_page));
      ASSERT(page_table[next_page].bytes_used == CARD_BYTES);
      /* Should have been unprotected by unprotect_oldspace()
       * for boxed objects, and after promotion unboxed ones
       * should not be on protected pages at all. */
      ASSERT(!page_table[next_page].write_protected);

      if (boxedp) {
	ASSERT(page_boxed_p(next_page));
      } else {
	ASSERT(page_allocated_no_region_p(next_page));
	page_table[next_page].allocated = UNBOXED_PAGE_FLAG;
      }
      page_table[next_page].gen = new_space;

      remaining_bytes -= CARD_BYTES;
      next_page++;
    }

    /* Now only one page remains, but the object may have shrunk so
     * there may be more unused pages which will be freed. */

    /* Object may have shrunk but shouldn't have grown - check. */
    ASSERT(page_table[next_page].bytes_used >= remaining_bytes);

    page_table[next_page].gen = new_space;

    if (boxedp) {
      ASSERT(page_boxed_p(next_page));
    } else {
      page_table[next_page].allocated = UNBOXED_PAGE_FLAG;
    }

    /* Adjust the bytes_used. */
    old_bytes_used = page_table[next_page].bytes_used;
    page_table[next_page].bytes_used = remaining_bytes;

    bytes_freed = old_bytes_used - remaining_bytes;

    /* Free any remaining pages; needs care. */
    next_page++;
    while ((old_bytes_used == CARD_BYTES) &&
	   (page_table[next_page].gen == from_space) &&
	   /* FIXME: It is not obvious to me why this is necessary
	    * as a loop condition: it seems to me that the
	    * region_start_offset test should be sufficient, but
	    * experimentally that is not the case. --NS
	    * 2011-11-28 */
	   (boxedp ?
	    page_boxed_p(next_page) :
	    page_allocated_no_region_p(next_page)) &&
	   page_table[next_page].large_object &&
	   (page_table[next_page].region_start_offset ==
	    npage_bytes(next_page - first_page))) {
      /* Checks out OK, free the page. Don't need to both zeroing
       * pages as this should have been done before shrinking the
       * object. These pages shouldn't be write-protected, even if
       * boxed they should be zero filled. */
      ASSERT(page_table[next_page].write_protected == 0);

      old_bytes_used = page_table[next_page].bytes_used;
      page_table[next_page].allocated = FREE_PAGE_FLAG;
      page_table[next_page].bytes_used = 0;
      bytes_freed += old_bytes_used;
      next_page++;
    }

    generations[from_space].bytes_allocated -= nbytes + bytes_freed;
    generations[new_space].bytes_allocated += nbytes;
    bytes_allocated -= bytes_freed;

    /* Add the region to the new_areas if requested. */
    if (boxedp)
      add_new_area(first_page, 0, nbytes);

    return object;

  } else {
    /* it's the same ... */
    return gc_general_copy_object(object, nbytes,
				  (boxedp
				   ? BOXED_PAGE_FLAG
				   : UNBOXED_PAGE_FLAG));
  }
}

/* Following procedures assume given object is on dynamic space */
void * copy_large_object(void *object)
{
  block_t *block = POINTER2BLOCK(object);
  return general_copy_large_object(object, MEMORY_SIZE(block), TRUE);
}

void * copy_large_unboxed_object(void *object)
{
  block_t *block = POINTER2BLOCK(object);
  return general_copy_large_object(object, MEMORY_SIZE(block), FALSE);
}

/* to copy unboxed objects */
void * copy_object(void *object)
{
  block_t *block = POINTER2BLOCK(object);
  return gc_general_copy_object(object, MEMORY_SIZE(block),
				BOXED_PAGE_FLAG);
}


/* to copy unboxed objects */
void * copy_unboxed_object(void *object)
{
  block_t *block = POINTER2BLOCK(object);
  return gc_general_copy_object(object, MEMORY_SIZE(block),
				UNBOXED_PAGE_FLAG);
}

static int blockable_p(void *pointer)
{
  return ((uintptr_t)dynamic_space_start+sizeof(memory_header_t)
	  <= (uintptr_t)pointer &&
	  pointer <= dynamic_space_end);
}
/* Assume start is the page boundary */
void * gc_search_space(void *start, intptr_t bytes, void *pointer, 
		       enum object_type_t type)
{
  while (bytes > 0) {
    block_t *thing = (block_t *)start;
    intptr_t count;
    void *boundary;
    count = MEMORY_SIZE(thing);
    boundary = (void *)((uintptr_t)start + count + sizeof(memory_header_t));
    /* Check whether the pointer is within this object. */
    switch (type) {
    case BLOCK:
      /* check with boundary */
      if (pointer == thing) return start;
      break;
    case OBJECT:
      if (pointer == ALLOCATED_POINTER(thing)) return start;
      break;
    default:
      /* check in range */
      if (pointer >= start && pointer < boundary) return start;
      break;
    }
    start = boundary;
    bytes -= count + sizeof(memory_header_t) ;
  }
  return NULL;
}


/* a faster version for searching the dynamic space. This will work even
 * if the object is in a current allocation region. */
void * search_dynamic_space(void *pointer, enum object_type_t type)
{
  page_index_t page_index = find_page_index(pointer);
  void *start;

  /* The address may be invalid, so do some checks. */
  if ((page_index == -1) || page_free_p(page_index))
    return NULL;
  start = page_region_start(page_index);
  /* let give all pointer some chance to be checked. is one word enough? */
  return gc_search_space(start, (pointer+1) - start, pointer, type);
}

/* we don't check if the pointer is strictly there or not.
   all we want is actually *accessible* address
 */
static int possibly_valid_static_area_pointer(void *pointer)
{
  int i;
  for (i = 0; i < current_static_space_index; i++) {
    void *start = static_spaces[i].start;
    void *end = static_spaces[i].end;
    if (start < pointer && pointer < end) return TRUE;
  }
  return FALSE;
}

static inline void *
general_alloc_internal(size_t reqbytes, int page_type_flag,
		       alloc_region_t *region, SgVM *vm)
{
  block_t *new_block;
  void *new_free_pointer;
  size_t trigger_bytes = 0, nbytes;

  nbytes = sizeof(memory_header_t) + reqbytes;
  /* never be 0 */
  /* ASSERT(nbytes > 0); */
  ASSERT((((uintptr_t)region->free_pointer & LOWTAG_MASK) == 0)
	 && ((nbytes & LOWTAG_MASK) == 0));

  if (nbytes > large_allocation) large_allocation = nbytes;
  /* maybe we can do this quickly ... */
  new_free_pointer = region->free_pointer + nbytes;
  if (new_free_pointer <= region->end_addr) {
    new_block = (block_t *)(region->free_pointer);
    region->free_pointer = new_free_pointer;
    SET_MEMORY_SIZE(new_block, reqbytes);
    return ALLOCATED_POINTER(new_block);        /* yup */
  }

  /* We don't want to count nbytes against auto_gc_trigger unless we
   * have to: it speeds up the tenuring of objects and slows down
   * allocation. However, unless we do so when allocating _very_
   * large objects we are in danger of exhausting the heap without
   * running sufficient GCs.
   */
  if (nbytes >= bytes_consed_between_gcs) trigger_bytes = nbytes;
  
  /* we have to go the long way around, it seems. Check whether we
   * should GC in the near future
   */
  if (auto_gc_trigger && (bytes_allocated+trigger_bytes > auto_gc_trigger)) {
    /* well if we need gc then we need gc anytime, so just set the flag. */
    need_gc = TRUE;    
  }
  return gc_alloc_with_region(reqbytes, page_type_flag, region, 0);
}

void * general_alloc(size_t nbytes, int page_type_flag)
{
  SgVM *vm = Sg_VM();
  if (BOXED_PAGE_FLAG & page_type_flag) {
    /* The initialisation time doesn't have VM yet, so check if the
       VM is already there. 
       TODO after VM initialisation we must have region there
       remove the check.
    */
    GC_thread_context_t *context = vm ? &vm->context : NULL;
    alloc_region_t *region = context ? &context->alloc_region : &boxed_region;
    return general_alloc_internal(nbytes, page_type_flag, region, vm);
  } else if (UNBOXED_PAGE_FLAG & page_type_flag) {
    void *obj;
    Sg_LockMutex(&allocation_lock);
    obj = general_alloc_internal(nbytes, page_type_flag, &unboxed_region, vm);
    Sg_UnlockMutex(&allocation_lock);
    return obj;
  } else {
    Sg_Panic("bad page type flag: %d", page_type_flag);
    return NULL;
  }
}

/* Update last_free_page, then SymbolValue(ALLOCATION_POINTER). */
static intptr_t update_dynamic_space_free_pointer(void)
{
  page_index_t last_page = -1, i;

  for (i = 0; i < last_free_page; i++)
    if (page_allocated_p(i) && (page_table[i].bytes_used != 0))
      last_page = i;

  last_free_page = last_page+1;

  /* set_alloc_pointer((lispobj)(page_address(last_free_page))); */
  return 0; /* dummy value: return something ... */
}

static void gc_alloc_update_all_page_tables(void)
{
  /* Flush the alloc regions updating the tables. */
  GC_thread_context_t *th;
  for_each_thread(th) {
    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, 
				(alloc_region_t *)&th->alloc_region);
  }
  gc_alloc_update_page_tables(UNBOXED_PAGE_FLAG, &unboxed_region);
  gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &boxed_region);
}


/* Write-protect all the dynamic boxed pages in the given generation. */
static void write_protect_generation_pages(generation_index_t generation)
{
  page_index_t start;

  ASSERT(generation < SCRATCH_GENERATION);

  for (start = 0; start < last_free_page; start++) {
    if (protect_page_p(start, generation)) {
      void *page_start;
      page_index_t last;

      /* Note the page as protected in the page tables. */
      page_table[start].write_protected = 1;

      for (last = start + 1; last < last_free_page; last++) {
	if (!protect_page_p(last, generation))
	  break;
	page_table[last].write_protected = 1;
      }

      page_start = (void *)page_address(start);

      os_protect(page_start,
		 npage_bytes(last - start),
		 OS_VM_PROT_READ | OS_VM_PROT_EXECUTE);

      start = last;
    }
  }
}

static void remap_page_range(page_index_t from, page_index_t to)
{
  /* There's a mysterious Solaris/x86 problem with using mmap
   * tricks for memory zeroing. See sbcl-devel thread
   * "Re: patch: standalone executable redux".
   */
  const page_index_t
    release_granularity = gencgc_release_granularity/CARD_BYTES,
	   release_mask = release_granularity-1,
		    end = to+1,
	   aligned_from = (from+release_mask)&~release_mask,
	    aligned_end = (end&~release_mask);

  if (aligned_from < aligned_end) {
    zero_pages_with_mmap(aligned_from, aligned_end-1);
    if (aligned_from != from)
      zero_and_mark_pages(from, aligned_from-1);
    if (aligned_end != end)
      zero_and_mark_pages(aligned_end, end-1);
  } else {
    zero_and_mark_pages(from, to);
  }
}


static void remap_free_pages(page_index_t from, page_index_t to, int forcibly)
{
  page_index_t first_page, last_page;

  if (forcibly)
    return remap_page_range(from, to);

  for (first_page = from; first_page <= to; first_page++) {
    if (page_allocated_p(first_page) ||
	(page_table[first_page].need_to_zero == 0))
      continue;

    last_page = first_page + 1;
    while (page_free_p(last_page) &&
	   (last_page <= to) &&
	   (page_table[last_page].need_to_zero == 1))
      last_page++;

    remap_page_range(first_page, last_page-1);

    first_page = last_page;
  }
}

/* Un-write-protect all the pages in from_space. This is done at the
 * start of a GC else there may be many page faults while scavenging
 * the newspace (I've seen drive the system time to 99%). These pages
 * would need to be unprotected anyway before unmapping in
 * free_oldspace; not sure what effect this has on paging.. */
static void unprotect_oldspace()
{
  page_index_t i;
  void *region_addr = NULL;
  void *page_addr = NULL;
  uintptr_t region_bytes = 0;

  for (i = 0; i < last_free_page; i++) {
    if (page_allocated_p(i)
	&& (page_table[i].bytes_used != 0)
	&& (page_table[i].gen == from_space)) {

      /* Remove any write-protection. We should be able to rely
       * on the write-protect flag to avoid redundant calls. */
      if (page_table[i].write_protected) {
	page_table[i].write_protected = 0;
	page_addr = page_address(i);
	if (!region_addr) {
	  /* First region. */
	  region_addr = page_addr;
	  region_bytes = CARD_BYTES;
	} else if (region_addr + region_bytes == page_addr) {
	  /* Region continue. */
	  region_bytes += CARD_BYTES;
	} else {
	  /* Unprotect previous region. */
	  os_protect(region_addr, region_bytes, OS_VM_PROT_ALL);
	  /* First page in new region. */
	  region_addr = page_addr;
	  region_bytes = CARD_BYTES;
	}
      }
    }
  }
  if (region_addr) {
    /* Unprotect last region. */
    os_protect(region_addr, region_bytes, OS_VM_PROT_ALL);
  }
}

/* FIXME This implementation sucks! */
static int is_scheme_pointer(void *p)
{
  /* TODO check if the given pointer is in static areas */
  if (SG_HPTRP(p) &&
      (possibly_valid_static_area_pointer(p) ||
       possibly_valid_dynamic_space_pointer(p)) &&
      SG_HOBJP(p)) {
    /* at least it's safe to access to the class pointer */
    SgClass *clazz = SG_CLASS_OF(p);
    if (possibly_valid_static_area_pointer(clazz)) {
      return clazz->magic == CLASS_MAGIC_VALUE;
    } else {
      return FALSE;
    }
  } else {
    return FALSE;
  }
}

/* this will be used in the included file so put forward declaration
   here */
static void scavenge(intptr_t *start, intptr_t n_words);
static void preserve_pointer(void *addr);
/* we need to salvage weak pointers *AFTER* scavenge, so this is the
   save pointer for it. */
static inline int forwarding_pointer_p(void *p)
{
  block_t *block = POINTER2BLOCK(p);
  ASSERT(possibly_valid_dynamic_space_pointer(block));
  return MEMORY_FORWARDED(block);
}

static inline int survived_gc_yet(void *p)
{
  return (!is_scheme_pointer(p) || !from_space_p(p) ||
	  forwarding_pointer_p(p));
}

static void *weak_hashtables = NULL;
#include "scavenge_dispatcher.c"

/* 
   Assuming the following memory structure

 start
   +--------+------------+
   | header | body       | ;; a block_t
   +--------+------------+
   | header | body       |
   +--------+------------+
            :
   +--------+------------+
   | header | body       |
   +--------+------------+< end

   If there is no forwarding pointer, then header contains requested
   object size, however if the pointer must forward then it should
   contain only word size.

   NOTE: alignment might not be this much aligned.
 */
void scavenge(intptr_t *start, intptr_t n_words)
{
  intptr_t *end = start + n_words;
  intptr_t *object_ptr;
  size_t n_bytes_scavenged;
  /* ASSUME 'start' indicates the proper memory structure */
  for (object_ptr = start;
       object_ptr < end;
       object_ptr = 
	 (intptr_t *)((uintptr_t)object_ptr + 
		      n_bytes_scavenged + sizeof(memory_header_t))) {
    /* the real ptr indicates real container of the objects which
       we might be interested in. */
    intptr_t *real_ptr = ALLOCATED_POINTER(object_ptr);
    /* indicating container can know exact bytes allocated.
       now we need to check inside of the object_ptr to move. */
    size_t limit = MEMORY_SIZE(object_ptr);
    /* check first */
    if (MEMORY_FORWARDED(object_ptr))
      Sg_Panic("unexpect forwarding pointer in scavenge: %p, start=%p, n=%ld\n",
    	       object_ptr, start, n_words);
    /* sort of check here.
       FIXME: this should not be here. */
    if (is_scheme_pointer(real_ptr) && SG_HASHTABLE_P(real_ptr)) {
      if (SG_HASHTABLE(real_ptr)->type == SG_HASH_EQ) {
	SG_HASHTABLE_CORE(real_ptr)->rehashNeeded = TRUE;
      }
    }

    n_bytes_scavenged = 0;
    /* object == body so get body from block */
    while (n_bytes_scavenged < limit) {
      intptr_t object = *real_ptr;
      block_t *block;
      size_t block_size;

      block = POINTER2BLOCK(object);
      /* both block and object need to be in dynamic space
	 even though block is diff of object however it
	 can be some extra check. */
      if (search_dynamic_space(block, BLOCK) &&
	  search_dynamic_space((void *)object, OBJECT)) {
	block_size = MEMORY_SIZE(block);
	if (from_space_p((void *)object)) {
	  if (MEMORY_FORWARDED(block)) {
	    /* Yes, there's a forwarding pointer. */
	    /* to keep the memory structure forward with header  */
	    *(void **)real_ptr = MEMORY_FORWARDED_VALUE(block);
	  } else {
	    if (is_scheme_pointer((void *)object)) {
	      /* Scavenge that pointer. */
	      dispatch_pointer((void **)real_ptr, (void *)object);
	    } else {
	      /* extra sanity check*/
	      page_index_t index = find_page_index(block);
	      
	      /* if the size is indicating more than large_object_size but
		 page said it's not a large object, that's weird enough
		 to scavenge. */
	      if (index < 0 ||
		  (block_size >= large_object_size &&
		   !page_table[index].large_object) ||
		  /* other way around */
		  (block_size < large_object_size &&
		   page_table[index].large_object))
		goto not_scavenge;
	      /* the block body must be allocated on the 8 byte boundary
		 means, size must be unit of N_WORD_BYTES. */
	      if (MEMORY_SIZE(block) % N_WORD_BYTES) {
		goto not_scavenge;
	      }
	      /* large object must be in the start region address */
	      if (page_table[index].large_object &&
		  page_table[index].region_start_offset != 0) {
		goto not_scavenge;
	      }
	      if (!search_dynamic_space((void *)object, OBJECT))
		goto not_scavenge;
	      /* can we do this now? */
	      /* Scavenge that pointer. */
	      scavenge_general_pointer((void **)real_ptr, (void *)object);
	    }
	  }
	}
#if 0
      } else {
	/* OK do it one by one */
	real_ptr = (intptr_t *)((uintptr_t)real_ptr + 1);
	n_bytes_scavenged++;
#endif
      }
    not_scavenge:
      real_ptr++;
      n_bytes_scavenged += N_WORD_BYTES;
    }
  }
}

/* comment from SBCL */
/* If the given page is not write-protected, then scan it for pointers
 * to younger generations or the top temp. generation, if no
 * suspicious pointers are found then the page is write-protected.
 *
 * Care is taken to check for pointers to the current gc_alloc()
 * region if it is a younger generation or the temp. generation. This
 * frees the caller from doing a gc_alloc_update_page_tables(). Actually
 * the gc_alloc_generation does not need to be checked as this is only
 * called from scavenge_generation() when the gc_alloc generation is
 * younger, so it just checks if there is a pointer to the current
 * region.
 *
 * We return 1 if the page was write-protected, else 0. */
static int update_page_write_prot(page_index_t page)
{
  generation_index_t gen = page_table[page].gen;
  int wp_it = 1;
  void **page_addr = (void **)page_address(page);
  intptr_t num_words = page_table[page].bytes_used / N_WORD_BYTES, j;

  /* Shouldn't be a free page. */
  ASSERT(page_allocated_p(page));
  ASSERT(page_table[page].bytes_used != 0);

  /* Skip if it's already write-protected, pinned, or unboxed */
  if (page_table[page].write_protected
      /* FIXME: What's the reason for not write-protecting pinned pages? */
      || page_table[page].dont_move
      || page_unboxed_p(page))
    return (0);

  /* Scan the page for pointers to younger generations or the
   * top temp. generation. */

  for (j = 0; j < num_words; j++) {
    void *ptr = *(page_addr+j);
    page_index_t index = find_page_index(ptr);

    /* Check that it's in the dynamic space */
    if (index != -1)
      if (/* Does it point to a younger or the temp. generation? */
	  (page_allocated_p(index)
	   && (page_table[index].bytes_used != 0)
	   && ((page_table[index].gen < gen)
	       || (page_table[index].gen == SCRATCH_GENERATION)))

	  /* Or does it point within a current gc_alloc() region? */
	  || ((boxed_region.start_addr <= ptr)
	      && (ptr <= boxed_region.free_pointer))
	  || ((unboxed_region.start_addr <= ptr)
	      && (ptr <= unboxed_region.free_pointer))) {
	wp_it = 0;
	break;
      }
  }

  if (wp_it == 1) {
    /* Write-protect the page. */
    /*FSHOW((stderr, "/write-protecting page %d gen %d\n", page, gen));*/

    os_protect((void *)page_addr,
	       CARD_BYTES, OS_VM_PROT_READ|OS_VM_PROT_EXECUTE);

    /* Note the page as protected in the page tables. */
    page_table[page].write_protected = 1;
  }

  return wp_it;
}

/* Scavenge all generations from FROM to TO, inclusive, except for
 * new_space which needs special handling, as new objects may be
 * added which are not checked here - use scavenge_newspace generation.
 *
 * Write-protected pages should not have any pointers to the
 * from_space so do need scavenging; thus write-protected pages are
 * not always scavenged. There is some code to check that these pages
 * are not written; but to check fully the write-protected pages need
 * to be scavenged by disabling the code to skip them.
 *
 * Under the current scheme when a generation is GCed the younger
 * generations will be empty. So, when a generation is being GCed it
 * is only necessary to scavenge the older generations for pointers
 * not the younger. So a page that does not have pointers to younger
 * generations does not need to be scavenged.
 *
 * The write-protection can be used to note pages that don't have
 * pointers to younger pages. But pages can be written without having
 * pointers to younger generations. After the pages are scavenged here
 * they can be scanned for pointers to younger generations and if
 * there are none the page can be write-protected.
 *
 * One complication is when the newspace is the top temp. generation.
 */
static void scavenge_generations(generation_index_t from, generation_index_t to)
{
  page_index_t i;
  page_index_t num_wp = 0;

  for (i = 0; i < last_free_page; i++) {
    generation_index_t generation = page_table[i].gen;
    if (page_boxed_p(i)
	&& (page_table[i].bytes_used != 0)
	&& (generation != new_space)
	&& (generation >= from)
	&& (generation <= to)) {
      page_index_t last_page, j;
      int write_protected = 1;

      /* This should be the start of a region */
      ASSERT(page_table[i].region_start_offset == 0);

      /* Now work forward until the end of the region */
      for (last_page = i; ; last_page++) {
	write_protected =
	  write_protected && page_table[last_page].write_protected;
	if ((page_table[last_page].bytes_used < CARD_BYTES)
	    /* Or it is CARD_BYTES and is the last in the block */
	    || (!page_boxed_p(last_page+1))
	    || (page_table[last_page+1].bytes_used == 0)
	    || (page_table[last_page+1].gen != generation)
	    || (page_table[last_page+1].region_start_offset == 0))
	  break;
      }
      if (!write_protected) {
	scavenge(page_address(i),
		 ((uintptr_t)(page_table[last_page].bytes_used
			      + npage_bytes(last_page-i)))
		 /N_WORD_BYTES);

	/* Now scan the pages and write protect those that
	 * don't have pointers to younger generations. */
	if (enable_page_protection) {
	  for (j = i; j <= last_page; j++) {
	    num_wp += update_page_write_prot(j);
	  }
	}
	/* FSHOW((stderr, */
	/*        "/write protected %d pages within generation %d\n", */
	/*        num_wp, generation)); */
      }
      i = last_page;
    }
  }
}

static struct new_area new_areas_1[NUM_NEW_AREAS];
static struct new_area new_areas_2[NUM_NEW_AREAS];

/* Do one full scan of the new space generation. This is not enough to
 * complete the job as new objects may be added to the generation in
 * the process which are not scavenged. */
static void scavenge_newspace_generation_one_scan(generation_index_t generation)
{
  page_index_t i;

  /* FSHOW((stderr, */
  /* 	 "/starting one full scan of newspace generation %d:%d\n", */
  /* 	 generation, last_free_page)); */
  for (i = 0; i < last_free_page; i++) {
    /* Note that this skips over open regions when it encounters them. */
    if (page_boxed_p(i)
	&& (page_table[i].bytes_used != 0)
	&& (page_table[i].gen == generation)
	&& ((page_table[i].write_protected == 0)
	    /* (This may be redundant as write_protected is now
	     * cleared before promotion.) */
	    || (page_table[i].dont_move == 1))) {
      page_index_t last_page;
      int all_wp=1;

      /* The scavenge will start at the region_start_offset of
       * page i.
       *
       * We need to find the full extent of this contiguous
       * block in case objects span pages.
       *
       * Now work forward until the end of this contiguous area
       * is found. A small area is preferred as there is a
       * better chance of its pages being write-protected. */
      for (last_page = i; ;last_page++) {
	/* If all pages are write-protected and movable,
	 * then no need to scavenge */
	all_wp = all_wp && page_table[last_page].write_protected &&
	  !page_table[last_page].dont_move;

	/* Check whether this is the last page in this
	 * contiguous block */
	if ((page_table[last_page].bytes_used < CARD_BYTES)
	    /* Or it is CARD_BYTES and is the last in the block */
	    || (!page_boxed_p(last_page+1))
	    || (page_table[last_page+1].bytes_used == 0)
	    || (page_table[last_page+1].gen != generation)
	    || (page_table[last_page+1].region_start_offset == 0))
	  break;
      }

      /* Do a limited check for write-protected pages.  */
      if (!all_wp) {
	intptr_t nwords = (((uintptr_t)
			    (page_table[last_page].bytes_used
			     + npage_bytes(last_page-i)
			     + page_table[i].region_start_offset))
			   / N_WORD_BYTES);
	new_areas_ignore_page = last_page;

	/* FSHOW((stderr, "scavenging region(%d) %d words\n", i, nwords)); */
	scavenge(page_region_start(i), nwords);

      }
      i = last_page;
    }
  }
  /* FSHOW((stderr, */
  /* 	 "/done with one full scan of newspace generation %d\n", */
  /* 	 generation)); */
}

/* Do a complete scavenge of the newspace generation. */
static void scavenge_newspace_generation(generation_index_t generation)
{
  size_t i;

  /* the new_areas array currently being written to by gc_alloc() */
  struct new_area (*current_new_areas)[] = &new_areas_1;
  size_t current_new_areas_index;

  /* the new_areas created by the previous scavenge cycle */
  struct new_area (*previous_new_areas)[] = NULL;
  size_t previous_new_areas_index;

  /* Flush the current regions updating the tables. */
  gc_alloc_update_all_page_tables();

  /* Turn on the recording of new areas by gc_alloc(). */
  new_areas = current_new_areas;
  new_areas_index = 0;

  /* Don't need to record new areas that get scavenged anyway during
   * scavenge_newspace_generation_one_scan. */
  record_new_objects = 1;

  /* Start with a full scavenge. */
  scavenge_newspace_generation_one_scan(generation);
  /* Record all new areas now. */
  record_new_objects = 2;

  /* Give a chance to weak hash tables to make other objects live.
   * FIXME: The algorithm implemented here for weak hash table gcing
   * is O(W^2+N) as Bruno Haible warns in
   * http://www.haible.de/bruno/papers/cs/weak/WeakDatastructures-writeup.html
   * see "Implementation 2". */
  /* scav_weak_hash_tables(); */

  /* Flush the current regions updating the tables. */
  gc_alloc_update_all_page_tables();

  /* Grab new_areas_index. */
  current_new_areas_index = new_areas_index;

  /* FSHOW((stderr, */
  /* 	 "The first scan is finished; current_new_areas_index=%d.\n", */
  /* 	 current_new_areas_index)); */

  while (current_new_areas_index > 0) {
    /* Move the current to the previous new areas */
    previous_new_areas = current_new_areas;
    previous_new_areas_index = current_new_areas_index;

    /* Scavenge all the areas in previous new areas. Any new areas
     * allocated are saved in current_new_areas. */

    /* Allocate an array for current_new_areas; alternating between
     * new_areas_1 and 2 */
    if (previous_new_areas == &new_areas_1)
      current_new_areas = &new_areas_2;
    else
      current_new_areas = &new_areas_1;

    /* Set up for gc_alloc(). */
    new_areas = current_new_areas;
    new_areas_index = 0;

    /* Check whether previous_new_areas had overflowed. */
    if (previous_new_areas_index >= NUM_NEW_AREAS) {
      /* FSHOW((stderr, "new_areas overflow, doing full scavenge\n")); */
      /* Don't need to record new areas that get scavenged
       * anyway during scavenge_newspace_generation_one_scan. */
      record_new_objects = 1;

      scavenge_newspace_generation_one_scan(generation);
      /* FSHOW((stderr, "new_areas overflow, doing full scavenge done\n")); */
      /* Record all new areas now. */
      record_new_objects = 2;

      /* scav_weak_hash_tables(); */

      /* Flush the current regions updating the tables. */
      gc_alloc_update_all_page_tables();

    } else {

      /* Work through previous_new_areas. */
      for (i = 0; i < previous_new_areas_index; i++) {
	page_index_t page = (*previous_new_areas)[i].page;
	size_t offset = (*previous_new_areas)[i].offset;
	size_t size = (*previous_new_areas)[i].size / N_WORD_BYTES;
	ASSERT((*previous_new_areas)[i].size % N_WORD_BYTES == 0);
	scavenge(page_address(page) + offset, size);
      }

      /* scav_weak_hash_tables(); */

      /* Flush the current regions updating the tables. */
      gc_alloc_update_all_page_tables();
    }

    current_new_areas_index = new_areas_index;

    /*FSHOW((stderr,
      "The re-scan has finished; current_new_areas_index=%d.\n",
      current_new_areas_index));*/
  }

  /* Turn off recording of areas allocated by gc_alloc(). */
  record_new_objects = 0;
}

/* Work through all the pages and free any in from_space. This
 * assumes that all objects have been copied or promoted to an older
 * generation. Bytes_allocated and the generation bytes_allocated
 * counter are updated. The number of bytes freed is returned. */
static uintptr_t free_oldspace(void)
{
  uintptr_t bytes_freed = 0;
  page_index_t first_page, last_page;

  first_page = 0;

  do {
    /* Find a first page for the next region of pages. */
    while ((first_page < last_free_page)
	   && (page_free_p(first_page)
	       || (page_table[first_page].bytes_used == 0)
	       || (page_table[first_page].gen != from_space)))
      first_page++;
    if (first_page >= last_free_page)
      break;

    /* Find the last page of this region. */
    last_page = first_page;

    do {
      /* Free the page. */
      bytes_freed += page_table[last_page].bytes_used;
      generations[page_table[last_page].gen].bytes_allocated -=
	page_table[last_page].bytes_used;
      page_table[last_page].allocated = FREE_PAGE_FLAG;
      page_table[last_page].bytes_used = 0;
      /* Should already be unprotected by unprotect_oldspace(). */
      ASSERT(!page_table[last_page].write_protected);
      last_page++;
    }
    while ((last_page < last_free_page)
	   && page_allocated_p(last_page)
	   && (page_table[last_page].bytes_used != 0)
	   && (page_table[last_page].gen == from_space));

    first_page = last_page;
  } while (first_page < last_free_page);
  bytes_allocated -= bytes_freed;
  return bytes_freed;
}

/* Take a possible pointer and mark its page in the page_table so that
 * it will not be relocated during a GC.
 *
 * This involves locating the page it points to, then backing up to
 * the start of its region, then marking all pages dont_move from there
 * up to the first page that's not full or has a different generation
 *
 * It is assumed that all the page static flags have been cleared at
 * the start of a GC.
 *
 * It is also assumed that the current gc_alloc() region has been
 * flushed and the tables updated. */
void preserve_pointer(void *addr)
{
  page_index_t addr_page_index;
  page_index_t first_page;
  page_index_t i;
  unsigned int region_allocation;

  /* obvious case */
  if (!addr) return;
  if ((intptr_t)addr == -1) return; /* hope never be used this address */

  addr_page_index = find_page_index(addr);
  /* quick check 1: Address is quite likely to have been invalid. */
  if ((addr_page_index == -1)
      || page_free_p(addr_page_index)
      || (page_table[addr_page_index].bytes_used == 0)
      || (page_table[addr_page_index].gen != from_space)
      /* Skip if already marked dont_move. */
      || (page_table[addr_page_index].dont_move != 0)) {
    return;
  }
  ASSERT(!(page_table[addr_page_index].allocated & OPEN_REGION_PAGE_FLAG));
  /* (Now that we know that addr_page_index is in range, it's
   * safe to index into page_table[] with it.) */
  region_allocation = page_table[addr_page_index].allocated;

  /* quick check 2: Check the offset within the page.
   *
   */
  if (((uintptr_t)addr & (CARD_BYTES - 1)) >
      page_table[addr_page_index].bytes_used)
    return;

  /* Filter out anything which is not in dynamic space. */
  if (!(possibly_valid_dynamic_space_pointer(addr))) {
    /* if the addr is not in dynamic space then we don't have any way to
       detect the pointer inside and there is no need to do it. 
       TODO what is somebody allocate GCable memory into non GCable?
    */
    return;
  }

  /* Find the beginning of the region.  Note that there may be
   * objects in the region preceding the one that we were passed a
   * pointer to: if this is the case, we will write-protect all the
   * previous objects' pages too.     */

  first_page = addr_page_index;
  while (page_table[first_page].region_start_offset != 0) {
    --first_page;
    /* Do some checks. */
    ASSERT(page_table[first_page].bytes_used == CARD_BYTES);
    ASSERT(page_table[first_page].gen == from_space);
    ASSERT(page_table[first_page].allocated == region_allocation);
  }
  /* Adjust any large objects before promotion as they won't be
   * copied after promotion. */
  if (page_table[first_page].large_object) {
    /* we only have this chance with Bignum, and I don't think it's
       necessary.*/
    /* maybe_adjust_large_object(page_address(first_page)); */

    /* If a large object has shrunk then addr may now point to a
     * free area in which case it's ignored here. Note it gets
     * through the valid pointer test above because the tail looks
     * like conses. */
    if (page_free_p(addr_page_index)
	|| (page_table[addr_page_index].bytes_used == 0)
	/* Check the offset within the page. */
	|| (((uintptr_t)addr & (CARD_BYTES - 1))
	    > page_table[addr_page_index].bytes_used)) {
      return;
    }
    /* It may have moved to unboxed pages. */
    region_allocation = page_table[first_page].allocated;
  }
  /* Now work forward until the end of this contiguous area is found,
   * marking all pages as dont_move. */
  for (i = first_page; ;i++) {
    ASSERT(page_table[i].allocated == region_allocation);
    /* Mark the page static. */
    page_table[i].dont_move = 1;

    /* Move the page to the new_space. XX I'd rather not do this
     * but the GC logic is not quite able to copy with the static
     * pages remaining in the from space. This also requires the
     * generation bytes_allocated counters be updated. */
    page_table[i].gen = new_space;
    generations[new_space].bytes_allocated += page_table[i].bytes_used;
    generations[from_space].bytes_allocated -= page_table[i].bytes_used;

    /* It is essential that the pages are not write protected as
     * they may have pointers into the old-space which need
     * scavenging. They shouldn't be write protected at this
     * stage. */
    ASSERT(!page_table[i].write_protected);

    /* Check whether this is the last page in this contiguous block.. */
    if ((page_table[i].bytes_used < CARD_BYTES)
	/* ..or it is CARD_BYTES and is the last in the block */
	|| page_free_p(i+1)
	|| (page_table[i+1].bytes_used == 0) /* next page free */
	|| (page_table[i+1].gen != from_space) /* diff. gen */
	|| (page_table[i+1].region_start_offset == 0))
      break;
  }

  /* Check that the page is now static. */
  ASSERT(page_table[addr_page_index].dont_move != 0);
}

static void preserve_context_registers(os_context_t *c)
{
  /* TODO Should Cygwin be treated the same as Windows?*/
#if defined(_WIN32) || defined(__CYGWIN__)
# if defined __i686__
  preserve_pointer((void*)*os_context_register_addr(c,reg_EAX));
  preserve_pointer((void*)*os_context_register_addr(c,reg_ECX));
  preserve_pointer((void*)*os_context_register_addr(c,reg_EDX));
  preserve_pointer((void*)*os_context_register_addr(c,reg_EBX));
  preserve_pointer((void*)*os_context_register_addr(c,reg_ESI));
  preserve_pointer((void*)*os_context_register_addr(c,reg_EDI));
  preserve_pointer((void*)*os_context_pc_addr(c));
# elif defined __x86_64__
  preserve_pointer((void*)*os_context_register_addr(c,reg_RAX));
  preserve_pointer((void*)*os_context_register_addr(c,reg_RCX));
  preserve_pointer((void*)*os_context_register_addr(c,reg_RDX));
  preserve_pointer((void*)*os_context_register_addr(c,reg_RBX));
  preserve_pointer((void*)*os_context_register_addr(c,reg_RSI));
  preserve_pointer((void*)*os_context_register_addr(c,reg_RDI));
  preserve_pointer((void*)*os_context_register_addr(c,reg_R8));
  preserve_pointer((void*)*os_context_register_addr(c,reg_R9));
  preserve_pointer((void*)*os_context_register_addr(c,reg_R10));
  preserve_pointer((void*)*os_context_register_addr(c,reg_R11));
  preserve_pointer((void*)*os_context_register_addr(c,reg_R12));
  preserve_pointer((void*)*os_context_register_addr(c,reg_R13));
  preserve_pointer((void*)*os_context_register_addr(c,reg_R14));
  preserve_pointer((void*)*os_context_register_addr(c,reg_R15));
  preserve_pointer((void*)*os_context_pc_addr(c));
# else
# error "preserve_context_registers needs to be tweaked for non-x86 Darwin"
# endif
#endif

#if !defined(_WIN32) && !defined(__CYGWIN__)
  void **ptr;
  for (ptr = ((void **)(c+1))-1; ptr>=(void **)c; ptr--) {
    preserve_pointer(*ptr);
  }
#endif
}

/* salvage all pointers can be followed from the gen pages.*/
static void salvage_generation(generation_index_t gen)
{
  /* the page index in the generation is reset when GC is started.
     so we must check it from the beginning. */
  page_index_t i;
  for (i = 0; i < last_free_page; i++) {
    if (page_table[i].gen == gen && page_table[i].dont_move) {
      /* OK we have reached the page */
      /* The allocation always started from the region boundary.
	 so first we need to get the region, then we can convert it
	 to block. */
      void *start = page_region_start(i);
      block_t *block = (block_t *)start;
      page_bytes_t checked = 0, limit = 0;
      /* calculate total amount of bytes */
      page_index_t si = find_page_index(start);
      while (si <= i) {
	limit += page_table[si++].bytes_used;
      } 
      /* fprintf(stderr, "salvage start: %p(%d bytes)\n", start, limit); */
      while (checked < limit) {
	uintptr_t next_offset;
	void *obj = ALLOCATED_POINTER(block);
	if (is_scheme_pointer(obj)) {
	  salvage_scheme_pointer(NULL, obj);
	} else {
	  /* salvage generic pointer including Scheme pair. 
	     TOOD move it.*/
	  size_t j, size = MEMORY_SIZE(block);
	  for (j = 0; j < size; j++) {
	    /* do last resort */
	    void *p = *(void **)((uintptr_t)obj + j);
	    if (is_scheme_pointer(p)) {
	      salvage_scheme_pointer((void **)((uintptr_t)obj + j), p);
	    } else {
	      preserve_pointer(p);
	    }
	  }
	}
	next_offset = BLOCK_SIZE(block);
	block = (block_t *)((uintptr_t)block + next_offset);
	checked +=  next_offset;
      }
      /* fprintf(stderr, "salvage end(%d bytes)\n", limit); */
    }
  }
}

static void
garbage_collect_generation(generation_index_t generation, int raise)
{
  uintptr_t bytes_freed;
  page_index_t i;
  /* uintptr_t static_space_size; */
  GC_thread_context_t *th;
  int si;			/* static index */

  ASSERT(generation <= GENERATIONS-1);
    /* The oldest generation can't be raised. */
  ASSERT((generation != GENERATIONS-1) || (raise == 0));

  /* Set the global src and dest. generations */
  from_space = generation;
  if (raise)
    new_space = generation+1;
  else
    new_space = SCRATCH_GENERATION;

  /* Change to a new space for allocation, resetting the alloc_start_page */
  gc_alloc_generation = new_space;
  generations[new_space].alloc_start_page = 0;
  generations[new_space].alloc_unboxed_start_page = 0;
  generations[new_space].alloc_large_start_page = 0;
  generations[new_space].alloc_large_unboxed_start_page = 0;

  /* Before any pointers are preserved, the dont_move flags on the
   * pages need to be cleared. */
  for (i = 0; i < last_free_page; i++)
    if(page_table[i].gen==from_space)
      page_table[i].dont_move = 0;

  /* Un-write-protect the old-space pages. This is essential for the
   * promoted pages as they may contain pointers into the old-space
   * which need to be scavenged. It also helps avoid unnecessary page
   * faults as forwarding pointers are written into them. They need to
   * be un-protected anyway before unmapping later. */
  unprotect_oldspace();

  /* Scavenge the stacks' conservative roots. */
  for_each_thread(th) {
    void **ptr;
    void **esp = (void **)-1;
    if (SG_VM(th->thread)->threadState == SG_VM_TERMINATED) continue;
    /* preserve_thread(th); */	/* pointers are not only on cstack */
    esp = th->cstackStart;	/* control stack start */
    if (th->thread == Sg_VM()) {
      /* Somebody is going to burn in hell for this, but casting it in two
	 steps shut gcc up about strict aliasing. */
      esp = (void **)((void *)&raise);
    } else {
      /* we might not this process */
      int i, free;
      void **esp1;
      free = th->freeInterruptContextIndex;
      for (i = free - 1; i >= 0; i--) {
	os_context_t *c = (os_context_t *)th->interruptContexts[i];
	esp1 = (void **)*os_context_register_addr(c, reg_SP);
	if (esp1 >= (void **)th->cstackStart && esp1 < (void **)th->cstackEnd) {
	  if (esp1 < esp) esp = esp1;
	  preserve_context_registers(c);
	}
      }
    }
    if (!esp || esp == (void *)-1) {
      Sg_Panic("garbage_collect: no SP known for thread %x (OS %x)",
	       th, th->thread);
    }
    for (ptr = ((void **)th->cstackEnd) -1; ptr >= esp; ptr--) {
      preserve_pointer(*ptr);
    }
    /* all thread (VM) must be preserved */
    preserve_pointer(th->thread);
    /* treat VM stack the same as C stack for now */
    for (ptr = SG_VM(th->thread)->stack; ptr < SG_VM(th->thread)->sp; ptr++) {
      preserve_pointer(*ptr);
    }
  }

  /* preserve static area. */
  for (si = 0; si < current_static_space_index; si++) {
    void *start = static_spaces[si].start;
    void *end = static_spaces[si].end;
    void **ptr;
    if (start && end) {
      for (ptr = (void **)end -1; (void *)ptr >= start; ptr--) {
	preserve_pointer(*ptr);
      }
    }
  }
  /* in case of restored frames (call/cc or stack overflow), we need to
     check the VM's continuation frame. We know the structure so that we
     can do it more efficiently than VM stack. so scavenge it here. */
  for_each_thread(th) {
    salvage_scheme_pointer(&SG_VM(th->thread)->ac, SG_VM(th->thread)->ac);
    salvage_scheme_pointer(&SG_VM(th->thread)->cl, SG_VM(th->thread)->cl);
    scavenge_continuation_frame(SG_VM(th->thread));
  }

  /* Before scavenge, we need to salvage all pointers can be followed from
     the ones we preserved. The original SBCL somehow doesn't consider it
     but as long as I know we need to do it. */
  /* salvage_generation(new_space); */

  /* FSHOW((stderr, */
  /* 	 "/scavenge_generations(%d %d)\n", generation+1, GENERATIONS)); */
  /* Scavenge all the rest of the roots. */
  /* All generations but the generation being GCed need to be
   * scavenged. The new_space generation needs special handling as
   * objects may be moved in - it is handled separately below. */
  scavenge_generations(generation+1, GENERATIONS);

  /* FSHOW((stderr, */
  /* 	 "/scavenge_newspace_generation(%d)\n", new_space)); */
  /* Finally scavenge the new_space generation. Keep going until no
   * more objects are moved into the new generation */ 
  scavenge_newspace_generation(new_space);

  /* OK now we need to check if the weak pointers survived. */
  scan_weak_hashtables();

  /* FSHOW((stderr, "/gc_alloc_update_all_page_tables()\n")); */
  /* Flush the current regions, updating the tables. */
  gc_alloc_update_all_page_tables();
  
  /* Free the pages in oldspace, but not those marked dont_move. */
  /* we don't collect nor mark yet so if we do this, it will stop... */
  bytes_freed = free_oldspace();

  /* If the GC is not raising the age then lower the generation back
   * to its normal generation number */
  if (!raise) {
    for (i = 0; i < last_free_page; i++)
      if ((page_table[i].bytes_used != 0)
	  && (page_table[i].gen == SCRATCH_GENERATION))
	page_table[i].gen = generation;
    ASSERT(generations[generation].bytes_allocated == 0);
    generations[generation].bytes_allocated =
      generations[SCRATCH_GENERATION].bytes_allocated;
    generations[SCRATCH_GENERATION].bytes_allocated = 0;
  }

  /* Reset the alloc_start_page for generation. */
  generations[generation].alloc_start_page = 0;
  generations[generation].alloc_unboxed_start_page = 0;
  generations[generation].alloc_large_start_page = 0;
  generations[generation].alloc_large_unboxed_start_page = 0;
#if 0
  if (generation >= verify_gens) {
    verify_gc();
    verify_dynamic_space();
  }
#endif
  generations[generation].gc_trigger =
    generations[generation].bytes_allocated
    + generations[generation].bytes_consed_between_gc;

  if (raise)
    generations[generation].num_gc = 0;
  else
    ++generations[generation].num_gc;

}

#define small_generation_limit 1
/* GC all generations newer than last_gen, raising the objects in each
 * to the next older generation - we finish when all generations below
 * last_gen are empty.  Then if last_gen is due for a GC, or if
 * last_gen==NUM_GENERATIONS (the scratch generation?  eh?) we GC that
 * too.  The valid range for last_gen is: 0,1,...,NUM_GENERATIONS.
 *
 * We stop collecting at gencgc_oldest_gen_to_gc, even if this is less than
 * last_gen (oh, and note that by default it is NUM_GENERATIONS-1) */
static void collect_garbage_inner(int last_gen)
{
  generation_index_t gen = 0, i;
  int raise, more = 0;
  int gen_to_wp;
  /* The largest value of last_free_page seen since the time
   * remap_free_pages was called. */
  static page_index_t high_water_mark = 0;
  /* log_generation_stats(gc_logfile, "=== GC Start ==="); */
  /* FSHOW((stderr, "/entering GC_collect_garbage(%d)\n", last_gen)); */

  gc_active_p = TRUE;
  if (last_gen > GENERATIONS) {
    /* FSHOW((stderr, */
    /* 	   "/collect_garbage: last_gen = %d, doing a level 0 GC\n", */
    /* 	   last_gen)); */
    last_gen = 0;
  }

  /* Flush the alloc regions updating the tables. */
  gc_alloc_update_all_page_tables();

  do {
    /* Collect the generation. */

    if (more || (gen >= gencgc_oldest_gen_to_gc)) {
      /* Never raise the oldest generation. Never raise the extra generation
       * collected due to more-flag. */
      raise = 0;
      more = 0;
    } else {
      raise =
	(gen < last_gen)
	|| (generations[gen].num_gc >= generations[gen].number_of_gcs_before_promotion);
      /* If we would not normally raise this one, but we're
       * running low on space in comparison to the object-sizes
       * we've been seeing, raise it and collect the next one
       * too. */
      if (!raise && gen == last_gen) {
	more = (2*large_allocation) >= (dynamic_space_size - bytes_allocated);
	raise = more;
      }
    }
    /* TODO where to write this? */
    if (debug_flag > 0) {
      FSHOW((stderr,
	     ";; starting GC of generation %d with raise=%d alloc=%d trig=%d GCs=%d\n",
	     gen,
	     raise,
	     generations[gen].bytes_allocated,
	     generations[gen].gc_trigger,
	     generations[gen].num_gc));
    }

    /* If an older generation is being filled, then update its
     * memory age. */
    if (raise == 1) {
      generations[gen+1].cum_sum_bytes_allocated +=
	generations[gen+1].bytes_allocated;
    }

    garbage_collect_generation(gen, raise);

    /* Reset the memory age cum_sum. */
    generations[gen].cum_sum_bytes_allocated = 0;

    /* FSHOW((stderr, "GC of generation %d finished:\n", gen)); */

    gen++;
  } while ((gen <= gencgc_oldest_gen_to_gc)
	   && ((gen < last_gen)
	       || more
	       || (raise
		   && (generations[gen].bytes_allocated
		       > generations[gen].gc_trigger)
		   && (generation_average_age(gen)
		       > generations[gen].minimum_age_before_gc))));


  /* Now if gen-1 was raised all generations before gen are empty.
   * If it wasn't raised then all generations before gen-1 are empty.
   *
   * Now objects within this gen's pages cannot point to younger
   * generations unless they are written to. This can be exploited
   * by write-protecting the pages of gen; then when younger
   * generations are GCed only the pages which have been written
   * need scanning. */
  if (raise)
    gen_to_wp = gen;
  else
    gen_to_wp = gen - 1;

  /* There's not much point in WPing pages in generation 0 as it is
   * never scavenged (except promoted pages). */
  if ((gen_to_wp > 0) && enable_page_protection) {
    /* Check that they are all empty. */
    for (i = 0; i < gen_to_wp; i++) {
      if (generations[i].bytes_allocated)
	Sg_Panic("trying to write-protect gen. %d when gen. %d nonempty\n",
		 gen_to_wp, i);
    }
    write_protect_generation_pages(gen_to_wp);
  }

  /* Set gc_alloc() back to generation 0. The current regions should
   * be flushed after the above GCs. */
  ASSERT((boxed_region.free_pointer - boxed_region.start_addr) == 0);
  gc_alloc_generation = 0;

  /* Save the high-water mark before updating last_free_page */
  if (last_free_page > high_water_mark)
    high_water_mark = last_free_page;

  update_dynamic_space_free_pointer();

  /* Update auto_gc_trigger. Make sure we trigger the next GC before
   * running out of heap! */
  if (bytes_consed_between_gcs <= (dynamic_space_size - bytes_allocated))
    auto_gc_trigger = bytes_allocated + bytes_consed_between_gcs;
  else
    auto_gc_trigger = bytes_allocated 
      + (dynamic_space_size - bytes_allocated)/2;

  /* If we did a big GC (arbitrarily defined as gen > 1), release memory
   * back to the OS.
   */
  if (gen > small_generation_limit) {
    if (last_free_page > high_water_mark)
      high_water_mark = last_free_page;
    remap_free_pages(0, high_water_mark, 0);
    high_water_mark = 0;
  }

  gc_active_p = 0;
  large_allocation = 0;
  need_gc = FALSE;

  /* TODO where to write this? */
  if (debug_flag) {
    write_generation_stats(stderr);
  }
  /* log_generation_stats(gc_logfile, "=== GC End ==="); */
}

static void scrub_stack_a_little()
{
  volatile char bogus[1024];
  memset((void *)bogus, 0, 1024);
}

void GC_collect_garbage(int last_gen)
{
  scrub_stack_a_little();
  collect_garbage_inner(last_gen);
}

void GC_init_context(GC_thread_context_t *context, void *thread)
{
  Sg_LockMutex(&context_lock);
  if (all_threads) {
    all_threads->next = context;
  } else {
    all_threads = context;
    /* the context must be root context so set it */
    *context = root_context;
  }
  gc_set_region_empty(&context->alloc_region);
  context->thread = thread;
  context->freeInterruptContextIndex = 0;
  Sg_UnlockMutex(&context_lock);
}

void GC_init_thread(GC_thread_context_t *context)
{
  thread_init(context);
}

void GC_set_debugging(int flag)
{
  debug_flag = flag;
}
void GC_print_statistic(FILE *out)
{
  write_generation_stats(out);
}

/* will be called in GC_init */
static int initialised = FALSE;
void GC_init()
{
  if (!initialised) {
    page_index_t i;
    /* initialise OS specific variables. */
    os_init();
    root_context_init(&root_context);
    dynamic_space_size = DEFAULT_DYNAMIC_SPACE_SIZE;

    Sg_InitMutex(&free_pages_lock, FALSE);
    Sg_InitMutex(&allocation_lock, FALSE);
    Sg_InitMutex(&context_lock, FALSE);

    page_table_pages = dynamic_space_size / CARD_BYTES;
    ASSERT(dynamic_space_size == npage_bytes(page_table_pages));

    bytes_consed_between_gcs = dynamic_space_size/(size_t)20;
    if (bytes_consed_between_gcs < (1024*1024))
      bytes_consed_between_gcs = 1024*1024;

    /* The page_table must be allocated using "calloc" to initialize
     * the page structures correctly. */
    page_table = calloc(page_table_pages, sizeof(page_t));
    ASSERT(page_table);

    heap_base = DYNAMIC_SPACE_START;

    bytes_allocated = 0;

    /* Initialize the generations. */
    for (i = 0; i < NUM_GENERATIONS; i++) {
      generations[i].alloc_start_page = 0;
      generations[i].alloc_unboxed_start_page = 0;
      generations[i].alloc_large_start_page = 0;
      generations[i].alloc_large_unboxed_start_page = 0;
      generations[i].bytes_allocated = 0;
      generations[i].gc_trigger = 2000000;
      generations[i].num_gc = 0;
      generations[i].cum_sum_bytes_allocated = 0;
      /* the tune-able parameters */
      generations[i].bytes_consed_between_gc
	= bytes_consed_between_gcs/(size_t)GENERATIONS-1;
      generations[i].number_of_gcs_before_promotion = 1;
      generations[i].minimum_age_before_gc = 0.75;
    }

    gc_alloc_generation = 0;
    gc_set_region_empty(&boxed_region);
    gc_set_region_empty(&unboxed_region);

    last_free_page = 0;
    /* to set auto_gc_trigger, later ... */
    /* GC_collect_garbage(0); */
    auto_gc_trigger = 2000000;
    initialised = TRUE;
  }
}
