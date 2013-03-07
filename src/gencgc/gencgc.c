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

size_t bytes_consed_between_gcs = 12*1024*1024;

static page_index_t gc_find_freeish_pages(page_index_t *restart_page_ptr,
					  intptr_t nbytes, int page_type_flag);
/* GC parameters */

/* Generations 0-5 are normal collected generations, 6 is only used as
   scratch space by the collector, and should never get collected.
 */
enum {
  SCRATCH_GENERATION = GENERATIONS + 1,
  NUM_GENERATIONS,
};

#define enable_page_protection TRUE

#if (CARD_BYTES >= PAGE_BYTES)
#define large_object_size (4 * CARD_BYTES)
#else
#define large_object_size (4 * PAGE_BYTES)
#endif

int need_gc = FALSE;

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
	  " Gen StaPg UbSta LaSta LUbSt Boxed Unboxed LB   LUB  !move  Alloc  Waste   Trig    WP  GCs Mem-age\n");

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
	    "   %1d: %5d %5d %5d %5d",
	    i,
	    generations[i].alloc_start_page,
	    generations[i].alloc_unboxed_start_page,
	    generations[i].alloc_large_start_page,
	    generations[i].alloc_large_unboxed_start_page);
    fprintf(file,
	    " %5d %5d %5d %5d %5d",
	    boxed_cnt, unboxed_cnt, large_boxed_cnt,
	    large_unboxed_cnt, pinned_cnt);
    fprintf(file,
	    " %8u %5u %8u %4d"" %3d %7.4f\n",
	    generations[i].bytes_allocated,
	    (npage_bytes(count_generation_pages(i)) 
	     - generations[i].bytes_allocated),
	    generations[i].gc_trigger,
	    count_write_protect_generation_pages(i),
	    generations[i].num_gc,
	    generation_average_age(i));
  }
  fprintf(file,"   Total bytes allocated    = %u\n", bytes_allocated);
  fprintf(file,"   Dynamic-space-size bytes = %u\n", dynamic_space_size);
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
  os_invalidate(addr, length);
  new_addr = os_validate(addr, length);
  if (new_addr == NULL || new_addr != addr) {
    Sg_Panic("remap_free_pages: page moved, 0x%08x ==> 0x%08x",
             start, new_addr);
  }
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
static void
gc_alloc_new_region(intptr_t nbytes, int page_type_flag, 
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
      add_new_area(first_page,orig_first_page_bytes_used, region_size);
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
void * gc_alloc_large(intptr_t nbytes, int page_type_flag,
		      alloc_region_t *alloc_region)
{
  int more;
  page_index_t first_page, next_page, last_page;
  page_bytes_t orig_first_page_bytes_used;
  size_t byte_cnt;
  size_t bytes_used;

  Sg_LockMutex(&free_pages_lock);

  first_page = generation_alloc_start_page(gc_alloc_generation,
					   page_type_flag, TRUE);
  if (first_page <= alloc_region->last_page) {
    first_page = alloc_region->last_page+1;
  }

  last_page=gc_find_freeish_pages(&first_page,nbytes, page_type_flag);

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
  if ((bytes_used = nbytes+orig_first_page_bytes_used) > CARD_BYTES) {
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
    bytes_used=(nbytes+orig_first_page_bytes_used)-byte_cnt;
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

  ASSERT((byte_cnt-orig_first_page_bytes_used) == nbytes);

  bytes_allocated += nbytes;
  generations[gc_alloc_generation].bytes_allocated += nbytes;

  /* Add the region to the new_areas if requested. */
  if (BOXED_PAGE_FLAG & page_type_flag)
    add_new_area(first_page,orig_first_page_bytes_used,nbytes);

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

  return page_address(first_page);
}

static page_index_t gencgc_alloc_start_page = -1;

static GC_oom_func oom_handler = NULL;
void GC_set_oom_fn(GC_oom_func handler)
{
  oom_handler = handler;
}

void gc_heap_exhausted_error(intptr_t available, intptr_t requested)
{
  /* dump the heap information */
  report_heap_exhaustion(available, requested);
  if (oom_handler) oom_handler(requested);
  /* we don't do anything but exit */
  Sg_Panic("Heap exhausted, game over.");
}


page_index_t
gc_find_freeish_pages(page_index_t *restart_page_ptr, intptr_t bytes,
                      int page_type_flag)
{
  page_index_t most_bytes_found_from = 0, most_bytes_found_to = 0;
  page_index_t first_page, last_page, restart_page = *restart_page_ptr;
  size_t nbytes = bytes;
  size_t nbytes_goal = nbytes;
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
gc_alloc_with_region(intptr_t nbytes,int page_type_flag, 
		     alloc_region_t *my_region, int quick_p)
{
  void *new_free_pointer;

  if (nbytes>=large_object_size)
    return gc_alloc_large(nbytes, page_type_flag, my_region);

  /* Check whether there is room in the current alloc region. */
  new_free_pointer = my_region->free_pointer + nbytes;

  if (new_free_pointer <= my_region->end_addr) {
    /* If so then allocate from the current alloc region. */
    void *new_obj = my_region->free_pointer;
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

    return((void *)new_obj);
  }

  /* Else not enough free space in the current region: retry with a
   * new region. */

  gc_alloc_update_page_tables(page_type_flag, my_region);
  gc_alloc_new_region(nbytes, page_type_flag, my_region);
  return gc_alloc_with_region(nbytes, page_type_flag, my_region,0);
}

static inline void *
gc_general_alloc(intptr_t nbytes, int page_type_flag, int quick_p)
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
static inline void * gc_quick_alloc(uintptr_t nbytes)
{
  return gc_general_alloc(nbytes, BOXED_PAGE_FLAG, ALLOC_QUICK);
}


static inline void * gc_alloc_unboxed(uintptr_t nbytes)
{
  return gc_general_alloc(nbytes, UNBOXED_PAGE_FLAG, FALSE);
}

static inline void * gc_quick_alloc_unboxed(uintptr_t nbytes)
{
  return gc_general_alloc(nbytes, UNBOXED_PAGE_FLAG, ALLOC_QUICK);
}


static inline SgObject
gc_general_copy_object(SgObject object, long nwords, int page_type_flag)
{
  SgObject newobj;
  /* TODO check */
  /* ASSERT(is_lisp_pointer(object)); */
  ASSERT(from_space_p(object));
  ASSERT((nwords & 0x01) == 0);
  /* Allocate space. */
  newobj = gc_general_alloc(nwords * N_WORD_BYTES, page_type_flag, ALLOC_QUICK);
  /* Copy the object.
     If the object contains a class as its header, however
     it will be copied anyway. So I *hope* it won't be a problem. */
  memcpy(newobj, object, nwords * N_WORD_BYTES);
  return newobj;
}


/* Copy a large object. If the object is in a large object region then
 * it is simply promoted, else it is copied. If it's large enough then
 * it's copied to a large object region.
 *
 * Bignums and vectors may have shrunk. If the object is not copied
 * the space needs to be reclaimed, and the page_tables corrected. */
static SgObject
general_copy_large_object(SgObject object, uintptr_t nwords, int boxedp)
{
  page_index_t first_page;
  /* TODO how to check? */
  /* ASSERT(is_lisp_pointer(object)); */
  ASSERT(from_space_p(object));
  ASSERT((nwords & 0x01) == 0);

  /* Check whether it's a large object. */
  first_page = find_page_index((void *)object);
  ASSERT(first_page >= 0);

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
    remaining_bytes = nwords * N_WORD_BYTES;

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

    generations[from_space].bytes_allocated -= nwords * N_WORD_BYTES
      + bytes_freed;
    generations[new_space].bytes_allocated += nwords * N_WORD_BYTES;
    bytes_allocated -= bytes_freed;

    /* Add the region to the new_areas if requested. */
    if (boxedp)
      add_new_area(first_page, 0, nwords * N_WORD_BYTES);

    return(object);

  } else {
    /* it's the same ... */
    return gc_general_copy_object(object, nwords,
				  (boxedp
				   ? BOXED_PAGE_FLAG
				   : UNBOXED_PAGE_FLAG));
  }
}

SgObject copy_large_object(SgObject object, intptr_t nwords)
{
  return general_copy_large_object(object, nwords, TRUE);
}

SgObject copy_large_unboxed_object(SgObject object, intptr_t nwords)
{
  return general_copy_large_object(object, nwords, FALSE);
}

/* to copy unboxed objects */
SgObject copy_unboxed_object(SgObject object, intptr_t nwords)
{
  return gc_general_copy_object(object, nwords, UNBOXED_PAGE_FLAG);
}

SgObject gc_search_space(void *start, size_t words, void *pointer)
{
  while (words > 0) {
    size_t count = 1;

    /* void *thing = start; */
    /* TODO how to get object size without accessing the pointer? */
    /* if (SG_PAIRP(thing) || SG_IMMEDIATEP(thing)) */
    /*   count = 2; */
    /* else */
    /*   count = (sizetab[widetag_of(thing)])(start); */

    /* Check whether the pointer is within this object. */
    if ((pointer >= start) && (pointer < (start+count))) {
      /* found it! */
      /*FSHOW((stderr,"/found %x in %x %x\n", pointer, start, thing));*/
      return start;
    }

    /* Round up the count. */
    count = CEILING(count, 2);

    start += count;
    words -= count;
  }
  return NULL;
}


/* a faster version for searching the dynamic space. This will work even
 * if the object is in a current allocation region. */
static SgObject search_dynamic_space(void *pointer)
{
  page_index_t page_index = find_page_index(pointer);
  void *start;

  /* The address may be invalid, so do some checks. */
  if ((page_index == -1) || page_free_p(page_index))
    return NULL;
  start = page_region_start(page_index);
  return gc_search_space(start, (pointer+2)-start, pointer);
}

static inline void *
general_alloc_internal(size_t nbytes, int page_type_flag,
		       alloc_region_t *region, SgVM *vm)
{
  void *new_obj, *new_free_pointer;
  size_t trigger_bytes = 0;

  ASSERT(nbytes > 0);
  ASSERT((((uintptr_t)region->free_pointer & LOWTAG_MASK) == 0)
	 && ((nbytes & LOWTAG_MASK) == 0));

  if (nbytes > large_allocation) large_allocation = nbytes;
  /* maybe we can do this quickly ... */
  new_free_pointer = region->free_pointer + nbytes;
  if (new_free_pointer <= region->end_addr) {
    new_obj = (void*)(region->free_pointer);
    region->free_pointer = new_free_pointer;
    return new_obj;        /* yup */
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
  return gc_alloc_with_region(nbytes, page_type_flag, region, 0);
}

void * general_alloc(size_t nbytes, int page_type_flag)
{
  SgVM *vm = Sg_VM();
  if (BOXED_PAGE_FLAG & page_type_flag) {
    /* TODO thread */
    alloc_region_t *region = &boxed_region;
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
  /* TODO multithread */
#if 0
  struct thread *th;
  for_each_thread(th) {
    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->alloc_region);
  }
#endif
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

static void remap_page_range (page_index_t from, page_index_t to)
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

static void
garbage_collect_generation(generation_index_t generation, int raise)
{
  uintptr_t bytes_freed;
  page_index_t i;
  uintptr_t static_space_size;
  SgVM *th;

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
  /* unprotect_oldspace(); */

  /* Scavenge the stacks' conservative roots. */
  /* TODO */

  /* Scavenge all the rest of the roots. */
  /* TODO */


  /* Flush the current regions, updating the tables. */
  gc_alloc_update_all_page_tables();
  
  /* Free the pages in oldspace, but not those marked dont_move. */
  /* we don't collect nor mark yet so if we do this, it will stop... */
  /* bytes_freed = free_oldspace(); */

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
void * GC_collect_garbage(int last_gen)
{
  generation_index_t gen = 0, i;
  int raise, more = 0;
  int gen_to_wp;
  /* The largest value of last_free_page seen since the time
   * remap_free_pages was called. */
  static page_index_t high_water_mark = 0;
  /* log_generation_stats(gc_logfile, "=== GC Start ==="); */

  gc_active_p = TRUE;
  if (last_gen > GENERATIONS) {
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
    /* If an older generation is being filled, then update its
     * memory age. */
    if (raise == 1) {
      generations[gen+1].cum_sum_bytes_allocated +=
	generations[gen+1].bytes_allocated;
    }

    garbage_collect_generation(gen, raise);

    /* Reset the memory age cum_sum. */
    generations[gen].cum_sum_bytes_allocated = 0;
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

  /* log_generation_stats(gc_logfile, "=== GC End ==="); */
}

/* will be called in GC_init */
void GC_init()
{
  page_index_t i;
  /* initialise OS specific variables. */
  os_init();

  Sg_InitMutex(&free_pages_lock, FALSE);
  Sg_InitMutex(&allocation_lock, FALSE);

  page_table_pages = dynamic_space_size / CARD_BYTES;
  ASSERT(dynamic_space_size == npage_bytes(page_table_pages));

  bytes_consed_between_gcs = dynamic_space_size/(size_t)20;
  if (bytes_consed_between_gcs < (1024*1024))
    bytes_consed_between_gcs = 1024*1024;

  /* The page_table must be allocated using "calloc" to initialize
   * the page structures correctly. There used to be a separate
   * initialization loop (now commented out; see below) but that was
   * unnecessary and did hurt startup time. */
  page_table = calloc(page_table_pages, sizeof(struct page));
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
}
