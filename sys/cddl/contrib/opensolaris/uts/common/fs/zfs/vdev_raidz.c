/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
 * or http://www.opensolaris.org/os/licensing.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at usr/src/OPENSOLARIS.LICENSE.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 */

/*
 * Copyright (c) 2005, 2010, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2012, 2017 by Delphix. All rights reserved.
 * Copyright (c) 2013, Joyent, Inc. All rights reserved.
 * Copyright (c) 2014 Integros [integros.com]
 */

#include <sys/zfs_context.h>
#include <sys/spa.h>
#include <sys/zap.h>
#include <sys/vdev_impl.h>
#ifdef illumos
#include <sys/vdev_disk.h>
#endif
#include <sys/vdev_file.h>
#include <sys/vdev_raidz.h>
#include <sys/metaslab_impl.h>
#include <sys/zio.h>
#include <sys/zio_checksum.h>
#include <sys/abd.h>
#include <sys/fs/zfs.h>
#include <sys/fm/fs/zfs.h>
#include <sys/bio.h>

#if 0
#ifdef ZFS_DEBUG
#include <sys/vdev_initialize.h>	/* vdev_xlate testing */
#endif
#endif

/*
 * Virtual device vector for RAID-Z.
 *
 * This vdev supports single, double, and triple parity. For single parity,
 * we use a simple XOR of all the data columns. For double or triple parity,
 * we use a special case of Reed-Solomon coding. This extends the
 * technique described in "The mathematics of RAID-6" by H. Peter Anvin by
 * drawing on the system described in "A Tutorial on Reed-Solomon Coding for
 * Fault-Tolerance in RAID-like Systems" by James S. Plank on which the
 * former is also based. The latter is designed to provide higher performance
 * for writes.
 *
 * Note that the Plank paper claimed to support arbitrary N+M, but was then
 * amended six years later identifying a critical flaw that invalidates its
 * claims. Nevertheless, the technique can be adapted to work for up to
 * triple parity. For additional parity, the amendment "Note: Correction to
 * the 1997 Tutorial on Reed-Solomon Coding" by James S. Plank and Ying Ding
 * is viable, but the additional complexity means that write performance will
 * suffer.
 *
 * All of the methods above operate on a Galois field, defined over the
 * integers mod 2^N. In our case we choose N=8 for GF(8) so that all elements
 * can be expressed with a single byte. Briefly, the operations on the
 * field are defined as follows:
 *
 *   o addition (+) is represented by a bitwise XOR
 *   o subtraction (-) is therefore identical to addition: A + B = A - B
 *   o multiplication of A by 2 is defined by the following bitwise expression:
 *
 *	(A * 2)_7 = A_6
 *	(A * 2)_6 = A_5
 *	(A * 2)_5 = A_4
 *	(A * 2)_4 = A_3 + A_7
 *	(A * 2)_3 = A_2 + A_7
 *	(A * 2)_2 = A_1 + A_7
 *	(A * 2)_1 = A_0
 *	(A * 2)_0 = A_7
 *
 * In C, multiplying by 2 is therefore ((a << 1) ^ ((a & 0x80) ? 0x1d : 0)).
 * As an aside, this multiplication is derived from the error correcting
 * primitive polynomial x^8 + x^4 + x^3 + x^2 + 1.
 *
 * Observe that any number in the field (except for 0) can be expressed as a
 * power of 2 -- a generator for the field. We store a table of the powers of
 * 2 and logs base 2 for quick look ups, and exploit the fact that A * B can
 * be rewritten as 2^(log_2(A) + log_2(B)) (where '+' is normal addition rather
 * than field addition). The inverse of a field element A (A^-1) is therefore
 * A ^ (255 - 1) = A^254.
 *
 * The up-to-three parity columns, P, Q, R over several data columns,
 * D_0, ... D_n-1, can be expressed by field operations:
 *
 *	P = D_0 + D_1 + ... + D_n-2 + D_n-1
 *	Q = 2^n-1 * D_0 + 2^n-2 * D_1 + ... + 2^1 * D_n-2 + 2^0 * D_n-1
 *	  = ((...((D_0) * 2 + D_1) * 2 + ...) * 2 + D_n-2) * 2 + D_n-1
 *	R = 4^n-1 * D_0 + 4^n-2 * D_1 + ... + 4^1 * D_n-2 + 4^0 * D_n-1
 *	  = ((...((D_0) * 4 + D_1) * 4 + ...) * 4 + D_n-2) * 4 + D_n-1
 *
 * We chose 1, 2, and 4 as our generators because 1 corresponds to the trival
 * XOR operation, and 2 and 4 can be computed quickly and generate linearly-
 * independent coefficients. (There are no additional coefficients that have
 * this property which is why the uncorrected Plank method breaks down.)
 *
 * See the reconstruction code below for how P, Q and R can used individually
 * or in concert to recover missing data columns.
 */

typedef struct raidz_col {
	uint64_t rc_devidx;		/* child device index for I/O */
	uint64_t rc_offset;		/* device offset */
	uint64_t rc_size;		/* I/O size */
	abd_t *rc_abd;			/* I/O data */
	void *rc_orig_data;		/* pre-reconstruction */
	void *rc_gdata;			/* used to store the "good" version */
	int rc_error;			/* I/O error for this device */
	uint8_t rc_tried;		/* Did we attempt this I/O column? */
	uint8_t rc_skipped;		/* Did we skip this I/O column? */
	uint8_t rc_need_orig_restore;	/* need to restore from orig_data? */
} raidz_col_t;

typedef struct raidz_row {
	uint64_t rr_cols;		/* Regular column count */
	uint64_t rr_missingdata;	/* Count of missing data devices */
	uint64_t rr_missingparity;	/* Count of missing parity devices */
	uint64_t rr_firstdatacol;	/* First data column/parity count */
	abd_t *rr_abd_copy;		/* rm_asize-buffer of copied data */
	int rr_code;			/* reconstruction code */
	raidz_col_t rr_col[0];		/* Flexible array of I/O columns */
} raidz_row_t;

typedef struct raidz_map {
	uintptr_t rm_reports;		/* # of referencing checksum reports */
	boolean_t rm_freed;		/* map no longer has referencing ZIO */
	boolean_t rm_ecksuminjected;	/* checksum error was injected */
	int rm_nrows;
	int rm_nskip;			/* Sectors skipped for padding */
	raidz_row_t *rm_row[0];		/* flexible array of rows */
} raidz_map_t;

#define	VDEV_RAIDZ_P		0
#define	VDEV_RAIDZ_Q		1
#define	VDEV_RAIDZ_R		2

#define	VDEV_RAIDZ_MUL_2(x)	(((x) << 1) ^ (((x) & 0x80) ? 0x1d : 0))
#define	VDEV_RAIDZ_MUL_4(x)	(VDEV_RAIDZ_MUL_2(VDEV_RAIDZ_MUL_2(x)))

/*
 * We provide a mechanism to perform the field multiplication operation on a
 * 64-bit value all at once rather than a byte at a time. This works by
 * creating a mask from the top bit in each byte and using that to
 * conditionally apply the XOR of 0x1d.
 */
#define	VDEV_RAIDZ_64MUL_2(x, mask) \
{ \
	(mask) = (x) & 0x8080808080808080ULL; \
	(mask) = ((mask) << 1) - ((mask) >> 7); \
	(x) = (((x) << 1) & 0xfefefefefefefefeULL) ^ \
	    ((mask) & 0x1d1d1d1d1d1d1d1d); \
}

#define	VDEV_RAIDZ_64MUL_4(x, mask) \
{ \
	VDEV_RAIDZ_64MUL_2((x), mask); \
	VDEV_RAIDZ_64MUL_2((x), mask); \
}

#define	VDEV_LABEL_OFFSET(x)	(x + VDEV_LABEL_START_SIZE)

/*
 * Force reconstruction to use the general purpose method.
 */
int vdev_raidz_default_to_general;

/* Powers of 2 in the Galois field defined above. */
static const uint8_t vdev_raidz_pow2[256] = {
	0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
	0x1d, 0x3a, 0x74, 0xe8, 0xcd, 0x87, 0x13, 0x26,
	0x4c, 0x98, 0x2d, 0x5a, 0xb4, 0x75, 0xea, 0xc9,
	0x8f, 0x03, 0x06, 0x0c, 0x18, 0x30, 0x60, 0xc0,
	0x9d, 0x27, 0x4e, 0x9c, 0x25, 0x4a, 0x94, 0x35,
	0x6a, 0xd4, 0xb5, 0x77, 0xee, 0xc1, 0x9f, 0x23,
	0x46, 0x8c, 0x05, 0x0a, 0x14, 0x28, 0x50, 0xa0,
	0x5d, 0xba, 0x69, 0xd2, 0xb9, 0x6f, 0xde, 0xa1,
	0x5f, 0xbe, 0x61, 0xc2, 0x99, 0x2f, 0x5e, 0xbc,
	0x65, 0xca, 0x89, 0x0f, 0x1e, 0x3c, 0x78, 0xf0,
	0xfd, 0xe7, 0xd3, 0xbb, 0x6b, 0xd6, 0xb1, 0x7f,
	0xfe, 0xe1, 0xdf, 0xa3, 0x5b, 0xb6, 0x71, 0xe2,
	0xd9, 0xaf, 0x43, 0x86, 0x11, 0x22, 0x44, 0x88,
	0x0d, 0x1a, 0x34, 0x68, 0xd0, 0xbd, 0x67, 0xce,
	0x81, 0x1f, 0x3e, 0x7c, 0xf8, 0xed, 0xc7, 0x93,
	0x3b, 0x76, 0xec, 0xc5, 0x97, 0x33, 0x66, 0xcc,
	0x85, 0x17, 0x2e, 0x5c, 0xb8, 0x6d, 0xda, 0xa9,
	0x4f, 0x9e, 0x21, 0x42, 0x84, 0x15, 0x2a, 0x54,
	0xa8, 0x4d, 0x9a, 0x29, 0x52, 0xa4, 0x55, 0xaa,
	0x49, 0x92, 0x39, 0x72, 0xe4, 0xd5, 0xb7, 0x73,
	0xe6, 0xd1, 0xbf, 0x63, 0xc6, 0x91, 0x3f, 0x7e,
	0xfc, 0xe5, 0xd7, 0xb3, 0x7b, 0xf6, 0xf1, 0xff,
	0xe3, 0xdb, 0xab, 0x4b, 0x96, 0x31, 0x62, 0xc4,
	0x95, 0x37, 0x6e, 0xdc, 0xa5, 0x57, 0xae, 0x41,
	0x82, 0x19, 0x32, 0x64, 0xc8, 0x8d, 0x07, 0x0e,
	0x1c, 0x38, 0x70, 0xe0, 0xdd, 0xa7, 0x53, 0xa6,
	0x51, 0xa2, 0x59, 0xb2, 0x79, 0xf2, 0xf9, 0xef,
	0xc3, 0x9b, 0x2b, 0x56, 0xac, 0x45, 0x8a, 0x09,
	0x12, 0x24, 0x48, 0x90, 0x3d, 0x7a, 0xf4, 0xf5,
	0xf7, 0xf3, 0xfb, 0xeb, 0xcb, 0x8b, 0x0b, 0x16,
	0x2c, 0x58, 0xb0, 0x7d, 0xfa, 0xe9, 0xcf, 0x83,
	0x1b, 0x36, 0x6c, 0xd8, 0xad, 0x47, 0x8e, 0x01
};
/* Logs of 2 in the Galois field defined above. */
static const uint8_t vdev_raidz_log2[256] = {
	0x00, 0x00, 0x01, 0x19, 0x02, 0x32, 0x1a, 0xc6,
	0x03, 0xdf, 0x33, 0xee, 0x1b, 0x68, 0xc7, 0x4b,
	0x04, 0x64, 0xe0, 0x0e, 0x34, 0x8d, 0xef, 0x81,
	0x1c, 0xc1, 0x69, 0xf8, 0xc8, 0x08, 0x4c, 0x71,
	0x05, 0x8a, 0x65, 0x2f, 0xe1, 0x24, 0x0f, 0x21,
	0x35, 0x93, 0x8e, 0xda, 0xf0, 0x12, 0x82, 0x45,
	0x1d, 0xb5, 0xc2, 0x7d, 0x6a, 0x27, 0xf9, 0xb9,
	0xc9, 0x9a, 0x09, 0x78, 0x4d, 0xe4, 0x72, 0xa6,
	0x06, 0xbf, 0x8b, 0x62, 0x66, 0xdd, 0x30, 0xfd,
	0xe2, 0x98, 0x25, 0xb3, 0x10, 0x91, 0x22, 0x88,
	0x36, 0xd0, 0x94, 0xce, 0x8f, 0x96, 0xdb, 0xbd,
	0xf1, 0xd2, 0x13, 0x5c, 0x83, 0x38, 0x46, 0x40,
	0x1e, 0x42, 0xb6, 0xa3, 0xc3, 0x48, 0x7e, 0x6e,
	0x6b, 0x3a, 0x28, 0x54, 0xfa, 0x85, 0xba, 0x3d,
	0xca, 0x5e, 0x9b, 0x9f, 0x0a, 0x15, 0x79, 0x2b,
	0x4e, 0xd4, 0xe5, 0xac, 0x73, 0xf3, 0xa7, 0x57,
	0x07, 0x70, 0xc0, 0xf7, 0x8c, 0x80, 0x63, 0x0d,
	0x67, 0x4a, 0xde, 0xed, 0x31, 0xc5, 0xfe, 0x18,
	0xe3, 0xa5, 0x99, 0x77, 0x26, 0xb8, 0xb4, 0x7c,
	0x11, 0x44, 0x92, 0xd9, 0x23, 0x20, 0x89, 0x2e,
	0x37, 0x3f, 0xd1, 0x5b, 0x95, 0xbc, 0xcf, 0xcd,
	0x90, 0x87, 0x97, 0xb2, 0xdc, 0xfc, 0xbe, 0x61,
	0xf2, 0x56, 0xd3, 0xab, 0x14, 0x2a, 0x5d, 0x9e,
	0x84, 0x3c, 0x39, 0x53, 0x47, 0x6d, 0x41, 0xa2,
	0x1f, 0x2d, 0x43, 0xd8, 0xb7, 0x7b, 0xa4, 0x76,
	0xc4, 0x17, 0x49, 0xec, 0x7f, 0x0c, 0x6f, 0xf6,
	0x6c, 0xa1, 0x3b, 0x52, 0x29, 0x9d, 0x55, 0xaa,
	0xfb, 0x60, 0x86, 0xb1, 0xbb, 0xcc, 0x3e, 0x5a,
	0xcb, 0x59, 0x5f, 0xb0, 0x9c, 0xa9, 0xa0, 0x51,
	0x0b, 0xf5, 0x16, 0xeb, 0x7a, 0x75, 0x2c, 0xd7,
	0x4f, 0xae, 0xd5, 0xe9, 0xe6, 0xe7, 0xad, 0xe8,
	0x74, 0xd6, 0xf4, 0xea, 0xa8, 0x50, 0x58, 0xaf,
};

static void vdev_raidz_generate_parity(raidz_row_t *);

/*
 * Multiply a given number by 2 raised to the given power.
 */
static uint8_t
vdev_raidz_exp2(uint_t a, int exp)
{
	if (a == 0)
		return (0);

	ASSERT(exp >= 0);
	ASSERT(vdev_raidz_log2[a] > 0 || a == 1);

	exp += vdev_raidz_log2[a];
	if (exp > 255)
		exp -= 255;

	return (vdev_raidz_pow2[exp]);
}

static void
vdev_raidz_row_free(raidz_row_t *rr)
{
	int c;

	for (c = 0; c < rr->rr_firstdatacol && c < rr->rr_cols; c++) {
		if (rr->rr_col[c].rc_abd != NULL)
			abd_free(rr->rr_col[c].rc_abd);

		if (rr->rr_col[c].rc_gdata != NULL) {
			zio_buf_free(rr->rr_col[c].rc_gdata,
			    rr->rr_col[c].rc_size);
		}
		if (rr->rr_col[c].rc_orig_data != NULL) {
			zio_buf_free(rr->rr_col[c].rc_orig_data,
			    rr->rr_col[c].rc_size);
		}
	}

	for (c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
		if (rr->rr_col[c].rc_abd != NULL)
			abd_put(rr->rr_col[c].rc_abd);
		if (rr->rr_col[c].rc_orig_data != NULL) {
			zio_buf_free(rr->rr_col[c].rc_orig_data,
			    rr->rr_col[c].rc_size);
		}
	}

	if (rr->rr_abd_copy != NULL)
		abd_free(rr->rr_abd_copy);

	kmem_free(rr, offsetof(raidz_row_t, rr_col[rr->rr_cols]));
}

static void
vdev_raidz_map_free(raidz_map_t *rm)
{
        for (int i = 0; i < rm->rm_nrows; i++) {
                vdev_raidz_row_free(rm->rm_row[i]);
        }
	kmem_free(rm, offsetof(raidz_map_t, rm_row[rm->rm_nrows]));
}

static void
vdev_raidz_map_free_vsd(zio_t *zio)
{
	raidz_map_t *rm = zio->io_vsd;

	ASSERT0(rm->rm_freed);
	rm->rm_freed = B_TRUE;

	if (rm->rm_reports == 0) {
		vdev_raidz_map_free(rm);
	}
}

/*ARGSUSED*/
static void
vdev_raidz_cksum_free(void *arg, size_t ignored)
{
	raidz_map_t *rm = arg;

	ASSERT3U(rm->rm_reports, >, 0);

	if (--rm->rm_reports == 0 && rm->rm_freed)
		vdev_raidz_map_free(rm);
}

static void
vdev_raidz_cksum_finish(zio_cksum_report_t *zcr, const void *good_data)
{
	raidz_map_t *rm = zcr->zcr_cbdata;
	size_t c = zcr->zcr_cbinfo;
	size_t x;

	const char *good = NULL;
	char *bad;

	zfs_dbgmsg("checksum error on rm=%p", rm);

	if (good_data == NULL) {
		zfs_ereport_finish_checksum(zcr, NULL, NULL, B_FALSE);
		return;
	}

	zfs_ereport_finish_checksum(zcr, NULL, NULL, B_FALSE);
#if 0
	if (c < rm->rr_firstdatacol) {
		/*
		 * The first time through, calculate the parity blocks for
		 * the good data (this relies on the fact that the good
		 * data never changes for a given logical ZIO)
		 */
		if (rm->rr_col[0].rc_gdata == NULL) {
			abd_t *bad_parity[VDEV_RAIDZ_MAXPARITY];
			char *buf;
			int offset;

			/*
			 * Set up the rm_col[]s to generate the parity for
			 * good_data, first saving the parity bufs and
			 * replacing them with buffers to hold the result.
			 */
			for (x = 0; x < rm->rr_firstdatacol; x++) {
				bad_parity[x] = rm->rr_col[x].rc_abd;
				rm->rr_col[x].rc_gdata =
				    zio_buf_alloc(rm->rr_col[x].rc_size);
				rm->rr_col[x].rc_abd =
				    abd_get_from_buf(rm->rr_col[x].rc_gdata,
				    rm->rr_col[x].rc_size);
			}

			/* fill in the data columns from good_data */
			buf = (char *)good_data;
			for (; x < rm->rr_cols; x++) {
				abd_put(rm->rr_col[x].rc_abd);
				rm->rr_col[x].rc_abd = abd_get_from_buf(buf,
				    rm->rr_col[x].rc_size);
				buf += rm->rr_col[x].rc_size;
			}

			/*
			 * Construct the parity from the good data.
			 */
			vdev_raidz_generate_parity(rm);

			/* restore everything back to its original state */
			for (x = 0; x < rm->rr_firstdatacol; x++) {
				abd_put(rm->rr_col[x].rc_abd);
				rm->rr_col[x].rc_abd = bad_parity[x];
			}

			offset = 0;
			for (x = rm->rr_firstdatacol; x < rm->rr_cols; x++) {
				abd_put(rm->rr_col[x].rc_abd);
				rm->rr_col[x].rc_abd = abd_get_offset(
				    rm->rr_abd_copy, offset);
				offset += rm->rr_col[x].rc_size;
			}
		}

		ASSERT3P(rm->rr_col[c].rc_gdata, !=, NULL);
		good = rm->rr_col[c].rc_gdata;
	} else {
		/* adjust good_data to point at the start of our column */
		good = good_data;

		for (x = rm->rr_firstdatacol; x < c; x++)
			good += rm->rr_col[x].rc_size;
	}

	bad = abd_borrow_buf_copy(rm->rr_col[c].rc_abd, rm->rr_col[c].rc_size);
	/* we drop the ereport if it ends up that the data was good */
	zfs_ereport_finish_checksum(zcr, good, bad, B_TRUE);
	abd_return_buf(rm->rr_col[c].rc_abd, bad, rm->rr_col[c].rc_size);
#endif
}

/*
 * Invoked indirectly by zfs_ereport_start_checksum(), called
 * below when our read operation fails completely.  The main point
 * is to keep a copy of everything we read from disk, so that at
 * vdev_raidz_cksum_finish() time we can compare it with the good data.
 */
static void
vdev_raidz_cksum_report(zio_t *zio, zio_cksum_report_t *zcr, void *arg)
{
	size_t c = (size_t)(uintptr_t)arg;
	raidz_map_t *rm = zio->io_vsd;

	/* set up the report and bump the refcount  */
	zcr->zcr_cbdata = rm;
	zcr->zcr_cbinfo = c;
	zcr->zcr_finish = vdev_raidz_cksum_finish;
	zcr->zcr_free = vdev_raidz_cksum_free;

	rm->rm_reports++;
	ASSERT3U(rm->rm_reports, >, 0);

	if (rm->rm_row[0]->rr_abd_copy != NULL)
		return;

	/*
	 * It's the first time we're called for this raidz_map_t, so we need
	 * to copy the data aside; there's no guarantee that our zio's buffer
	 * won't be re-used for something else.
	 *
	 * Our parity data is already in separate buffers, so there's no need
	 * to copy them.
	 */

	for (int i = 0; i < rm->rm_nrows; i++) {
		raidz_row_t *rr = rm->rm_row[i];
		size_t offset;
		size_t size = 0;
		for (c = rr->rr_firstdatacol; c < rr->rr_cols; c++)
			size += rr->rr_col[c].rc_size;

		rr->rr_abd_copy =
		    abd_alloc_sametype(rr->rr_col[rr->rr_firstdatacol].rc_abd,
		    size);

		for (offset = 0, c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
			raidz_col_t *col = &rr->rr_col[c];

			if (col->rc_size == 0)
				continue;

			abd_t *tmp = abd_get_offset(rr->rr_abd_copy, offset);

			abd_copy(tmp, col->rc_abd, col->rc_size);
			abd_put(col->rc_abd);
			col->rc_abd = tmp;

			offset += col->rc_size;
		}
		ASSERT3U(offset, ==, size);
	}
}

static const zio_vsd_ops_t vdev_raidz_vsd_ops = {
	vdev_raidz_map_free_vsd,
	vdev_raidz_cksum_report
};

/*
 * Divides the IO evenly across all child vdevs; usually, dcols is
 * the number of children in the target vdev.
 */
static raidz_map_t *
vdev_raidz_map_alloc(abd_t *abd, uint64_t size, uint64_t offset, boolean_t dofree,
    uint64_t unit_shift, uint64_t dcols, uint64_t nparity)
{
	raidz_row_t *rr;
	/* The starting RAIDZ (parent) vdev sector of the block. */
	uint64_t b = offset >> unit_shift;
	/* The zio's size in units of the vdev's minimum sector size. */
	uint64_t s = size >> unit_shift;
	/* The first column for this stripe. */
	uint64_t f = b % dcols;
	/* The starting byte offset on each child vdev. */
	uint64_t o = (b / dcols) << unit_shift;
	uint64_t q, r, c, bc, col, acols, coff, devidx, asize, tot;
	uint64_t off = 0;

	raidz_map_t *rm =
	    kmem_zalloc(offsetof(raidz_map_t, rm_row[1]), KM_SLEEP);
	rm->rm_nrows = 1;

	/*
	 * "Quotient": The number of data sectors for this stripe on all but
	 * the "big column" child vdevs that also contain "remainder" data.
	 */
	q = s / (dcols - nparity);

	/*
	 * "Remainder": The number of partial stripe data sectors in this I/O.
	 * This will add a sector to some, but not all, child vdevs.
	 */
	r = s - q * (dcols - nparity);

	/* The number of "big columns" - those which contain remainder data. */
	bc = (r == 0 ? 0 : r + nparity);

	/*
	 * The total number of data and parity sectors associated with
	 * this I/O.
	 */
	tot = s + nparity * (q + (r == 0 ? 0 : 1));

	/* acols: The columns that will be accessed. */
	if (q == 0) {
		/* Our I/O request doesn't span all child vdevs. */
		acols = bc;
	} else {
		acols = dcols;
	}

	rr = kmem_alloc(offsetof(raidz_row_t, rr_col[acols]), KM_SLEEP);
	rm->rm_row[0] = rr;

	rr->rr_cols = acols;
	rr->rr_missingdata = 0;
	rr->rr_missingparity = 0;
	rr->rr_firstdatacol = nparity;
	rr->rr_abd_copy = NULL;

	asize = 0;

	for (c = 0; c < acols; c++) {
		col = f + c;
		coff = o;
		if (col >= dcols) {
			col -= dcols;
			coff += 1ULL << unit_shift;
		}
		rr->rr_col[c].rc_devidx = col;
		rr->rr_col[c].rc_offset = coff;
		rr->rr_col[c].rc_abd = NULL;
		rr->rr_col[c].rc_gdata = NULL;
		rr->rr_col[c].rc_orig_data = NULL;
		rr->rr_col[c].rc_error = 0;
		rr->rr_col[c].rc_tried = 0;
		rr->rr_col[c].rc_skipped = 0;
		rr->rr_col[c].rc_need_orig_restore = B_FALSE;

		if (c < bc)
			rr->rr_col[c].rc_size = (q + 1) << unit_shift;
		else
			rr->rr_col[c].rc_size = q << unit_shift;

		asize += rr->rr_col[c].rc_size;
	}

	ASSERT3U(asize, ==, tot << unit_shift);
	rm->rm_nskip = roundup(tot, nparity + 1) - tot;

	if (!dofree) {
		for (c = 0; c < rr->rr_firstdatacol; c++)
			rr->rr_col[c].rc_abd =
			    abd_alloc_linear(rr->rr_col[c].rc_size, B_TRUE);

		rr->rr_col[c].rc_abd = abd_get_offset(abd, 0);
		off = rr->rr_col[c].rc_size;

		for (c = c + 1; c < acols; c++) {
			rr->rr_col[c].rc_abd = abd_get_offset(abd, off);
			off += rr->rr_col[c].rc_size;
		}
	}

	/*
	 * If all data stored spans all columns, there's a danger that parity
	 * will always be on the same device and, since parity isn't read
	 * during normal operation, that that device's I/O bandwidth won't be
	 * used effectively. We therefore switch the parity every 1MB.
	 *
	 * ... at least that was, ostensibly, the theory. As a practical
	 * matter unless we juggle the parity between all devices evenly, we
	 * won't see any benefit. Further, occasional writes that aren't a
	 * multiple of the LCM of the number of children and the minimum
	 * stripe width are sufficient to avoid pessimal behavior.
	 * Unfortunately, this decision created an implicit on-disk format
	 * requirement that we need to support for all eternity, but only
	 * for single-parity RAID-Z.
	 *
	 * If we intend to skip a sector in the zeroth column for padding
	 * we must make sure to note this swap. We will never intend to
	 * skip the first column since at least one data and one parity
	 * column must appear in each row.
	 */
	ASSERT(rr->rr_cols >= 2);
	ASSERT(rr->rr_col[0].rc_size == rr->rr_col[1].rc_size);

	if (rr->rr_firstdatacol == 1 && (offset & (1ULL << 20))) {
		devidx = rr->rr_col[0].rc_devidx;
		o = rr->rr_col[0].rc_offset;
		rr->rr_col[0].rc_devidx = rr->rr_col[1].rc_devidx;
		rr->rr_col[0].rc_offset = rr->rr_col[1].rc_offset;
		rr->rr_col[1].rc_devidx = devidx;
		rr->rr_col[1].rc_offset = o;
	}

	return (rm);
}

static raidz_map_t *
vdev_raidz_map_alloc_expanded(abd_t *abd, uint64_t size, uint64_t offset, boolean_t dofree,
    uint64_t ashift, uint64_t physical_cols, uint64_t logical_cols,
    uint64_t nparity)
{
	/* The starting RAIDZ (parent) vdev sector of the block. */
	uint64_t b = offset >> ashift;
	/* The zio's size in units of the vdev's minimum sector size. */
	uint64_t s = size >> ashift;
	uint64_t cur_col = b % physical_cols;
	/* The starting byte offset on each child vdev. */
	uint64_t child_offset = (b / physical_cols) << ashift;
	uint64_t q, r, bc, devidx, asize, tot;

	/*
	 * "Quotient": The number of data sectors for this stripe on all but
	 * the "big column" child vdevs that also contain "remainder" data.
	 * AKA "full rows"
	 */
	q = s / (logical_cols - nparity);

	/*
	 * "Remainder": The number of partial stripe data sectors in this I/O.
	 * This will add a sector to some, but not all, child vdevs.
	 */
	r = s - q * (logical_cols - nparity);

	/* The number of "big columns" - those which contain remainder data. */
	bc = (r == 0 ? 0 : r + nparity);

	/*
	 * The total number of data and parity sectors associated with
	 * this I/O.
	 */
	tot = s + nparity * (q + (r == 0 ? 0 : 1));

	/* How many rows contain data (not skip) */
	uint64_t rows = howmany(tot, logical_cols);
	int cols = MIN(tot, logical_cols);

	raidz_map_t *rm =
	    kmem_zalloc(offsetof(raidz_map_t, rm_row[rows]),
	    KM_SLEEP);
	rm->rm_nrows = rows;
	rm->rm_nskip = roundup(tot, nparity + 1) - tot;
	asize = 0;

	zfs_dbgmsg("rm=%p s=%d q=%d r=%d bc=%d nrows=%d cols=%d",
	    rm, (int)s, (int)q, (int)r, (int)bc, (int)rows, (int)cols);

	for (uint64_t row = 0; row < rows; row++) {
		raidz_row_t *rr = kmem_alloc(offsetof(raidz_row_t,
		    rr_col[cols]), KM_SLEEP);
		rm->rm_row[row] = rr;

		/*
		 * We set cols to the entire width of the block, even
		 * if this row is shorter.  This is needed because parity
		 * generation (for Q and R) needs to know the entire width,
		 * because it treats the short row as though it was
		 * full-width (and the "phantom" sectors were zero-filled).
		 *
		 * Another approach to this would be to set cols shorter
		 * (to just the number of columns that we might do i/o to)
		 * and have another mechanism to tell the parity generation
		 * about the "entire width".  Reconstruction (at least
		 * vdev_raidz_reconstruct_general()) would also need to
		 * know about the "entire width".
		 */
		rr->rr_cols = cols;
		rr->rr_missingdata = 0;
		rr->rr_missingparity = 0;
		rr->rr_firstdatacol = nparity;
		rr->rr_abd_copy = NULL;

		for (int c = 0; c < rr->rr_cols; c++, cur_col++) {
			if (cur_col >= physical_cols) {
				cur_col -= physical_cols;
				child_offset += 1ULL << ashift;
			}
			rr->rr_col[c].rc_devidx = cur_col;
			rr->rr_col[c].rc_offset = child_offset;
			rr->rr_col[c].rc_gdata = NULL;
			rr->rr_col[c].rc_orig_data = NULL;
			rr->rr_col[c].rc_error = 0;
			rr->rr_col[c].rc_tried = 0;
			rr->rr_col[c].rc_skipped = 0;
			rr->rr_col[c].rc_abd = NULL;
			rr->rr_col[c].rc_need_orig_restore = B_FALSE;

			uint64_t dc = c - rr->rr_firstdatacol;
			if (c < rr->rr_firstdatacol) {
				rr->rr_col[c].rc_size = 1ULL << ashift;
				if (!dofree) {
					rr->rr_col[c].rc_abd =
					    abd_alloc_linear(rr->rr_col[c].rc_size,
					    B_TRUE);
				}
			} else if (row == rows - 1 && bc != 0 && c >= bc) {
				/*
				 * Past the end, this for parity generation.
				 */
				rr->rr_col[c].rc_size = 0;
				rr->rr_col[c].rc_abd = NULL;
			} else {
				/* XXX ASCII art diagram here */
				/* "data column" (col excluding parity) */
				uint64_t off;

				if (c < bc || r == 0) {
					off = dc * rows + row;
				} else {
					off = r * rows +
					    (dc - r) * (rows - 1) + row;
				}
				zfs_dbgmsg("rm=%p row=%d c=%d dc=%d off=%u devidx=%u",
				    rm, (int)row, (int)c, (int)dc, (int)off, (int)cur_col);
				rr->rr_col[c].rc_size = 1ULL << ashift;
				if (!dofree) {
					rr->rr_col[c].rc_abd =
					    abd_get_offset(abd, off << ashift);
				}
			}

			asize += rr->rr_col[c].rc_size;
		}

		/*
		 * If all data stored spans all columns, there's a danger that parity
		 * will always be on the same device and, since parity isn't read
		 * during normal operation, that that device's I/O bandwidth won't be
		 * used effectively. We therefore switch the parity every 1MB.
		 *
		 * ... at least that was, ostensibly, the theory. As a practical
		 * matter unless we juggle the parity between all devices evenly, we
		 * won't see any benefit. Further, occasional writes that aren't a
		 * multiple of the LCM of the number of children and the minimum
		 * stripe width are sufficient to avoid pessimal behavior.
		 * Unfortunately, this decision created an implicit on-disk format
		 * requirement that we need to support for all eternity, but only
		 * for single-parity RAID-Z.
		 *
		 * If we intend to skip a sector in the zeroth column for padding
		 * we must make sure to note this swap. We will never intend to
		 * skip the first column since at least one data and one parity
		 * column must appear in each row.
		 */
		if (rr->rr_firstdatacol == 1 && rr->rr_cols > 1 &&
		    (offset & (1ULL << 20))) {
			ASSERT(rr->rr_cols >= 2);
			ASSERT(rr->rr_col[0].rc_size == rr->rr_col[1].rc_size);
			devidx = rr->rr_col[0].rc_devidx;
			uint64_t o = rr->rr_col[0].rc_offset;
			rr->rr_col[0].rc_devidx = rr->rr_col[1].rc_devidx;
			rr->rr_col[0].rc_offset = rr->rr_col[1].rc_offset;
			rr->rr_col[1].rc_devidx = devidx;
			rr->rr_col[1].rc_offset = o;
		}

	}
	ASSERT3U(asize, ==, tot << ashift);

	return (rm);
}

struct pqr_struct {
	uint64_t *p;
	uint64_t *q;
	uint64_t *r;
};

static int
vdev_raidz_p_func(void *buf, size_t size, void *private)
{
	struct pqr_struct *pqr = private;
	const uint64_t *src = buf;
	int i, cnt = size / sizeof (src[0]);

	ASSERT(pqr->p && !pqr->q && !pqr->r);

	for (i = 0; i < cnt; i++, src++, pqr->p++)
		*pqr->p ^= *src;

	return (0);
}

static int
vdev_raidz_pq_func(void *buf, size_t size, void *private)
{
	struct pqr_struct *pqr = private;
	const uint64_t *src = buf;
	uint64_t mask;
	int i, cnt = size / sizeof (src[0]);

	ASSERT(pqr->p && pqr->q && !pqr->r);

	for (i = 0; i < cnt; i++, src++, pqr->p++, pqr->q++) {
		*pqr->p ^= *src;
		VDEV_RAIDZ_64MUL_2(*pqr->q, mask);
		*pqr->q ^= *src;
	}

	return (0);
}

static int
vdev_raidz_pqr_func(void *buf, size_t size, void *private)
{
	struct pqr_struct *pqr = private;
	const uint64_t *src = buf;
	uint64_t mask;
	int i, cnt = size / sizeof (src[0]);

	ASSERT(pqr->p && pqr->q && pqr->r);

	for (i = 0; i < cnt; i++, src++, pqr->p++, pqr->q++, pqr->r++) {
		*pqr->p ^= *src;
		VDEV_RAIDZ_64MUL_2(*pqr->q, mask);
		*pqr->q ^= *src;
		VDEV_RAIDZ_64MUL_4(*pqr->r, mask);
		*pqr->r ^= *src;
	}

	return (0);
}

static void
vdev_raidz_generate_parity_p(raidz_row_t *rr)
{
	uint64_t *p = abd_to_buf(rr->rr_col[VDEV_RAIDZ_P].rc_abd);

	for (int c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
		abd_t *src = rr->rr_col[c].rc_abd;

		if (c == rr->rr_firstdatacol) {
			abd_copy_to_buf(p, src, rr->rr_col[c].rc_size);
		} else {
			struct pqr_struct pqr = { p, NULL, NULL };
			(void) abd_iterate_func(src, 0, rr->rr_col[c].rc_size,
			    vdev_raidz_p_func, &pqr);
		}
	}
}

static void
vdev_raidz_generate_parity_pq(raidz_row_t *rr)
{
	uint64_t *p = abd_to_buf(rr->rr_col[VDEV_RAIDZ_P].rc_abd);
	uint64_t *q = abd_to_buf(rr->rr_col[VDEV_RAIDZ_Q].rc_abd);
	uint64_t pcnt = rr->rr_col[VDEV_RAIDZ_P].rc_size / sizeof (p[0]);
	ASSERT(rr->rr_col[VDEV_RAIDZ_P].rc_size ==
	    rr->rr_col[VDEV_RAIDZ_Q].rc_size);

	for (int c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
		abd_t *src = rr->rr_col[c].rc_abd;

		uint64_t ccnt = rr->rr_col[c].rc_size / sizeof (p[0]);

		if (c == rr->rr_firstdatacol) {
			abd_copy_to_buf(p, src, rr->rr_col[c].rc_size);
			(void) memcpy(q, p, rr->rr_col[c].rc_size);
		} else {
			struct pqr_struct pqr = { p, q, NULL };
			(void) abd_iterate_func(src, 0, rr->rr_col[c].rc_size,
			    vdev_raidz_pq_func, &pqr);
		}

		if (c == rr->rr_firstdatacol) {
			for (uint64_t i = ccnt; i < pcnt; i++) {
				p[i] = 0;
				q[i] = 0;
			}
		} else {
			/*
			 * Treat short columns as though they are full of 0s.
			 * Note that there's therefore nothing needed for P.
			 */
			uint64_t mask;
			for (uint64_t i = ccnt; i < pcnt; i++) {
				VDEV_RAIDZ_64MUL_2(q[i], mask);
			}
		}
	}
}

static void
vdev_raidz_generate_parity_pqr(raidz_row_t *rr)
{
	uint64_t *p = abd_to_buf(rr->rr_col[VDEV_RAIDZ_P].rc_abd);
	uint64_t *q = abd_to_buf(rr->rr_col[VDEV_RAIDZ_Q].rc_abd);
	uint64_t *r = abd_to_buf(rr->rr_col[VDEV_RAIDZ_R].rc_abd);
	uint64_t pcnt = rr->rr_col[VDEV_RAIDZ_P].rc_size / sizeof (p[0]);
	ASSERT(rr->rr_col[VDEV_RAIDZ_P].rc_size ==
	    rr->rr_col[VDEV_RAIDZ_Q].rc_size);
	ASSERT(rr->rr_col[VDEV_RAIDZ_P].rc_size ==
	    rr->rr_col[VDEV_RAIDZ_R].rc_size);

	for (int c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
		abd_t *src = rr->rr_col[c].rc_abd;

		uint64_t ccnt = rr->rr_col[c].rc_size / sizeof (p[0]);

		if (c == rr->rr_firstdatacol) {
			abd_copy_to_buf(p, src, rr->rr_col[c].rc_size);
			(void) memcpy(q, p, rr->rr_col[c].rc_size);
			(void) memcpy(r, p, rr->rr_col[c].rc_size);
		} else {
			struct pqr_struct pqr = { p, q, r };
			(void) abd_iterate_func(src, 0, rr->rr_col[c].rc_size,
			    vdev_raidz_pqr_func, &pqr);
		}

		if (c == rr->rr_firstdatacol) {
			for (uint64_t i = ccnt; i < pcnt; i++) {
				/* XXX does this really happen? firstdatacol should be the same size as the parity cols */
				p[i] = 0;
				q[i] = 0;
				r[i] = 0;
			}
		} else {
			/*
			 * Treat short columns as though they are full of 0s.
			 * Note that there's therefore nothing needed for P.
			 */
			uint64_t mask;
			for (uint64_t i = ccnt; i < pcnt; i++) {
				VDEV_RAIDZ_64MUL_2(q[i], mask);
				VDEV_RAIDZ_64MUL_4(r[i], mask);
			}
		}
	}
}

/*
 * Generate RAID parity in the first virtual columns according to the number of
 * parity columns available.
 */
static void
vdev_raidz_generate_parity(raidz_row_t *rr)
{
	if (rr->rr_cols == 0) {
		/*
		 * We are handling this block one row at a time (because
		 * this block has a different logical vs physical width,
		 * due to RAIDZ expansion), and this is a pad-only row,
		 * which has no parity.
		 */
		return;
	}

	switch (rr->rr_firstdatacol) {
	case 1:
		vdev_raidz_generate_parity_p(rr);
		break;
	case 2:
		vdev_raidz_generate_parity_pq(rr);
		break;
	case 3:
		vdev_raidz_generate_parity_pqr(rr);
		break;
	default:
		cmn_err(CE_PANIC, "invalid RAID-Z configuration");
	}
}

/* ARGSUSED */
static int
vdev_raidz_reconst_p_func(void *dbuf, void *sbuf, size_t size, void *private)
{
	uint64_t *dst = dbuf;
	uint64_t *src = sbuf;
	int cnt = size / sizeof (src[0]);

	for (int i = 0; i < cnt; i++) {
		dst[i] ^= src[i];
	}

	return (0);
}

/* ARGSUSED */
static int
vdev_raidz_reconst_q_pre_func(void *dbuf, void *sbuf, size_t size,
    void *private)
{
	uint64_t *dst = dbuf;
	uint64_t *src = sbuf;
	uint64_t mask;
	int cnt = size / sizeof (dst[0]);

	for (int i = 0; i < cnt; i++, dst++, src++) {
		VDEV_RAIDZ_64MUL_2(*dst, mask);
		*dst ^= *src;
	}

	return (0);
}

/* ARGSUSED */
static int
vdev_raidz_reconst_q_pre_tail_func(void *buf, size_t size, void *private)
{
	uint64_t *dst = buf;
	uint64_t mask;
	int cnt = size / sizeof (dst[0]);

	for (int i = 0; i < cnt; i++, dst++) {
		/* same operation as vdev_raidz_reconst_q_pre_func() on dst */
		VDEV_RAIDZ_64MUL_2(*dst, mask);
	}

	return (0);
}

struct reconst_q_struct {
	uint64_t *q;
	int exp;
};

static int
vdev_raidz_reconst_q_post_func(void *buf, size_t size, void *private)
{
	struct reconst_q_struct *rq = private;
	uint64_t *dst = buf;
	int cnt = size / sizeof (dst[0]);

	for (int i = 0; i < cnt; i++, dst++, rq->q++) {
		*dst ^= *rq->q;

		int j;
		uint8_t *b;
		for (j = 0, b = (uint8_t *)dst; j < 8; j++, b++) {
			*b = vdev_raidz_exp2(*b, rq->exp);
		}
	}

	return (0);
}

struct reconst_pq_struct {
	uint8_t *p;
	uint8_t *q;
	uint8_t *pxy;
	uint8_t *qxy;
	int aexp;
	int bexp;
};

static int
vdev_raidz_reconst_pq_func(void *xbuf, void *ybuf, size_t size, void *private)
{
	struct reconst_pq_struct *rpq = private;
	uint8_t *xd = xbuf;
	uint8_t *yd = ybuf;

	for (int i = 0; i < size;
	    i++, rpq->p++, rpq->q++, rpq->pxy++, rpq->qxy++, xd++, yd++) {
		*xd = vdev_raidz_exp2(*rpq->p ^ *rpq->pxy, rpq->aexp) ^
		    vdev_raidz_exp2(*rpq->q ^ *rpq->qxy, rpq->bexp);
		*yd = *rpq->p ^ *rpq->pxy ^ *xd;
	}

	return (0);
}

static int
vdev_raidz_reconst_pq_tail_func(void *xbuf, size_t size, void *private)
{
	struct reconst_pq_struct *rpq = private;
	uint8_t *xd = xbuf;

	for (int i = 0; i < size;
	    i++, rpq->p++, rpq->q++, rpq->pxy++, rpq->qxy++, xd++) {
		/* same operation as vdev_raidz_reconst_pq_func() on xd */
		*xd = vdev_raidz_exp2(*rpq->p ^ *rpq->pxy, rpq->aexp) ^
		    vdev_raidz_exp2(*rpq->q ^ *rpq->qxy, rpq->bexp);
	}

	return (0);
}

static int
vdev_raidz_reconstruct_p(raidz_row_t *rr, int *tgts, int ntgts)
{
	int x = tgts[0];
	abd_t *dst, *src;

	zfs_dbgmsg("reconstruct_p(rm=%p x=%u)",
	    rr, x);

	ASSERT3U(ntgts, ==, 1);
	ASSERT3U(x, >=, rr->rr_firstdatacol);
	ASSERT3U(x, <, rr->rr_cols);

	ASSERT3U(rr->rr_col[x].rc_size, <=, rr->rr_col[VDEV_RAIDZ_P].rc_size);

	src = rr->rr_col[VDEV_RAIDZ_P].rc_abd;
	dst = rr->rr_col[x].rc_abd;

	abd_copy(dst, src, rr->rr_col[x].rc_size);

	for (int c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
		uint64_t size = MIN(rr->rr_col[x].rc_size,
		    rr->rr_col[c].rc_size);

		src = rr->rr_col[c].rc_abd;
		dst = rr->rr_col[x].rc_abd; /* XXX not needed, done above */

		if (c == x)
			continue;

		(void) abd_iterate_func2(dst, src, 0, 0, size,
		    vdev_raidz_reconst_p_func, NULL);
	}

	return (1 << VDEV_RAIDZ_P);
}

static int
vdev_raidz_reconstruct_q(raidz_row_t *rr, int *tgts, int ntgts)
{
	int x = tgts[0];
	int c, exp;
	abd_t *dst, *src;

	zfs_dbgmsg("reconstruct_q(rm=%p x=%u)",
	    rr, x);

	ASSERT(ntgts == 1);

	ASSERT(rr->rr_col[x].rc_size <= rr->rr_col[VDEV_RAIDZ_Q].rc_size);

	for (c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
		uint64_t size = (c == x) ? 0 : MIN(rr->rr_col[x].rc_size,
		    rr->rr_col[c].rc_size);

		src = rr->rr_col[c].rc_abd;
		dst = rr->rr_col[x].rc_abd;

		if (c == rr->rr_firstdatacol) {
			abd_copy(dst, src, size);
			if (rr->rr_col[x].rc_size > size)
				abd_zero_off(dst, size,
				    rr->rr_col[x].rc_size - size);
		} else {
			ASSERT3U(size, <=, rr->rr_col[x].rc_size);
			(void) abd_iterate_func2(dst, src, 0, 0, size,
			    vdev_raidz_reconst_q_pre_func, NULL);
			(void) abd_iterate_func(dst,
			    size, rr->rr_col[x].rc_size - size,
			    vdev_raidz_reconst_q_pre_tail_func, NULL);
		}
	}

	src = rr->rr_col[VDEV_RAIDZ_Q].rc_abd;
	dst = rr->rr_col[x].rc_abd;
	exp = 255 - (rr->rr_cols - 1 - x);

	struct reconst_q_struct rq = { abd_to_buf(src), exp };
	(void) abd_iterate_func(dst, 0, rr->rr_col[x].rc_size,
	    vdev_raidz_reconst_q_post_func, &rq);

	return (1 << VDEV_RAIDZ_Q);
}

static int
vdev_raidz_reconstruct_pq(raidz_row_t *rr, int *tgts, int ntgts)
{
	uint8_t *p, *q, *pxy, *qxy, tmp, a, b, aexp, bexp;
	abd_t *pdata, *qdata;
	uint64_t xsize, ysize;
	int x = tgts[0];
	int y = tgts[1];
	abd_t *xd, *yd;

	zfs_dbgmsg("reconstruct_pq(rm=%p x=%u y=%u)",
	    rr, x, y);

	ASSERT(ntgts == 2);
	ASSERT(x < y);
	ASSERT(x >= rr->rr_firstdatacol);
	ASSERT(y < rr->rr_cols);

	ASSERT(rr->rr_col[x].rc_size >= rr->rr_col[y].rc_size);

	/*
	 * Move the parity data aside -- we're going to compute parity as
	 * though columns x and y were full of zeros -- Pxy and Qxy. We want to
	 * reuse the parity generation mechanism without trashing the actual
	 * parity so we make those columns appear to be full of zeros by
	 * setting their lengths to zero.
	 */
	pdata = rr->rr_col[VDEV_RAIDZ_P].rc_abd;
	qdata = rr->rr_col[VDEV_RAIDZ_Q].rc_abd;
	xsize = rr->rr_col[x].rc_size;
	ysize = rr->rr_col[y].rc_size;

	rr->rr_col[VDEV_RAIDZ_P].rc_abd =
	    abd_alloc_linear(rr->rr_col[VDEV_RAIDZ_P].rc_size, B_TRUE);
	rr->rr_col[VDEV_RAIDZ_Q].rc_abd =
	    abd_alloc_linear(rr->rr_col[VDEV_RAIDZ_Q].rc_size, B_TRUE);
	rr->rr_col[x].rc_size = 0;
	rr->rr_col[y].rc_size = 0;

	vdev_raidz_generate_parity_pq(rr);

	rr->rr_col[x].rc_size = xsize;
	rr->rr_col[y].rc_size = ysize;

	p = abd_to_buf(pdata);
	q = abd_to_buf(qdata);
	pxy = abd_to_buf(rr->rr_col[VDEV_RAIDZ_P].rc_abd);
	qxy = abd_to_buf(rr->rr_col[VDEV_RAIDZ_Q].rc_abd);
	xd = rr->rr_col[x].rc_abd;
	yd = rr->rr_col[y].rc_abd;

	/*
	 * We now have:
	 *	Pxy = P + D_x + D_y
	 *	Qxy = Q + 2^(ndevs - 1 - x) * D_x + 2^(ndevs - 1 - y) * D_y
	 *
	 * We can then solve for D_x:
	 *	D_x = A * (P + Pxy) + B * (Q + Qxy)
	 * where
	 *	A = 2^(x - y) * (2^(x - y) + 1)^-1
	 *	B = 2^(ndevs - 1 - x) * (2^(x - y) + 1)^-1
	 *
	 * With D_x in hand, we can easily solve for D_y:
	 *	D_y = P + Pxy + D_x
	 */

	a = vdev_raidz_pow2[255 + x - y];
	b = vdev_raidz_pow2[255 - (rr->rr_cols - 1 - x)];
	tmp = 255 - vdev_raidz_log2[a ^ 1];

	aexp = vdev_raidz_log2[vdev_raidz_exp2(a, tmp)];
	bexp = vdev_raidz_log2[vdev_raidz_exp2(b, tmp)];

	ASSERT3U(xsize, >=, ysize);
	struct reconst_pq_struct rpq = { p, q, pxy, qxy, aexp, bexp };
	(void) abd_iterate_func2(xd, yd, 0, 0, ysize,
	    vdev_raidz_reconst_pq_func, &rpq);
	(void) abd_iterate_func(xd, ysize, xsize - ysize,
	    vdev_raidz_reconst_pq_tail_func, &rpq);

	abd_free(rr->rr_col[VDEV_RAIDZ_P].rc_abd);
	abd_free(rr->rr_col[VDEV_RAIDZ_Q].rc_abd);

	/*
	 * Restore the saved parity data.
	 */
	rr->rr_col[VDEV_RAIDZ_P].rc_abd = pdata;
	rr->rr_col[VDEV_RAIDZ_Q].rc_abd = qdata;

	return ((1 << VDEV_RAIDZ_P) | (1 << VDEV_RAIDZ_Q));
}

/* BEGIN CSTYLED */
/*
 * In the general case of reconstruction, we must solve the system of linear
 * equations defined by the coeffecients used to generate parity as well as
 * the contents of the data and parity disks. This can be expressed with
 * vectors for the original data (D) and the actual data (d) and parity (p)
 * and a matrix composed of the identity matrix (I) and a dispersal matrix (V):
 *
 *            __   __                     __     __
 *            |     |         __     __   |  p_0  |
 *            |  V  |         |  D_0  |   | p_m-1 |
 *            |     |    x    |   :   | = |  d_0  |
 *            |  I  |         | D_n-1 |   |   :   |
 *            |     |         ~~     ~~   | d_n-1 |
 *            ~~   ~~                     ~~     ~~
 *
 * I is simply a square identity matrix of size n, and V is a vandermonde
 * matrix defined by the coeffecients we chose for the various parity columns
 * (1, 2, 4). Note that these values were chosen both for simplicity, speedy
 * computation as well as linear separability.
 *
 *      __               __               __     __
 *      |   1   ..  1 1 1 |               |  p_0  |
 *      | 2^n-1 ..  4 2 1 |   __     __   |   :   |
 *      | 4^n-1 .. 16 4 1 |   |  D_0  |   | p_m-1 |
 *      |   1   ..  0 0 0 |   |  D_1  |   |  d_0  |
 *      |   0   ..  0 0 0 | x |  D_2  | = |  d_1  |
 *      |   :       : : : |   |   :   |   |  d_2  |
 *      |   0   ..  1 0 0 |   | D_n-1 |   |   :   |
 *      |   0   ..  0 1 0 |   ~~     ~~   |   :   |
 *      |   0   ..  0 0 1 |               | d_n-1 |
 *      ~~               ~~               ~~     ~~
 *
 * Note that I, V, d, and p are known. To compute D, we must invert the
 * matrix and use the known data and parity values to reconstruct the unknown
 * data values. We begin by removing the rows in V|I and d|p that correspond
 * to failed or missing columns; we then make V|I square (n x n) and d|p
 * sized n by removing rows corresponding to unused parity from the bottom up
 * to generate (V|I)' and (d|p)'. We can then generate the inverse of (V|I)'
 * using Gauss-Jordan elimination. In the example below we use m=3 parity
 * columns, n=8 data columns, with errors in d_1, d_2, and p_1:
 *           __                               __
 *           |  1   1   1   1   1   1   1   1  |
 *           | 128  64  32  16  8   4   2   1  | <-----+-+-- missing disks
 *           |  19 205 116  29  64  16  4   1  |      / /
 *           |  1   0   0   0   0   0   0   0  |     / /
 *           |  0   1   0   0   0   0   0   0  | <--' /
 *  (V|I)  = |  0   0   1   0   0   0   0   0  | <---'
 *           |  0   0   0   1   0   0   0   0  |
 *           |  0   0   0   0   1   0   0   0  |
 *           |  0   0   0   0   0   1   0   0  |
 *           |  0   0   0   0   0   0   1   0  |
 *           |  0   0   0   0   0   0   0   1  |
 *           ~~                               ~~
 *           __                               __
 *           |  1   1   1   1   1   1   1   1  |
 *           |  19 205 116  29  64  16  4   1  |
 *           |  1   0   0   0   0   0   0   0  |
 *  (V|I)' = |  0   0   0   1   0   0   0   0  |
 *           |  0   0   0   0   1   0   0   0  |
 *           |  0   0   0   0   0   1   0   0  |
 *           |  0   0   0   0   0   0   1   0  |
 *           |  0   0   0   0   0   0   0   1  |
 *           ~~                               ~~
 *
 * Here we employ Gauss-Jordan elimination to find the inverse of (V|I)'. We
 * have carefully chosen the seed values 1, 2, and 4 to ensure that this
 * matrix is not singular.
 * __                                                                 __
 * |  1   1   1   1   1   1   1   1     1   0   0   0   0   0   0   0  |
 * |  19 205 116  29  64  16  4   1     0   1   0   0   0   0   0   0  |
 * |  1   0   0   0   0   0   0   0     0   0   1   0   0   0   0   0  |
 * |  0   0   0   1   0   0   0   0     0   0   0   1   0   0   0   0  |
 * |  0   0   0   0   1   0   0   0     0   0   0   0   1   0   0   0  |
 * |  0   0   0   0   0   1   0   0     0   0   0   0   0   1   0   0  |
 * |  0   0   0   0   0   0   1   0     0   0   0   0   0   0   1   0  |
 * |  0   0   0   0   0   0   0   1     0   0   0   0   0   0   0   1  |
 * ~~                                                                 ~~
 * __                                                                 __
 * |  1   0   0   0   0   0   0   0     0   0   1   0   0   0   0   0  |
 * |  1   1   1   1   1   1   1   1     1   0   0   0   0   0   0   0  |
 * |  19 205 116  29  64  16  4   1     0   1   0   0   0   0   0   0  |
 * |  0   0   0   1   0   0   0   0     0   0   0   1   0   0   0   0  |
 * |  0   0   0   0   1   0   0   0     0   0   0   0   1   0   0   0  |
 * |  0   0   0   0   0   1   0   0     0   0   0   0   0   1   0   0  |
 * |  0   0   0   0   0   0   1   0     0   0   0   0   0   0   1   0  |
 * |  0   0   0   0   0   0   0   1     0   0   0   0   0   0   0   1  |
 * ~~                                                                 ~~
 * __                                                                 __
 * |  1   0   0   0   0   0   0   0     0   0   1   0   0   0   0   0  |
 * |  0   1   1   0   0   0   0   0     1   0   1   1   1   1   1   1  |
 * |  0  205 116  0   0   0   0   0     0   1   19  29  64  16  4   1  |
 * |  0   0   0   1   0   0   0   0     0   0   0   1   0   0   0   0  |
 * |  0   0   0   0   1   0   0   0     0   0   0   0   1   0   0   0  |
 * |  0   0   0   0   0   1   0   0     0   0   0   0   0   1   0   0  |
 * |  0   0   0   0   0   0   1   0     0   0   0   0   0   0   1   0  |
 * |  0   0   0   0   0   0   0   1     0   0   0   0   0   0   0   1  |
 * ~~                                                                 ~~
 * __                                                                 __
 * |  1   0   0   0   0   0   0   0     0   0   1   0   0   0   0   0  |
 * |  0   1   1   0   0   0   0   0     1   0   1   1   1   1   1   1  |
 * |  0   0  185  0   0   0   0   0    205  1  222 208 141 221 201 204 |
 * |  0   0   0   1   0   0   0   0     0   0   0   1   0   0   0   0  |
 * |  0   0   0   0   1   0   0   0     0   0   0   0   1   0   0   0  |
 * |  0   0   0   0   0   1   0   0     0   0   0   0   0   1   0   0  |
 * |  0   0   0   0   0   0   1   0     0   0   0   0   0   0   1   0  |
 * |  0   0   0   0   0   0   0   1     0   0   0   0   0   0   0   1  |
 * ~~                                                                 ~~
 * __                                                                 __
 * |  1   0   0   0   0   0   0   0     0   0   1   0   0   0   0   0  |
 * |  0   1   1   0   0   0   0   0     1   0   1   1   1   1   1   1  |
 * |  0   0   1   0   0   0   0   0    166 100  4   40 158 168 216 209 |
 * |  0   0   0   1   0   0   0   0     0   0   0   1   0   0   0   0  |
 * |  0   0   0   0   1   0   0   0     0   0   0   0   1   0   0   0  |
 * |  0   0   0   0   0   1   0   0     0   0   0   0   0   1   0   0  |
 * |  0   0   0   0   0   0   1   0     0   0   0   0   0   0   1   0  |
 * |  0   0   0   0   0   0   0   1     0   0   0   0   0   0   0   1  |
 * ~~                                                                 ~~
 * __                                                                 __
 * |  1   0   0   0   0   0   0   0     0   0   1   0   0   0   0   0  |
 * |  0   1   0   0   0   0   0   0    167 100  5   41 159 169 217 208 |
 * |  0   0   1   0   0   0   0   0    166 100  4   40 158 168 216 209 |
 * |  0   0   0   1   0   0   0   0     0   0   0   1   0   0   0   0  |
 * |  0   0   0   0   1   0   0   0     0   0   0   0   1   0   0   0  |
 * |  0   0   0   0   0   1   0   0     0   0   0   0   0   1   0   0  |
 * |  0   0   0   0   0   0   1   0     0   0   0   0   0   0   1   0  |
 * |  0   0   0   0   0   0   0   1     0   0   0   0   0   0   0   1  |
 * ~~                                                                 ~~
 *                   __                               __
 *                   |  0   0   1   0   0   0   0   0  |
 *                   | 167 100  5   41 159 169 217 208 |
 *                   | 166 100  4   40 158 168 216 209 |
 *       (V|I)'^-1 = |  0   0   0   1   0   0   0   0  |
 *                   |  0   0   0   0   1   0   0   0  |
 *                   |  0   0   0   0   0   1   0   0  |
 *                   |  0   0   0   0   0   0   1   0  |
 *                   |  0   0   0   0   0   0   0   1  |
 *                   ~~                               ~~
 *
 * We can then simply compute D = (V|I)'^-1 x (d|p)' to discover the values
 * of the missing data.
 *
 * As is apparent from the example above, the only non-trivial rows in the
 * inverse matrix correspond to the data disks that we're trying to
 * reconstruct. Indeed, those are the only rows we need as the others would
 * only be useful for reconstructing data known or assumed to be valid. For
 * that reason, we only build the coefficients in the rows that correspond to
 * targeted columns.
 */
/* END CSTYLED */

static void
vdev_raidz_matrix_init(raidz_row_t *rr, int n, int nmap, int *map,
    uint8_t **rows)
{
	int i, j;
	int pow;

	ASSERT(n == rr->rr_cols - rr->rr_firstdatacol);

	/*
	 * Fill in the missing rows of interest.
	 */
	for (i = 0; i < nmap; i++) {
		ASSERT3S(0, <=, map[i]);
		ASSERT3S(map[i], <=, 2);

		pow = map[i] * n;
		if (pow > 255)
			pow -= 255;
		ASSERT(pow <= 255);

		for (j = 0; j < n; j++) {
			pow -= map[i];
			if (pow < 0)
				pow += 255;
			rows[i][j] = vdev_raidz_pow2[pow];
		}
	}
}

static void
vdev_raidz_matrix_invert(raidz_row_t *rr, int n, int nmissing, int *missing,
    uint8_t **rows, uint8_t **invrows, const uint8_t *used)
{
	int i, j, ii, jj;
	uint8_t log;

	/*
	 * Assert that the first nmissing entries from the array of used
	 * columns correspond to parity columns and that subsequent entries
	 * correspond to data columns.
	 */
	for (i = 0; i < nmissing; i++) {
		ASSERT3S(used[i], <, rr->rr_firstdatacol);
	}
	for (; i < n; i++) {
		ASSERT3S(used[i], >=, rr->rr_firstdatacol);
	}

	/*
	 * First initialize the storage where we'll compute the inverse rows.
	 */
	for (i = 0; i < nmissing; i++) {
		for (j = 0; j < n; j++) {
			invrows[i][j] = (i == j) ? 1 : 0;
		}
	}

	/*
	 * Subtract all trivial rows from the rows of consequence.
	 */
	for (i = 0; i < nmissing; i++) {
		for (j = nmissing; j < n; j++) {
			ASSERT3U(used[j], >=, rr->rr_firstdatacol);
			jj = used[j] - rr->rr_firstdatacol;
			ASSERT3S(jj, <, n);
			invrows[i][j] = rows[i][jj];
			rows[i][jj] = 0;
		}
	}

	/*
	 * For each of the rows of interest, we must normalize it and subtract
	 * a multiple of it from the other rows.
	 */
	for (i = 0; i < nmissing; i++) {
		for (j = 0; j < missing[i]; j++) {
			ASSERT0(rows[i][j]);
		}
		ASSERT3U(rows[i][missing[i]], !=, 0);

		/*
		 * Compute the inverse of the first element and multiply each
		 * element in the row by that value.
		 */
		log = 255 - vdev_raidz_log2[rows[i][missing[i]]];

		for (j = 0; j < n; j++) {
			rows[i][j] = vdev_raidz_exp2(rows[i][j], log);
			invrows[i][j] = vdev_raidz_exp2(invrows[i][j], log);
		}

		for (ii = 0; ii < nmissing; ii++) {
			if (i == ii)
				continue;

			ASSERT3U(rows[ii][missing[i]], !=, 0);

			log = vdev_raidz_log2[rows[ii][missing[i]]];

			for (j = 0; j < n; j++) {
				rows[ii][j] ^=
				    vdev_raidz_exp2(rows[i][j], log);
				invrows[ii][j] ^=
				    vdev_raidz_exp2(invrows[i][j], log);
			}
		}
	}

	/*
	 * Verify that the data that is left in the rows are properly part of
	 * an identity matrix.
	 */
	for (i = 0; i < nmissing; i++) {
		for (j = 0; j < n; j++) {
			if (j == missing[i]) {
				ASSERT3U(rows[i][j], ==, 1);
			} else {
				ASSERT0(rows[i][j]);
			}
		}
	}
}

static void
vdev_raidz_matrix_reconstruct(raidz_row_t *rr, int n, int nmissing,
    int *missing, uint8_t **invrows, const uint8_t *used)
{
	int i, j, x, cc, c;
	uint8_t *src;
	uint64_t ccount;
	uint8_t *dst[VDEV_RAIDZ_MAXPARITY];
	uint64_t dcount[VDEV_RAIDZ_MAXPARITY];
	uint8_t log = 0;
	uint8_t val;
	int ll;
	uint8_t *invlog[VDEV_RAIDZ_MAXPARITY];
	uint8_t *p, *pp;
	size_t psize;

	psize = sizeof (invlog[0][0]) * n * nmissing;
	p = kmem_alloc(psize, KM_SLEEP);

	for (pp = p, i = 0; i < nmissing; i++) {
		invlog[i] = pp;
		pp += n;
	}

	for (i = 0; i < nmissing; i++) {
		for (j = 0; j < n; j++) {
			ASSERT3U(invrows[i][j], !=, 0);
			invlog[i][j] = vdev_raidz_log2[invrows[i][j]];
		}
	}

	for (i = 0; i < n; i++) {
		c = used[i];
		ASSERT3U(c, <, rr->rr_cols);

		ccount = rr->rr_col[c].rc_size;
		ASSERT(ccount >= rr->rr_col[missing[0]].rc_size || i > 0);
		if (ccount == 0)
			continue;
		src = abd_to_buf(rr->rr_col[c].rc_abd);
		for (j = 0; j < nmissing; j++) {
			cc = missing[j] + rr->rr_firstdatacol;
			ASSERT3U(cc, >=, rr->rr_firstdatacol);
			ASSERT3U(cc, <, rr->rr_cols);
			ASSERT3U(cc, !=, c);

			dcount[j] = rr->rr_col[cc].rc_size;
			if (dcount[j] != 0)
				dst[j] = abd_to_buf(rr->rr_col[cc].rc_abd);
		}

		for (x = 0; x < ccount; x++, src++) {
			if (*src != 0)
				log = vdev_raidz_log2[*src];

			for (cc = 0; cc < nmissing; cc++) {
				if (x >= dcount[cc])
					continue;

				if (*src == 0) {
					val = 0;
				} else {
					if ((ll = log + invlog[cc][i]) >= 255)
						ll -= 255;
					val = vdev_raidz_pow2[ll];
				}

				if (i == 0)
					dst[cc][x] = val;
				else
					dst[cc][x] ^= val;
			}
		}
	}

	kmem_free(p, psize);
}

static int
vdev_raidz_reconstruct_general(raidz_row_t *rr, int *tgts, int ntgts)
{
	int n, i, c, t, tt;
	int nmissing_rows;
	int missing_rows[VDEV_RAIDZ_MAXPARITY];
	int parity_map[VDEV_RAIDZ_MAXPARITY];

	zfs_dbgmsg("reconstruct_general(rm=%p ntgts=%u)",
	    rr, ntgts);
	uint8_t *p, *pp;
	size_t psize;

	uint8_t *rows[VDEV_RAIDZ_MAXPARITY];
	uint8_t *invrows[VDEV_RAIDZ_MAXPARITY];
	uint8_t *used;

	abd_t **bufs = NULL;

	int code = 0;

	/*
	 * Matrix reconstruction can't use scatter ABDs yet, so we allocate
	 * temporary linear ABDs.
	 */
	if (!abd_is_linear(rr->rr_col[rr->rr_firstdatacol].rc_abd)) {
		bufs = kmem_alloc(rr->rr_cols * sizeof (abd_t *), KM_PUSHPAGE);

		for (c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
			raidz_col_t *col = &rr->rr_col[c];

			bufs[c] = col->rc_abd;
			if (bufs[c] != NULL) {
                                col->rc_abd =
                                    abd_alloc_linear(col->rc_size, B_TRUE);
                                abd_copy(col->rc_abd, bufs[c], col->rc_size);
			}
		}
	}

	n = rr->rr_cols - rr->rr_firstdatacol;

	/*
	 * Figure out which data columns are missing.
	 */
	nmissing_rows = 0;
	for (t = 0; t < ntgts; t++) {
		if (tgts[t] >= rr->rr_firstdatacol) {
			missing_rows[nmissing_rows++] =
			    tgts[t] - rr->rr_firstdatacol;
		}
	}

	/*
	 * Figure out which parity columns to use to help generate the missing
	 * data columns.
	 */
	for (tt = 0, c = 0, i = 0; i < nmissing_rows; c++) {
		ASSERT(tt < ntgts);
		ASSERT(c < rr->rr_firstdatacol);

		/*
		 * Skip any targeted parity columns.
		 */
		if (c == tgts[tt]) {
			tt++;
			continue;
		}

		code |= 1 << c;

		parity_map[i] = c;
		i++;
	}

	ASSERT(code != 0);
	ASSERT3U(code, <, 1 << VDEV_RAIDZ_MAXPARITY);

	psize = (sizeof (rows[0][0]) + sizeof (invrows[0][0])) *
	    nmissing_rows * n + sizeof (used[0]) * n;
	p = kmem_alloc(psize, KM_SLEEP);

	for (pp = p, i = 0; i < nmissing_rows; i++) {
		rows[i] = pp;
		pp += n;
		invrows[i] = pp;
		pp += n;
	}
	used = pp;

	for (i = 0; i < nmissing_rows; i++) {
		used[i] = parity_map[i];
	}

	for (tt = 0, c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
		if (tt < nmissing_rows &&
		    c == missing_rows[tt] + rr->rr_firstdatacol) {
			tt++;
			continue;
		}

		ASSERT3S(i, <, n);
		used[i] = c;
		i++;
	}

	/*
	 * Initialize the interesting rows of the matrix.
	 */
	vdev_raidz_matrix_init(rr, n, nmissing_rows, parity_map, rows);

	/*
	 * Invert the matrix.
	 */
	vdev_raidz_matrix_invert(rr, n, nmissing_rows, missing_rows, rows,
	    invrows, used);

	/*
	 * Reconstruct the missing data using the generated matrix.
	 */
	vdev_raidz_matrix_reconstruct(rr, n, nmissing_rows, missing_rows,
	    invrows, used);

	kmem_free(p, psize);

	/*
	 * copy back from temporary linear abds and free them
	 */
	if (bufs) {
		for (c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
			raidz_col_t *col = &rr->rr_col[c];

			if (bufs[c] != NULL) {
                                abd_copy(bufs[c], col->rc_abd, col->rc_size);
                                abd_free(col->rc_abd);
			}
			col->rc_abd = bufs[c];
		}
		kmem_free(bufs, rr->rr_cols * sizeof (abd_t *));
	}

	return (code);
}

static int
vdev_raidz_reconstruct_row(raidz_row_t *rr, int *t, int nt)
{
	int tgts[VDEV_RAIDZ_MAXPARITY], *dt;
	int ntgts;
	int i, c;
	int code;
	int nbadparity, nbaddata;
	int parity_valid[VDEV_RAIDZ_MAXPARITY];

	zfs_dbgmsg("reconstruct(rm=%p nt=%u cols=%u md=%u mp=%u)",
	    rr, nt, (int)rr->rr_cols, (int)rr->rr_missingdata, (int)rr->rr_missingparity);

	/*
	 * The tgts list must already be sorted.
	 */
	zfs_dbgmsg("reconstruct(rm=%p t[%u]=%u)", rr, 0, t[0]);
	for (i = 1; i < nt; i++) {
		zfs_dbgmsg("reconstruct(rm=%p t[%u]=%u)",
		    rr, i, t[i]);
		ASSERT(t[i] > t[i - 1]);
	}

	nbadparity = rr->rr_firstdatacol;
	nbaddata = rr->rr_cols - nbadparity;
	ntgts = 0;
	for (i = 0, c = 0; c < rr->rr_cols; c++) {
		zfs_dbgmsg("reconstruct(rm=%p col=%u devid=%u offset=%llx error=%u)",
		    rr, c,
		    (int)rr->rr_col[c].rc_devidx,
		    (long long)rr->rr_col[c].rc_offset,
		    (int)rr->rr_col[c].rc_error);
		if (c < rr->rr_firstdatacol)
			parity_valid[c] = B_FALSE;

		if (i < nt && c == t[i]) {
			tgts[ntgts++] = c;
			i++;
		} else if (rr->rr_col[c].rc_error != 0) {
			tgts[ntgts++] = c;
		} else if (c >= rr->rr_firstdatacol) {
			nbaddata--;
		} else {
			parity_valid[c] = B_TRUE;
			nbadparity--;
		}
	}

	ASSERT(ntgts >= nt);
	ASSERT(nbaddata >= 0);
	ASSERT(nbaddata + nbadparity == ntgts);

	dt = &tgts[nbadparity];

	/*
	 * See if we can use any of our optimized reconstruction routines.
	 */
	if (!vdev_raidz_default_to_general) {
		switch (nbaddata) {
		case 1:
			if (parity_valid[VDEV_RAIDZ_P])
				return (vdev_raidz_reconstruct_p(rr, dt, 1));

			ASSERT(rr->rr_firstdatacol > 1);

			if (parity_valid[VDEV_RAIDZ_Q])
				return (vdev_raidz_reconstruct_q(rr, dt, 1));

			ASSERT(rr->rr_firstdatacol > 2);
			break;

		case 2:
			ASSERT(rr->rr_firstdatacol > 1);

			if (parity_valid[VDEV_RAIDZ_P] &&
			    parity_valid[VDEV_RAIDZ_Q])
				return (vdev_raidz_reconstruct_pq(rr, dt, 2));

			ASSERT(rr->rr_firstdatacol > 2);

			break;
		}
	}

	code = vdev_raidz_reconstruct_general(rr, tgts, ntgts);
	ASSERT(code < (1 << VDEV_RAIDZ_MAXPARITY));
	ASSERT(code > 0);
	return (code);
}

static int
vdev_raidz_open(vdev_t *vd, uint64_t *asize, uint64_t *max_asize,
    uint64_t *logical_ashift, uint64_t *physical_ashift)
{
	vdev_raidz_t *vdrz = vd->vdev_tsd;
	uint64_t nparity = vdrz->vd_nparity;
	int c;
	int lasterror = 0;
	int numerrors = 0;

	ASSERT(nparity > 0);

	if (nparity > VDEV_RAIDZ_MAXPARITY ||
	    vd->vdev_children < nparity + 1) {
		vd->vdev_stat.vs_aux = VDEV_AUX_BAD_LABEL;
		return (SET_ERROR(EINVAL));
	}

	vdev_open_children(vd);

	for (c = 0; c < vd->vdev_children; c++) {
		vdev_t *cvd = vd->vdev_child[c];

		if (cvd->vdev_open_error != 0) {
			lasterror = cvd->vdev_open_error;
			numerrors++;
			continue;
		}

		*asize = MIN(*asize - 1, cvd->vdev_asize - 1) + 1;
		*max_asize = MIN(*max_asize - 1, cvd->vdev_max_asize - 1) + 1;
		*logical_ashift = MAX(*logical_ashift, cvd->vdev_ashift);
		*physical_ashift = MAX(*physical_ashift,
		    cvd->vdev_physical_ashift);
	}

	*asize *= vd->vdev_children;
	*max_asize *= vd->vdev_children;

	if (numerrors > nparity) {
		vd->vdev_stat.vs_aux = VDEV_AUX_NO_REPLICAS;
		return (lasterror);
	}

	return (0);
}

static void
vdev_raidz_close(vdev_t *vd)
{
	int c;

	for (c = 0; c < vd->vdev_children; c++)
		vdev_close(vd->vdev_child[c]);
}

#ifdef illumos
/*
 * Handle a read or write I/O to a RAID-Z dump device.
 *
 * The dump device is in a unique situation compared to other ZFS datasets:
 * writing to this device should be as simple and fast as possible.  In
 * addition, durability matters much less since the dump will be extracted
 * once the machine reboots.  For that reason, this function eschews parity for
 * performance and simplicity.  The dump device uses the checksum setting
 * ZIO_CHECKSUM_NOPARITY to indicate that parity is not maintained for this
 * dataset.
 *
 * Blocks of size 128 KB have been preallocated for this volume.  I/Os less than
 * 128 KB will not fill an entire block; in addition, they may not be properly
 * aligned.  In that case, this function uses the preallocated 128 KB block and
 * omits reading or writing any "empty" portions of that block, as opposed to
 * allocating a fresh appropriately-sized block.
 *
 * Looking at an example of a 32 KB I/O to a RAID-Z vdev with 5 child vdevs:
 *
 *     vdev_raidz_io_start(data, size: 32 KB, offset: 64 KB)
 *
 * If this were a standard RAID-Z dataset, a block of at least 40 KB would be
 * allocated which spans all five child vdevs.  8 KB of data would be written to
 * each of four vdevs, with the fifth containing the parity bits.
 *
 *       parity    data     data     data     data
 *     |   PP   |   XX   |   XX   |   XX   |   XX   |
 *         ^        ^        ^        ^        ^
 *         |        |        |        |        |
 *   8 KB parity    ------8 KB data blocks------
 *
 * However, when writing to the dump device, the behavior is different:
 *
 *     vdev_raidz_physio(data, size: 32 KB, offset: 64 KB)
 *
 * Unlike the normal RAID-Z case in which the block is allocated based on the
 * I/O size, reads and writes here always use a 128 KB logical I/O size.  If the
 * I/O size is less than 128 KB, only the actual portions of data are written.
 * In this example the data is written to the third data vdev since that vdev
 * contains the offset [64 KB, 96 KB).
 *
 *       parity    data     data     data     data
 *     |        |        |        |   XX   |        |
 *                                    ^
 *                                    |
 *                             32 KB data block
 *
 * As a result, an individual I/O may not span all child vdevs; moreover, a
 * small I/O may only operate on a single child vdev.
 *
 * Note that since there are no parity bits calculated or written, this format
 * remains the same no matter how many parity bits are used in a normal RAID-Z
 * stripe.  On a RAID-Z3 configuration with seven child vdevs, the example above
 * would look like:
 *
 *       parity   parity   parity    data     data     data     data
 *     |        |        |        |        |        |   XX   |        |
 *                                                      ^
 *                                                      |
 *                                               32 KB data block
 */
int
vdev_raidz_physio(vdev_t *vd, caddr_t data, size_t size,
    uint64_t offset, uint64_t origoffset, boolean_t doread, boolean_t isdump)
{
	vdev_raidz_t *vdrz = vd->vdev_tsd;
	vdev_t *tvd = vd->vdev_top;
	vdev_t *cvd;
	raidz_row_t *rr;
	raidz_col_t *rc;
	int c, err = 0;

	uint64_t start, end, colstart, colend;
	uint64_t coloffset, colsize, colskip;

	int flags = doread ? BIO_READ : BIO_WRITE;

#ifdef	_KERNEL

	/*
	 * Don't write past the end of the block
	 */
	VERIFY3U(offset + size, <=, origoffset + SPA_OLD_MAXBLOCKSIZE);

	start = offset;
	end = start + size;

	/*
	 * Allocate a RAID-Z map for this block.  Note that this block starts
	 * from the "original" offset, this is, the offset of the extent which
	 * contains the requisite offset of the data being read or written.
	 *
	 * Even if this I/O operation doesn't span the full block size, let's
	 * treat the on-disk format as if the only blocks are the complete 128
	 * KB size.
	 */
	abd_t *abd = abd_get_from_buf(data - (offset - origoffset),
	    SPA_OLD_MAXBLOCKSIZE);
	/*
	 * XXX deal with dump to expanded raidz
	 */
	raidz_map_t *rm = vdev_raidz_map_alloc(abd,
	    SPA_OLD_MAXBLOCKSIZE, origoffset, B_FALSE, tvd->vdev_ashift,
	    vd->vdev_children, vdrz->vd_nparity);
	rr = rm->rm_row[0];

	coloffset = origoffset;

	for (c = rr->rr_firstdatacol; c < rr->rr_cols;
	    c++, coloffset += rc->rc_size) {
		rc = &rr->rr_col[c];
		cvd = vd->vdev_child[rc->rc_devidx];

		/*
		 * Find the start and end of this column in the RAID-Z map,
		 * keeping in mind that the stated size and offset of the
		 * operation may not fill the entire column for this vdev.
		 *
		 * If any portion of the data spans this column, issue the
		 * appropriate operation to the vdev.
		 */
		if (coloffset + rc->rc_size <= start)
			continue;
		if (coloffset >= end)
			continue;

		colstart = MAX(coloffset, start);
		colend = MIN(end, coloffset + rc->rc_size);
		colsize = colend - colstart;
		colskip = colstart - coloffset;

		VERIFY3U(colsize, <=, rc->rc_size);
		VERIFY3U(colskip, <=, rc->rc_size);

		/*
		 * Note that the child vdev will have a vdev label at the start
		 * of its range of offsets, hence the need for
		 * VDEV_LABEL_OFFSET().  See zio_vdev_child_io() for another
		 * example of why this calculation is needed.
		 */
		if ((err = vdev_disk_physio(cvd,
		    ((char *)abd_to_buf(rc->rc_abd)) + colskip, colsize,
		    VDEV_LABEL_OFFSET(rc->rc_offset) + colskip,
		    flags, isdump)) != 0)
			break;
	}

	vdev_raidz_row_free(rr);
	abd_put(abd);
#endif	/* KERNEL */

	return (err);
}
#endif

static uint64_t
vdev_raidz_asize(vdev_t *vd, uint64_t psize)
{
	vdev_raidz_t *vdrz = vd->vdev_tsd;
	uint64_t asize;
	uint64_t ashift = vd->vdev_top->vdev_ashift;
	uint64_t cols = vdrz->vd_logical_width;
	uint64_t nparity = vdrz->vd_nparity;

	asize = ((psize - 1) >> ashift) + 1;
	asize += nparity * ((asize + cols - nparity - 1) / (cols - nparity));
	asize = roundup(asize, nparity + 1) << ashift;

	return (asize);
}

static void
vdev_raidz_child_done(zio_t *zio)
{
	raidz_col_t *rc = zio->io_private;

	rc->rc_error = zio->io_error;
	rc->rc_tried = 1;
	rc->rc_skipped = 0;
}

static void
vdev_raidz_io_verify(zio_t *zio, raidz_row_t *rr, int col)
{
#if 0
#ifdef ZFS_DEBUG
	vdev_t *vd = zio->io_vd;
	vdev_t *tvd = vd->vdev_top;

	range_seg_t logical_rs, physical_rs;
	logical_rs.rs_start = zio->io_offset;
	logical_rs.rs_end = logical_rs.rs_start +
	    vdev_raidz_asize(zio->io_vd, zio->io_size);

	raidz_col_t *rc = &rr->rr_col[col];
	vdev_t *cvd = vd->vdev_child[rc->rc_devidx];

	vdev_xlate(cvd, &logical_rs, &physical_rs);
	ASSERT3U(rc->rc_offset, ==, physical_rs.rs_start);
	ASSERT3U(rc->rc_offset, <, physical_rs.rs_end);
	/*
	 * It would be nice to assert that rs_end is equal
	 * to rc_offset + rc_size but there might be an
	 * optional I/O at the end that is not accounted in
	 * rc_size.
	 */
	if (physical_rs.rs_end > rc->rc_offset + rc->rc_size) {
		ASSERT3U(physical_rs.rs_end, ==, rc->rc_offset +
		    rc->rc_size + (1 << tvd->vdev_ashift));
	} else {
		ASSERT3U(physical_rs.rs_end, ==, rc->rc_offset + rc->rc_size);
	}
#endif
#endif
}

static void
vdev_raidz_io_start_free(zio_t *zio, raidz_row_t *rr)
{
	vdev_t *vd = zio->io_vd;
	vdev_t *tvd = vd->vdev_top;

	for (int c = 0; c < rr->rr_cols; c++) {
		raidz_col_t *rc = &rr->rr_col[c];
		if (rc->rc_size == 0)
			continue;
		vdev_t *cvd = vd->vdev_child[rc->rc_devidx];

		zio_nowait(zio_vdev_child_io(zio, NULL, cvd,
		    rc->rc_offset, rc->rc_abd, rc->rc_size,
		    zio->io_type, zio->io_priority, 0,
		    vdev_raidz_child_done, rc));
	}
}

static void
vdev_raidz_io_start_write(zio_t *zio, raidz_row_t *rr)
{
	vdev_t *vd = zio->io_vd;
	vdev_t *tvd = vd->vdev_top;

	vdev_raidz_generate_parity(rr);

	for (int c = 0; c < rr->rr_cols; c++) {
		raidz_col_t *rc = &rr->rr_col[c];
		if (rc->rc_size == 0)
			continue;
		vdev_t *cvd = vd->vdev_child[rc->rc_devidx];

		zio_nowait(zio_vdev_child_io(zio, NULL, cvd,
		    rc->rc_offset, rc->rc_abd, rc->rc_size,
		    zio->io_type, zio->io_priority, 0,
		    vdev_raidz_child_done, rc));
	}

	/* XXX do this in vdev_raidz_io_start, based on nskip stored in rm
	 */
#if 0
	/*
	 * Generate optional I/Os for any skipped sectors to improve
	 * aggregation contiguity.
	 */
	for (int c = rr->rm_skipstart, i = 0; i < rr->rm_nskip; c++, i++) {
		ASSERT(c <= rr->rm_scols);
		if (c == rr->rm_scols)
			c = 0;
		raidz_col_t *rc = &rr->rr_col[c];
		vdev_t *cvd = vd->vdev_child[rc->rc_devidx];
		zio_nowait(zio_vdev_child_io(zio, NULL, cvd,
		    rc->rc_offset + rc->rc_size, NULL,
		    1 << tvd->vdev_ashift,
		    zio->io_type, zio->io_priority,
		    ZIO_FLAG_NODATA | ZIO_FLAG_OPTIONAL, NULL, NULL));
	}
#endif
}

static void
vdev_raidz_io_start_read(zio_t *zio, raidz_row_t *rr, boolean_t forceparity)
{
	vdev_t *vd = zio->io_vd;

	/*
	 * Iterate over the columns in reverse order so that we hit the parity
	 * last -- any errors along the way will force us to read the parity.
	 */
	for (int c = rr->rr_cols - 1; c >= 0; c--) {
		raidz_col_t *rc = &rr->rr_col[c];
		if (rc->rc_size == 0)
			continue;
		vdev_t *cvd = vd->vdev_child[rc->rc_devidx];
		if (!vdev_readable(cvd)) {
			if (c >= rr->rr_firstdatacol)
				rr->rr_missingdata++;
			else
				rr->rr_missingparity++;
			rc->rc_error = SET_ERROR(ENXIO);
			rc->rc_tried = 1;	/* don't even try */
			rc->rc_skipped = 1;
			continue;
		}
		if (vdev_dtl_contains(cvd, DTL_MISSING, zio->io_txg, 1)) {
			if (c >= rr->rr_firstdatacol)
				rr->rr_missingdata++;
			else
				rr->rr_missingparity++;
			rc->rc_error = SET_ERROR(ESTALE);
			rc->rc_skipped = 1;
			continue;
		}
		if (forceparity ||
		    c >= rr->rr_firstdatacol || rr->rr_missingdata > 0 ||
		    (zio->io_flags & (ZIO_FLAG_SCRUB | ZIO_FLAG_RESILVER))) {
			zio_nowait(zio_vdev_child_io(zio, NULL, cvd,
			    rc->rc_offset, rc->rc_abd, rc->rc_size,
			    zio->io_type, zio->io_priority, 0,
			    vdev_raidz_child_done, rc));
		}
	}
}

/*
 * Start an IO operation on a RAIDZ VDev
 *
 * Outline:
 * - For write operations:
 *   1. Generate the parity data
 *   2. Create child zio write operations to each column's vdev, for both
 *      data and parity.
 *   3. If the column skips any sectors for padding, create optional dummy
 *      write zio children for those areas to improve aggregation continuity.
 * - For read operations:
 *   1. Create child zio read operations to each data column's vdev to read
 *      the range of data required for zio.
 *   2. If this is a scrub or resilver operation, or if any of the data
 *      vdevs have had errors, then create zio read operations to the parity
 *      columns' VDevs as well.
 */
static void
vdev_raidz_io_start(zio_t *zio)
{
	vdev_t *vd = zio->io_vd;
	vdev_t *tvd = vd->vdev_top;
	vdev_raidz_t *vdrz = vd->vdev_tsd;
	raidz_map_t *rm;

	ASSERT(!vdrz->vn_expanding);

	if (vdrz->vd_logical_width != vdrz->vd_physical_width) {
                rm = vdev_raidz_map_alloc_expanded(zio->io_abd,
		    zio->io_size, zio->io_offset,
		    zio->io_type == ZIO_TYPE_FREE,
                    tvd->vdev_ashift, vdrz->vd_physical_width,
                    vdrz->vd_logical_width, vdrz->vd_nparity);
	} else {
                rm = vdev_raidz_map_alloc(zio->io_abd,
                    zio->io_size, zio->io_offset,
		    zio->io_type == ZIO_TYPE_FREE,
                    tvd->vdev_ashift, vdrz->vd_logical_width,
                    vdrz->vd_nparity);
	}

        zio->io_vsd = rm;
        zio->io_vsd_ops = &vdev_raidz_vsd_ops;

	if (zio->io_type == ZIO_TYPE_FREE) {
		for (int i = 0; i < rm->rm_nrows; i++) {
			vdev_raidz_io_start_free(zio, rm->rm_row[i]);
		}
	} else if (zio->io_type == ZIO_TYPE_WRITE) {
                for (int i = 0; i < rm->rm_nrows; i++) {
                        vdev_raidz_io_start_write(zio,
                            rm->rm_row[i]);
                }
        } else {
                ASSERT(zio->io_type == ZIO_TYPE_READ);
                /*
                 * If there are multiple rows, we will be hitting
                 * all disks, so go ahead and read the parity so
                 * that we are reading in decent size chunks.
                 * XXX maybe doesn't really matter?
                 */
                boolean_t forceparity = rm->rm_nrows > 1;
                for (int i = 0; i < rm->rm_nrows; i++) {
                        vdev_raidz_io_start_read(zio,
                            rm->rm_row[i], forceparity);
                }
        }

	zio_execute(zio);
}


/*
 * Report a checksum error for a child of a RAID-Z device.
 */
static void
raidz_checksum_error(zio_t *zio, raidz_col_t *rc, void *bad_data)
{
	void *buf;
	vdev_t *vd = zio->io_vd->vdev_child[rc->rc_devidx];

	if (!(zio->io_flags & ZIO_FLAG_SPECULATIVE)) {
		zio_bad_cksum_t zbc;
		raidz_map_t *rm = zio->io_vsd;

		mutex_enter(&vd->vdev_stat_lock);
		vd->vdev_stat.vs_checksum_errors++;
		mutex_exit(&vd->vdev_stat_lock);

		zbc.zbc_has_cksum = 0;
		zbc.zbc_injected = rm->rm_ecksuminjected;

		buf = abd_borrow_buf_copy(rc->rc_abd, rc->rc_size);
		zfs_ereport_post_checksum(zio->io_spa, vd, zio,
		    rc->rc_offset, rc->rc_size, buf, bad_data,
		    &zbc);
		abd_return_buf(rc->rc_abd, buf, rc->rc_size);
	}
}

/*
 * We keep track of whether or not there were any injected errors, so that
 * any ereports we generate can note it.
 */
static int
raidz_checksum_verify(zio_t *zio)
{
	zio_bad_cksum_t zbc;
	raidz_map_t *rm = zio->io_vsd;

	int ret = zio_checksum_error(zio, &zbc);
	if (ret != 0 && zbc.zbc_injected != 0)
		rm->rm_ecksuminjected = 1;

	return (ret);
}

/*
 * Generate the parity from the data columns. If we tried and were able to
 * read the parity without error, verify that the generated parity matches the
 * data we read. If it doesn't, we fire off a checksum error. Return the
 * number of such failures.
 */
static int
raidz_parity_verify(zio_t *zio, raidz_row_t *rr)
{
	void *orig[VDEV_RAIDZ_MAXPARITY];
	int c, ret = 0;
	raidz_col_t *rc;

	blkptr_t *bp = zio->io_bp;
	enum zio_checksum checksum = (bp == NULL ? zio->io_prop.zp_checksum :
	    (BP_IS_GANG(bp) ? ZIO_CHECKSUM_GANG_HEADER : BP_GET_CHECKSUM(bp)));

	if (checksum == ZIO_CHECKSUM_NOPARITY)
		return (ret);

	for (c = 0; c < rr->rr_firstdatacol; c++) {
		rc = &rr->rr_col[c];
		if (!rc->rc_tried || rc->rc_error != 0)
			continue;
		orig[c] = zio_buf_alloc(rc->rc_size);
		abd_copy_to_buf(orig[c], rc->rc_abd, rc->rc_size);
	}

	/* XXX regenerates parity even for !tried||rc_error!=0
	 * This could cause a side effect of fixing stuff we didn't realize
	 * was necessary (i.e. even if we return 0)
	 */
	vdev_raidz_generate_parity(rr);

	for (c = 0; c < rr->rr_firstdatacol; c++) {
		rc = &rr->rr_col[c];

		if (!rc->rc_tried || rc->rc_error != 0)
			continue;

		if (abd_cmp_buf(rc->rc_abd, orig[c], rc->rc_size) != 0) {
			zfs_dbgmsg("raidz_parity_verify found error on col=%u devidx=%u",
			    c, (int)rc->rc_devidx);
			raidz_checksum_error(zio, rc, orig[c]);
			rc->rc_error = SET_ERROR(ECKSUM);
			ret++;
		}
		zio_buf_free(orig[c], rc->rc_size);
	}

	return (ret);
}

/*
 * Keep statistics on all the ways that we used parity to correct data.
 */
static uint64_t raidz_corrected[1 << VDEV_RAIDZ_MAXPARITY];

static int
vdev_raidz_worst_error(raidz_row_t *rr)
{
	int error = 0;

	for (int c = 0; c < rr->rr_cols; c++)
		error = zio_worst_error(error, rr->rr_col[c].rc_error);

	return (error);
}

static void
vdev_raidz_io_done_verified(zio_t *zio, raidz_row_t *rr)
{
	int unexpected_errors = 0;
	int parity_errors = 0;
	int parity_untried = 0;
	int data_errors = 0;

	ASSERT3U(zio->io_type, ==, ZIO_TYPE_READ);

	for (int c = 0; c < rr->rr_cols; c++) {
		raidz_col_t *rc = &rr->rr_col[c];

		if (rc->rc_error) {
			if (c < rr->rr_firstdatacol)
				parity_errors++;
			else
				data_errors++;

			if (!rc->rc_skipped)
				unexpected_errors++;
		} else if (c < rr->rr_firstdatacol && !rc->rc_tried) {
			parity_untried++;
		}
	}

	/*
	 * If we read more parity disks than were used for
	 * reconstruction, confirm that the other parity disks produced
	 * correct data.
	 *
	 * Note that we also regenerate parity when resilvering so we
	 * can write it out to failed devices later.
	 */
	zfs_dbgmsg("parity_errors=%u parity_untried=%u data_errors=%u verifying=%s",
	    parity_errors, parity_untried, data_errors,
	    (parity_errors + parity_untried < rr->rr_firstdatacol - data_errors) ? "yes" : "no");
	if (parity_errors + parity_untried <
	    rr->rr_firstdatacol - data_errors ||
	    (zio->io_flags & ZIO_FLAG_RESILVER)) {
		int n = raidz_parity_verify(zio, rr);
		unexpected_errors += n;
		ASSERT3U(parity_errors + n, <=, rr->rr_firstdatacol);
	}

	if (zio->io_error == 0 && spa_writeable(zio->io_spa) &&
	    (unexpected_errors > 0 || (zio->io_flags & ZIO_FLAG_RESILVER))) {
		/*
		 * Use the good data we have in hand to repair damaged children.
		 */
		for (int c = 0; c < rr->rr_cols; c++) {
			raidz_col_t *rc = &rr->rr_col[c];
			vdev_t *vd = zio->io_vd;
			vdev_t *cvd = vd->vdev_child[rc->rc_devidx];

			if (rc->rc_error == 0 || rc->rc_size == 0)
				continue;

			zio_nowait(zio_vdev_child_io(zio, NULL, cvd,
			    rc->rc_offset, rc->rc_abd, rc->rc_size,
			    ZIO_TYPE_WRITE, ZIO_PRIORITY_ASYNC_WRITE,
			    ZIO_FLAG_IO_REPAIR | (unexpected_errors ?
			    ZIO_FLAG_SELF_HEAL : 0), NULL, NULL));
		}
	}
}

/*
 * Iterate over all combinations of bad data and attempt a reconstruction.
 * Note that the algorithm below is non-optimal because it doesn't take into
 * account how reconstruction is actually performed. For example, with
 * triple-parity RAID-Z the reconstruction procedure is the same if column 4
 * is targeted as invalid as if columns 1 and 4 are targeted since in both
 * cases we'd only use parity information in column 0.
 *
 * The order that we find the various possible combinations of failed
 * disks is dictated by these rules:
 * - Examine each "slot" (the "i" in tgts[i])
 *   - Try to increment this slot (tgts[i] = tgts[i] + 1)
 *   - if we can't increment because it runs into the next slot,
 *     reset our slot to the minimum, and examine the next slot
 *  For example, with a 6-wide RAIDZ3, and no known errors (so we have to choose
 *  3 columns to reconstruct), we will generate the following sequence:
 *
 *  STATE        ACTION
 *  0 1 2        special case: skip since these are all parity
 *  0 1   3      first slot: reset to 0; middle slot: increment to 2
 *  0   2 3      first slot: increment to 1
 *    1 2 3      first: reset to 0; middle: reset to 1; last: increment to 4
 *  0 1     4    first: reset to 0; middle: increment to 2
 *  0   2   4    first: increment to 1
 *    1 2   4    first: reset to 0; middle: increment to 3
 *  0     3 4    first: increment to 1
 *    1   3 4    first: increment to 2
 *      2 3 4    first: reset to 0; middle: reset to 1; last: increment to 5
 *  0 1       5  first: reset to 0; middle: increment to 2
 *  0   2     5  first: increment to 1
 *    1 2     5  first: reset to 0; middle: increment to 3
 *  0     3   5  first: increment to 1
 *    1   3   5  first: increment to 2
 *      2 3   5  first: reset to 0; middle: increment to 4
 *  0       4 5  first: increment to 1
 *    1     4 5  first: increment to 2
 *      2   4 5  first: increment to 3
 *        3 4 5  done
 */

/*
 * Should this sector be considered failed for logical child ID i?
 * XXX comment explaining logical child ID's
 */
static boolean_t
raidz_simulate_failure(vdev_raidz_t *vdrz, int ashift, int i, raidz_col_t *rc)
{
	uint64_t sector_id =
	    vdrz->vd_physical_width * (rc->rc_offset >> ashift) +
	    rc->rc_devidx;

#if 0
	zfs_dbgmsg("raidz_simulate_failure(pw=%u lw=%u ashift=%u i=%u rc_offset=%llx rc_devidx=%u sector_id=%u",
	    vdrz->vd_physical_width,
	    vdrz->vd_logical_width,
	    ashift,
	    i,
	    (long long)rc->rc_offset,
	    (int)rc->rc_devidx,
	    (long long)sector_id);
#endif

	for (int w = vdrz->vd_physical_width;
	    w >= vdrz->vd_logical_width; w--) {
		if (i < w) {
			return (sector_id % w == i);
		} else {
			i -= w;
		}
	}
	ASSERT(!"invalid logical child id");
	return (B_FALSE);
}

static void
raidz_restore_orig_data(raidz_map_t *rm)
{
	for (int i = 0; i < rm->rm_nrows; i++) {
		raidz_row_t *rr = rm->rm_row[i];
		for (int c = 0; c < rr->rr_cols; c++) {
			raidz_col_t *rc = &rr->rr_col[c];
			if (rc->rc_need_orig_restore) {
				abd_copy_from_buf(rc->rc_abd,
				    rc->rc_orig_data, rc->rc_size);
				rc->rc_need_orig_restore = B_FALSE;
			}
		}
	}
}

/*
 * returns EINVAL if reconstruction of the block will not be possible
 * returns ECKSUM if this specific reconstruction failed
 * returns 0 on successful reconstruction
 */
static int
raidz_reconstruct(zio_t *zio, int *ltgts, int ntgts)
{
	raidz_map_t *rm = zio->io_vsd;
	vdev_raidz_t *vdrz = zio->io_vd->vdev_tsd;

	zfs_dbgmsg("raidz_reconstruct_expanded(zio=%p ltgts=%u,%u,%u ntgts=%u",
	    zio, ltgts[0], ltgts[1], ltgts[2], ntgts);

	/* Reconstruct each row */
	for (int r = 0; r < rm->rm_nrows; r++) {
		raidz_row_t *rr = rm->rm_row[r];
		int my_tgts[VDEV_RAIDZ_MAXPARITY]; /* value is child id */
		int t = 0;
		int dead = 0;
		int dead_data = 0;

		zfs_dbgmsg("raidz_reconstruct_expanded(row=%u)",
		    r);

		for (int c = 0; c < rr->rr_cols; c++) {
			raidz_col_t *rc = &rr->rr_col[c];
			ASSERT0(rc->rc_need_orig_restore);
			if (rc->rc_error != 0) {
				dead++;
				if (c >= vdrz->vd_nparity)
					dead_data++;
				continue;
			}
			if (rc->rc_size == 0)
				continue;
			for (int lt = 0; lt < ntgts; lt++) {
				if (raidz_simulate_failure(vdrz,
				    zio->io_vd->vdev_top->vdev_ashift,
				    ltgts[lt], rc)) {
					if (rc->rc_orig_data == NULL) {
						rc->rc_orig_data =
						    zio_buf_alloc(rc->rc_size);
						abd_copy_to_buf(rc->rc_orig_data,
						    rc->rc_abd, rc->rc_size);
					}
					rc->rc_need_orig_restore = B_TRUE;

					dead++;
					if (c >= vdrz->vd_nparity)
						dead_data++;
					my_tgts[t++] = c;
					zfs_dbgmsg("simulating failure of col %u devidx %u",
					    c, (int)rc->rc_devidx);
					break;
				}
			}
		}
		if (dead > vdrz->vd_nparity) {
			/* reconstruction not possible */
			zfs_dbgmsg("reconstruction not possible; too many failures");
			raidz_restore_orig_data(rm);
			return (EINVAL);
		}
		rr->rr_code = 0;
		if (dead_data > 0)
			rr->rr_code = vdev_raidz_reconstruct_row(rr, my_tgts, t);
	}

	/* Check for success */
	if (raidz_checksum_verify(zio) == 0) {

		/* Reconstruction succeeded - report errors */
		for (int i = 0; i < rm->rm_nrows; i++) {
			raidz_row_t *rr = rm->rm_row[i];
			atomic_inc_64(&raidz_corrected[rr->rr_code]);

			for (int c = 0; c < rr->rr_cols; c++) {
				raidz_col_t *rc = &rr->rr_col[c];
				if (rc->rc_need_orig_restore) {
					/*
					 * Note: if this is a parity column,
					 * we don't really know if it's wrong.
					 * We need to let
					 * vdev_raidz_io_done_verified() check
					 * it, and if we set rc_error, it will
					 * think that it is a "known" error
					 * that doesn't need to be checked
					 * or corrected.
					 */
					if (rc->rc_error == 0 && c >= rr->rr_firstdatacol) {
						raidz_checksum_error(zio, rc, rc->rc_gdata);
						rc->rc_error = SET_ERROR(ECKSUM);
					}
					rc->rc_need_orig_restore = B_FALSE;
				}
			}

			vdev_raidz_io_done_verified(zio, rr);
		}

		zio_checksum_verified(zio);

		zfs_dbgmsg("reconstruction successful (checksum verified)");
		return (0);
	}

	/* Reconstruction failed - restore original data */
	raidz_restore_orig_data(rm);
	zfs_dbgmsg("raidz_reconstruct_expanded(zio=%p) checksum failed",
	    zio);
	return (ECKSUM);
}

/*
 * return 0 on success, ECKSUM on failure
 */
static int
vdev_raidz_combrec(zio_t *zio)
{
	raidz_map_t *rm = zio->io_vsd;
	vdev_raidz_t *vdrz = zio->io_vd->vdev_tsd;

	for (int num_failures = 1; num_failures <= vdrz->vd_nparity;
	    num_failures++) {
		int tstore[VDEV_RAIDZ_MAXPARITY + 2];
		int *ltgts = &tstore[1]; /* value is logical child ID */

		/* Determine number of logical children, n */
		int n = 0;
		for (int w = vdrz->vd_physical_width;
		    w >= vdrz->vd_logical_width; w--) {
			n += w;
		}

		ASSERT3U(num_failures, <=, vdrz->vd_nparity);
		ASSERT3U(num_failures, <=, VDEV_RAIDZ_MAXPARITY);
		/* handle corner cases in combrec logic */
		ltgts[-1] = -1;
		for (int i = 0; i < num_failures; i++) {
			ltgts[i] = i;
		}
		ltgts[num_failures] = n;

		for (;;) {
			int err = raidz_reconstruct(zio,
			    ltgts, num_failures);
			if (err == EINVAL) {
				/*
				 * Reconstruction not possible with this #
				 * failures; try more failures.
				 */
				break;
			} else if (err == 0)
				return (0);

			/* Compute next targets to try */
			for (int t = 0; ; t++) {
				ASSERT3U(t, <, num_failures);
				ltgts[t]++;
				if (ltgts[t] == n) {
					ASSERT3U(t, ==, num_failures - 1);
					zfs_dbgmsg("reconstruction failed for num_failures=%u; tried all combinations",
					    num_failures);
					break; // try more failures
				}

				ASSERT3U(ltgts[t], <, n);
				ASSERT3U(ltgts[t], <=, ltgts[t + 1]);

				/*
				 * If that spot is available, we're done here.
				 */
				if (ltgts[t] != ltgts[t + 1])
					break; // found next combination

				/*
				 * Otherwise, reset this tgt to the minimum,
				 * and move on to the next tgt.
				 */
				ltgts[t] = ltgts[t - 1] + 1;
				ASSERT3U(ltgts[t], ==, t);
			}
			if (ltgts[num_failures - 1] == n)
				break; // try more failures
		}
	}
	zfs_dbgmsg("reconstruction failed for all num_failures");
	return (ECKSUM);
}

/*
 * Complete a write IO operation on a RAIDZ VDev
 *
 * Outline:
 *   1. Check for errors on the child IOs.
 *   2. Return, setting an error code if too few child VDevs were written
 *      to reconstruct the data later.  Note that partial writes are
 *      considered successful if they can be reconstructed at all.
 */
static void
vdev_raidz_io_done_write_impl(zio_t *zio, raidz_row_t *rr)
{
	int total_errors = 0;

	ASSERT3U(rr->rr_missingparity, <=, rr->rr_firstdatacol);
	ASSERT3U(rr->rr_missingdata, <=, rr->rr_cols - rr->rr_firstdatacol);
	ASSERT3U(zio->io_type, ==, ZIO_TYPE_WRITE);

	for (int c = 0; c < rr->rr_cols; c++) {
		raidz_col_t *rc = &rr->rr_col[c];

		if (rc->rc_error) {
			ASSERT(rc->rc_error != ECKSUM);	/* child has no bp */

			total_errors++;
		}
	}

	/*
	 * XXX -- for now, treat partial writes as a success.
	 * (If we couldn't write enough columns to reconstruct
	 * the data, the I/O failed.  Otherwise, good enough.)
	 *
	 * Now that we support write reallocation, it would be better
	 * to treat partial failure as real failure unless there are
	 * no non-degraded top-level vdevs left, and not update DTLs
	 * if we intend to reallocate.
	 */
	/* XXPOLICY */
	if (total_errors > rr->rr_firstdatacol) {
		zio->io_error = zio_worst_error(zio->io_error,
		    vdev_raidz_worst_error(rr));
	}
}

/*
 * return 0 if no reconstruction occurred, otherwise the "code" from
 * vdev_raidz_reconstruct().
 */
static int
vdev_raidz_io_done_reconstruct_known_missing(zio_t *zio, raidz_row_t *rr)
{
	int parity_errors = 0;
	int parity_untried = 0;
	int data_errors = 0;
	int total_errors = 0;
	int code = 0;

	ASSERT3U(rr->rr_missingparity, <=, rr->rr_firstdatacol);
	ASSERT3U(rr->rr_missingdata, <=, rr->rr_cols - rr->rr_firstdatacol);
	ASSERT3U(zio->io_type, ==, ZIO_TYPE_READ);

	for (int c = 0; c < rr->rr_cols; c++) {
		raidz_col_t *rc = &rr->rr_col[c];

		if (rc->rc_error) {
			ASSERT(rc->rc_error != ECKSUM);	/* child has no bp */

			if (c < rr->rr_firstdatacol)
				parity_errors++;
			else
				data_errors++;

			total_errors++;
		} else if (c < rr->rr_firstdatacol && !rc->rc_tried) {
			parity_untried++;
		}
	}

	/*
	 * If there were data errors and the number of errors we saw was
	 * correctable -- less than or equal to the number of parity disks read
	 * -- reconstruct based on the missing data.
	 */
	if (data_errors != 0 &&
	    total_errors <= rr->rr_firstdatacol - parity_untried) {
		/*
		 * We either attempt to read all the parity columns or
		 * none of them. If we didn't try to read parity, we
		 * wouldn't be here in the correctable case. There must
		 * also have been fewer parity errors than parity
		 * columns or, again, we wouldn't be in this code path.
		 */
		ASSERT(parity_untried == 0);
		ASSERT(parity_errors < rr->rr_firstdatacol);

		/*
		 * Identify the data columns that reported an error.
		 */
		int n = 0;
		int tgts[VDEV_RAIDZ_MAXPARITY];
		for (int c = rr->rr_firstdatacol; c < rr->rr_cols; c++) {
			raidz_col_t *rc = &rr->rr_col[c];
			if (rc->rc_error != 0) {
				ASSERT(n < VDEV_RAIDZ_MAXPARITY);
				tgts[n++] = c;
			}
		}

		ASSERT(rr->rr_firstdatacol >= n);

		code = vdev_raidz_reconstruct_row(rr, tgts, n);
	}

	return (code);
}

/*
 * return the number of reads issued.
 */
static int
vdev_raidz_read_all(zio_t *zio, raidz_row_t *rr)
{
	vdev_t *vd = zio->io_vd;
	int nread = 0;

	rr->rr_missingdata = 0;
	rr->rr_missingparity = 0;

	for (int c = 0; c < rr->rr_cols; c++) {
		raidz_col_t *rc = &rr->rr_col[c];
		if (rc->rc_tried || rc->rc_size == 0)
			continue;

		zio_nowait(zio_vdev_child_io(zio, NULL,
		    vd->vdev_child[rc->rc_devidx],
		    rc->rc_offset, rc->rc_abd, rc->rc_size,
		    zio->io_type, zio->io_priority, 0,
		    vdev_raidz_child_done, rc));
		nread++;
	}
	return (nread);
}

static void
vdev_raidz_io_done(zio_t *zio)
{
	raidz_map_t *rm = zio->io_vsd;
	vdev_raidz_t *vdrz = zio->io_vd->vdev_tsd;

	ASSERT(zio->io_bp != NULL);  /* XXX need to add code to enforce this */
	if (zio->io_type == ZIO_TYPE_WRITE) {
		for (int i = 0; i < rm->rm_nrows; i++) {
			vdev_raidz_io_done_write_impl(zio, rm->rm_row[i]);
		}
	} else if (zio->io_type == ZIO_TYPE_FREE) {
		return;
	} else {
		for (int i = 0; i < rm->rm_nrows; i++) {
			raidz_row_t *rr = rm->rm_row[i];
			rr->rr_code =
			    vdev_raidz_io_done_reconstruct_known_missing(zio,
			    rr);
		}

		if (raidz_checksum_verify(zio) == 0) {
			for (int i = 0; i < rm->rm_nrows; i++) {
				raidz_row_t *rr = rm->rm_row[i];
				atomic_inc_64(&raidz_corrected[rr->rr_code]);
				vdev_raidz_io_done_verified(zio, rr);
			}
			zio_checksum_verified(zio);
		} else {
			/*
			 * This isn't a typical situation -- either we got a
			 * read error or a child silently returned bad data.
			 * Read every block so we can try again with as much
			 * data and parity as we can track down. If we've
			 * already been through once before, all children will
			 * be marked as tried so we'll proceed to combinatorial
			 * reconstruction.
			 */
			int nread = 0;
			for (int i = 0; i < rm->rm_nrows; i++) {
				nread += vdev_raidz_read_all(zio,
				    rm->rm_row[i]);
			}
			if (nread != 0) {
				/*
				 * Normally our stage is VDEV_IO_DONE, but if
				 * we've already called redone(), it will have
				 * changed to VDEV_IO_START, in which case we
				 * don't want to call redone() again.
				 */
				if (zio->io_stage != ZIO_STAGE_VDEV_IO_START)
					zio_vdev_io_redone(zio);
				return;
			}
			/*
			 * It would be too expensive to try every possible
			 * combination of failed sectors in every row, so
			 * instead we try every combination of failed current or
			 * past physical disk. This means that if the incorrect
			 * sectors were all on Nparity disks at any point in the
			 * past, we will find the correct data.  I think that
			 * the only case where this is less durable than
			 * a non-expanded RAIDZ, is if we have a silent
			 * failure during expansion.  In that case, one block
			 * could be partially in the old format and partially
			 * in the new format, so we'd lost some sectors
			 * from the old format and some from the new format.
			 *
			 * e.g. logical_width=4 physical_width=6
			 * the 15 (6+5+4) possible failed disks are:
			 * width=6 child=0
			 * width=6 child=1
			 * width=6 child=2
			 * width=6 child=3
			 * width=6 child=4
			 * width=6 child=5
			 * width=5 child=0
			 * width=5 child=1
			 * width=5 child=2
			 * width=5 child=3
			 * width=5 child=4
			 * width=4 child=0
			 * width=4 child=1
			 * width=4 child=2
			 * width=4 child=3
			 * And we will try every combination of Nparity of these
			 * failing.
			 *
			 * As a first pass, we can generate every combo,
			 * and try reconstructing, ignoring any known
			 * failures.  If any row has too many known + simulated
			 * failures, then we bail on reconstructing with this
			 * number of simulated failures.  As an improvement,
			 * we could detect the number of whole known failures
			 * (i.e. we have known failures on these disks for
			 * every row; the disks never succeeded), and
			 * subtract that from the max # failures to simulate.
			 * We could go even further like the current
			 * combrec code, but that doesn't seem like it
			 * gains us very much.  If we simulate a failure
			 * that is also a known failure, that's fine.
			 */
			if (vdev_raidz_combrec(zio) != 0) {
				/*
				 * We're here because either:
				 *
				 *	total_errors == rm_first_datacol, or
				 *	vdev_raidz_combrec() failed
				 *
				 * In either case, there is enough bad data to prevent
				 * reconstruction.
				 *
				 * Start checksum ereports for all children which haven't
				 * failed, and the IO wasn't speculative.
				 */
				zio->io_error = SET_ERROR(ECKSUM);

				if (!(zio->io_flags & ZIO_FLAG_SPECULATIVE)) {
					for (int i = 0; i < rm->rm_nrows; i++) {
						raidz_row_t *rr = rm->rm_row[i];
						for (int c = 0; c < rr->rr_cols; c++) {
							raidz_col_t *rc = &rr->rr_col[c];
							if (rc->rc_error == 0) {
								zio_bad_cksum_t zbc;
								zbc.zbc_has_cksum = 0;
								zbc.zbc_injected =
								    rm->rm_ecksuminjected;

								zfs_ereport_start_checksum(
								    zio->io_spa,
								    zio->io_vd->vdev_child[rc->rc_devidx],
								    zio, rc->rc_offset, rc->rc_size,
								    (void *)(uintptr_t)c, &zbc);
							}
						}
					}
				}
			}
		}
	}
	ASSERT(!vdrz->vn_expanding);
}

static void
vdev_raidz_state_change(vdev_t *vd, int faulted, int degraded)
{
	vdev_raidz_t *vdrz = vd->vdev_tsd;
	if (faulted > vdrz->vd_nparity)
		vdev_set_state(vd, B_FALSE, VDEV_STATE_CANT_OPEN,
		    VDEV_AUX_NO_REPLICAS);
	else if (degraded + faulted != 0)
		vdev_set_state(vd, B_FALSE, VDEV_STATE_DEGRADED, VDEV_AUX_NONE);
	else
		vdev_set_state(vd, B_FALSE, VDEV_STATE_HEALTHY, VDEV_AUX_NONE);
}

static void
raidz_copy_range(void *arg, uint64_t start, uint64_t size)
{
	vdev_t *vd = arg;
	int ashift = vd->vdev_top->vdev_ashift;
	int old_children = vd->vdev_children - 1;
	spa_t *spa = vd->vdev_spa;

	ASSERT(IS_P2ALIGNED(start, 1 << ashift));
	ASSERT(IS_P2ALIGNED(size, 1 << ashift));

	abd_t *abd = abd_alloc_for_io(1 << ashift, B_FALSE);
	for (uint64_t i = MAX(start >> ashift, old_children);
	    i < (start + size) >> ashift; i++) {
		int child = i % old_children;
		int offset = (i / old_children) << ashift;
		spa_config_enter(spa, SCL_STATE, spa, RW_READER);
		VERIFY0(zio_wait(zio_read_phys(NULL,
		    vd->vdev_child[child],
		    offset + VDEV_LABEL_START_SIZE,
		    1 << ashift, abd,
		    ZIO_CHECKSUM_OFF, NULL, NULL,
		    ZIO_PRIORITY_REMOVAL, 0, B_FALSE)));

		child = i % vd->vdev_children;
		offset = (i / vd->vdev_children) << ashift;
		VERIFY0(zio_wait(zio_write_phys(NULL,
		    vd->vdev_child[child],
		    offset + VDEV_LABEL_START_SIZE,
		    1 << ashift, abd,
		    ZIO_CHECKSUM_OFF, NULL, NULL,
		    ZIO_PRIORITY_REMOVAL, 0, B_FALSE)));
		spa_config_exit(spa, SCL_STATE, spa);
	}
	abd_free(abd);
}

void
vdev_raidz_attach_sync(void *arg, dmu_tx_t *tx)
{
	vdev_t *new_child = arg;
	spa_t *spa = new_child->vdev_spa;
	vdev_t *raidvd = new_child->vdev_parent;
	vdev_raidz_t *vdrz = raidvd->vdev_tsd;
	ASSERT3P(raidvd->vdev_ops, ==, &vdev_raidz_ops);
	ASSERT3P(raidvd->vdev_top, ==, raidvd);
	ASSERT3U(raidvd->vdev_children, >, vdrz->vd_logical_width);
	ASSERT3U(raidvd->vdev_children, ==, vdrz->vd_physical_width + 1);

	/*
	 * XXX assuming that no other i/o takes place while this is happening,
	 * until we increment physical_width. But ZIL could do i/o.
	 */
	vdrz->vn_expanding = B_TRUE;

	/*spa_config_enter(spa, SCL_CONFIG, FTAG, RW_READER);*/

	range_tree_t *rt = range_tree_create(NULL, NULL);

	for (uint64_t i = 0; i < raidvd->vdev_ms_count; i++) {
		metaslab_t *msp = raidvd->vdev_ms[i];

		/*vdev_initialize_ms_mark(msp);*/
		mutex_enter(&msp->ms_lock);

		metaslab_load_wait(msp);
		if (!msp->ms_loaded)
			VERIFY0(metaslab_load(msp));

		/*
		 * We want to copy everything except the free (allocatable)
		 * space.  Note that there may be a little bit more free
		 * space (e.g. in ms_defer), and it's fine to copy that too.
		 */
		ASSERT(range_tree_is_empty(rt));
		range_tree_add(rt, msp->ms_start, msp->ms_size);
		range_tree_walk(msp->ms_allocatable, range_tree_remove, rt);
		mutex_exit(&msp->ms_lock);

		/*spa_config_exit(spa, SCL_CONFIG, FTAG);*/
		/* Note, _vacate() doesn't visit in order */
		range_tree_walk(rt, raidz_copy_range, raidvd);
		range_tree_vacate(rt, NULL, NULL);
		/*vdev_initialize_ms_unmark(msp);*/
		/*spa_config_enter(spa, SCL_CONFIG, FTAG, RW_READER);*/
	}

	/*spa_config_exit(spa, SCL_CONFIG, FTAG);*/
	range_tree_destroy(rt);

	vdrz->vd_physical_width++;

#if 0
	raidvd->vdev_expanding = B_TRUE;
	vdev_reopen(raidvd);
	raidvd->vdev_expanding = B_FALSE;
#endif

	vdrz->vn_expanding = B_FALSE;
	/* Ensure that widths get written to label config */
	vdev_config_dirty(raidvd);
}

/*
 * Add RAIDZ-specific fields to the config nvlist.
 * XXX add this to vdev_ops_t?
 */
void
vdev_raidz_config_generate(vdev_t *vd, nvlist_t *nv)
{
	spa_t *spa = vd->vdev_spa;
	ASSERT3P(vd->vdev_ops, ==, &vdev_raidz_ops);
	vdev_raidz_t *vdrz = vd->vdev_tsd;

	/*
	 * Make sure someone hasn't managed to sneak a fancy new vdev
	 * into a crufty old storage pool.
	 */
	ASSERT(vdrz->vd_nparity == 1 ||
	    (vdrz->vd_nparity <= 2 &&
	    spa_version(spa) >= SPA_VERSION_RAIDZ2) ||
	    (vdrz->vd_nparity <= 3 &&
	    spa_version(spa) >= SPA_VERSION_RAIDZ3));

	/*
	 * Note that we'll add these even on storage pools where they
	 * aren't strictly required -- older software will just ignore
	 * it.
	 */
	fnvlist_add_uint64(nv, ZPOOL_CONFIG_NPARITY, vdrz->vd_nparity);
	fnvlist_add_uint64(nv, ZPOOL_CONFIG_RAIDZ_LOGICAL_WIDTH,
	    vdrz->vd_logical_width);
}

/*
 * Set RAIDZ-specific fields in the vdev_t, based on the config.
 * Can't assume that anything about the vdev_t is already set.
 * XXX add this to vdev_ops_t?
 */
void *
vdev_raidz_get_tsd(spa_t *spa, nvlist_t *nv)
{
	uint64_t nparity, lw;
	vdev_raidz_t *vdrz = kmem_zalloc(sizeof (*vdrz), KM_SLEEP);

	uint_t children;
	nvlist_t **child;
	int error = nvlist_lookup_nvlist_array(nv,
	    ZPOOL_CONFIG_CHILDREN, &child, &children);
	if (error != 0)
		goto out;

	vdrz->vd_logical_width = children;
	vdrz->vd_physical_width = children;

	if (nvlist_lookup_uint64(nv, ZPOOL_CONFIG_RAIDZ_LOGICAL_WIDTH,
	    &lw) == 0) {
		vdrz->vd_logical_width = lw;
	}

	if (nvlist_lookup_uint64(nv, ZPOOL_CONFIG_NPARITY,
	    &nparity) == 0) {
		if (nparity == 0 || nparity > VDEV_RAIDZ_MAXPARITY)
			goto out;
		/*
		 * Previous versions could only support 1 or 2 parity
		 * device.
		 */
		if (nparity > 1 &&
		    spa_version(spa) < SPA_VERSION_RAIDZ2)
			goto out;
		if (nparity > 2 &&
		    spa_version(spa) < SPA_VERSION_RAIDZ3)
			goto out;
	} else {
		/*
		 * We require the parity to be specified for SPAs that
		 * support multiple parity levels.
		 */
		if (spa_version(spa) >= SPA_VERSION_RAIDZ2)
			goto out;
		/*
		 * Otherwise, we default to 1 parity device for RAID-Z.
		 */
		nparity = 1;
	}
	vdrz->vd_nparity = nparity;
	return (vdrz);
out:
	kmem_free(vdrz, sizeof (*vdrz));
	return (NULL);
}

vdev_ops_t vdev_raidz_ops = {
	vdev_raidz_open,
	vdev_raidz_close,
	vdev_raidz_asize,
	vdev_raidz_io_start,
	vdev_raidz_io_done,
	vdev_raidz_state_change,
	NULL,
	NULL,
	NULL,
	VDEV_TYPE_RAIDZ,	/* name of this vdev type */
	B_FALSE			/* not a leaf vdev */
};
