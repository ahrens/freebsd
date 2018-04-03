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
 * Copyright (c) 2013, Joyent, Inc. All rights reserved.
 */

#ifndef _SYS_VDEV_RAIDZ_H
#define	_SYS_VDEV_RAIDZ_H

#include <sys/vdev.h>
#ifdef illumos
#include <sys/semaphore.h>
#ifdef _KERNEL
#include <sys/ddi.h>
#include <sys/sunldi.h>
#include <sys/sunddi.h>
#endif
#endif

#ifdef	__cplusplus
extern "C" {
#endif

typedef struct vdev_raidz {
	int vd_logical_width;
	int vd_physical_width;
	int vd_nparity;
	boolean_t vn_expanding;
} vdev_raidz_t;

extern int vdev_raidz_physio(vdev_t *,
    caddr_t, size_t, uint64_t, uint64_t, boolean_t, boolean_t);
extern void vdev_raidz_attach_sync(void *, dmu_tx_t *);
extern void vdev_raidz_config_generate(vdev_t *, nvlist_t *);
extern void *vdev_raidz_get_tsd(spa_t *, nvlist_t *);
#ifdef	__cplusplus
}
#endif

#endif	/* _SYS_VDEV_RAIDZ_H */
