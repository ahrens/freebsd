/*-
 * Copyright (c) 1997,1998 Doug Rabson
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $FreeBSD$
 */

#ifndef _SYS_BUS_H_
#define _SYS_BUS_H_

/*
 * Interface information structure.
 */
struct u_businfo {
	int	ub_version;		/* interface version */
#define BUS_USER_VERSION	1
	int	ub_generation;		/* generation count */
};

/*
 * Device information exported to userspace.
 */
struct u_device {
	uintptr_t	dv_handle;
	uintptr_t	dv_parent;

	char	dv_name[32];
	char	dv_desc[32];
	char	dv_drivername[32];
	/* XXX more driver info? */
};

#ifdef _KERNEL

#include <sys/queue.h>
#include <sys/kobj.h>

/*
 * Forward declarations
 */
typedef struct device		*device_t;
typedef struct driver		driver_t;
typedef struct devclass		*devclass_t;
#define device_method_t		kobj_method_t

typedef void driver_intr_t(void*);

/*
 * Interrupt type bits.  These flags are used both by newbus interrupt
 * registration (nexus.c) and also in struct intrec, which defines
 * interrupt properties.
 *
 * XXX We should probably revisit this and remove the vestiges of the
 * spls implicit in names like INTR_TYPE_TTY.  In the meantime, don't
 * confuse things by renaming them (Grog, 18 July 2000).
 *
 * We define this in terms of bits because some devices may belong
 * to multiple classes (and therefore need to be included in
 * multiple interrupt masks, which is what this really serves to
 * indicate.  Buses which do interrupt remapping will want to
 * change their type to reflect what sort of devices are underneath.
 */
enum intr_type {
	INTR_TYPE_TTY = 1,
	INTR_TYPE_BIO = 2,
	INTR_TYPE_NET = 4,
	INTR_TYPE_CAM = 8,
	INTR_TYPE_MISC = 16,
	INTR_TYPE_CLK = 32,
	INTR_TYPE_AV = 64,
	INTR_FAST = 128,
	INTR_EXCL = 256,		/* exclusive interrupt */
	INTR_MPSAFE = 512,		/* this interrupt is SMP safe */
	INTR_ENTROPY = 1024		/* this interrupt provides entropy */
};

typedef int (*devop_t)(void);

struct driver {
	KOBJ_CLASS_FIELDS;
	void	*priv;			/* driver private data */
};

typedef enum device_state {
	DS_NOTPRESENT,			/* not probed or probe failed */
	DS_ALIVE,			/* probe succeeded */
	DS_ATTACHED,			/* attach method called */
	DS_BUSY				/* device is open */
} device_state_t;

/*
 * Definitions for drivers which need to keep simple lists of resources
 * for their child devices.
 */
struct	resource;

struct resource_list_entry {
	SLIST_ENTRY(resource_list_entry) link;
	int	type;			/* type argument to alloc_resource */
	int	rid;			/* resource identifier */
	struct	resource *res;		/* the real resource when allocated */
	u_long	start;			/* start of resource range */
	u_long	end;			/* end of resource range */
	u_long	count;			/* count within range */
};
SLIST_HEAD(resource_list, resource_list_entry);

/*
 * Initialise a resource list.
 */
void	resource_list_init(struct resource_list *rl);

/*
 * Reclaim memory used by a resource list.
 */
void	resource_list_free(struct resource_list *rl);

/*
 * Add a resource entry or modify an existing entry if one exists with 
 * the same type and rid.
 */
void	resource_list_add(struct resource_list *rl,
			  int type, int rid,
			  u_long start, u_long end, u_long count);

/*
 * Find a resource entry by type and rid.
 */
struct resource_list_entry*
	resource_list_find(struct resource_list *rl,
			   int type, int rid);

/*
 * Delete a resource entry.
 */
void	resource_list_delete(struct resource_list *rl,
			     int type, int rid);

/*
 * Implement BUS_ALLOC_RESOURCE by looking up a resource from the list 
 * and passing the allocation up to the parent of bus. This assumes
 * that the first entry of device_get_ivars(child) is a struct
 * resource_list. This also handles 'passthrough' allocations where a
 * child is a remote descendant of bus by passing the allocation up to 
 * the parent of bus.
 */
struct resource *
	resource_list_alloc(struct resource_list *rl,
			    device_t bus, device_t child,
			    int type, int *rid,
			    u_long start, u_long end,
			    u_long count, u_int flags);

/*
 * Implement BUS_RELEASE_RESOURCE.
 */
int	resource_list_release(struct resource_list *rl,
			      device_t bus, device_t child,
			      int type, int rid, struct resource *res);

/*
 * Print all resources of a specified type, for use in bus_print_child.
 * The name is printed if at least one resource of the given type is available.
 * The format ist used to print resource start and end.
 */
int	resource_list_print_type(struct resource_list *rl,
				 const char *name, int type,
				 const char *format);
/*
 * The root bus, to which all top-level busses are attached.
 */
extern device_t root_bus;
extern devclass_t root_devclass;
void	root_bus_configure(void);

/*
 * Useful functions for implementing busses.
 */

int	bus_generic_activate_resource(device_t dev, device_t child, int type,
				      int rid, struct resource *r);
struct resource *
	bus_generic_alloc_resource(device_t bus, device_t child, int type,
				   int *rid, u_long start, u_long end,
				   u_long count, u_int flags);
int	bus_generic_attach(device_t dev);
int	bus_generic_deactivate_resource(device_t dev, device_t child, int type,
					int rid, struct resource *r);
int	bus_generic_detach(device_t dev);
void	bus_generic_driver_added(device_t dev, driver_t *driver);
struct resource_list *
	bus_generic_get_resource_list (device_t, device_t);
int	bus_print_child_header(device_t dev, device_t child);
int	bus_print_child_footer(device_t dev, device_t child);
int	bus_generic_print_child(device_t dev, device_t child);
int	bus_generic_probe(device_t dev);
int	bus_generic_read_ivar(device_t dev, device_t child, int which,
			      uintptr_t *result);
int	bus_generic_release_resource(device_t bus, device_t child,
				     int type, int rid, struct resource *r);
int	bus_generic_resume(device_t dev);
int	bus_generic_setup_intr(device_t dev, device_t child,
			       struct resource *irq, int flags,
			       driver_intr_t *intr, void *arg, void **cookiep);

struct resource *
	bus_generic_rl_alloc_resource (device_t, device_t, int, int *,
				       u_long, u_long, u_long, u_int);
void	bus_generic_rl_delete_resource (device_t, device_t, int, int);
int	bus_generic_rl_get_resource (device_t, device_t, int, int, u_long *,
				     u_long *);
int	bus_generic_rl_set_resource (device_t, device_t, int, int, u_long,
				     u_long);
int	bus_generic_rl_release_resource (device_t, device_t, int, int,
					 struct resource *);

int	bus_generic_shutdown(device_t dev);
int	bus_generic_suspend(device_t dev);
int	bus_generic_teardown_intr(device_t dev, device_t child,
				  struct resource *irq, void *cookie);
int	bus_generic_write_ivar(device_t dev, device_t child, int which,
			       uintptr_t value);

/*
 * Wrapper functions for the BUS_*_RESOURCE methods to make client code
 * a little simpler.
 */
struct	resource *bus_alloc_resource(device_t dev, int type, int *rid,
				     u_long start, u_long end, u_long count,
				     u_int flags);
int	bus_activate_resource(device_t dev, int type, int rid, 
			      struct resource *r);
int	bus_deactivate_resource(device_t dev, int type, int rid,
				struct resource *r);
int	bus_release_resource(device_t dev, int type, int rid, 
			     struct resource *r);
int	bus_setup_intr(device_t dev, struct resource *r, int flags,
		       driver_intr_t handler, void *arg, void **cookiep);
int	bus_teardown_intr(device_t dev, struct resource *r, void *cookie);
int	bus_set_resource(device_t dev, int type, int rid,
			 u_long start, u_long count);
int	bus_get_resource(device_t dev, int type, int rid,
			 u_long *startp, u_long *countp);
u_long	bus_get_resource_start(device_t dev, int type, int rid);
u_long	bus_get_resource_count(device_t dev, int type, int rid);
void	bus_delete_resource(device_t dev, int type, int rid);

/*
 * Access functions for device.
 */
device_t	device_add_child(device_t dev, const char *name, int unit);
device_t	device_add_child_ordered(device_t dev, int order,
					 const char *name, int unit);
void	device_busy(device_t dev);
int	device_delete_child(device_t dev, device_t child);
int	device_detach(device_t dev);
void	device_disable(device_t dev);
void	device_enable(device_t dev);
device_t	device_find_child(device_t dev, const char *classname,
				  int unit);
const char 	*device_get_desc(device_t dev);
devclass_t	device_get_devclass(device_t dev);
driver_t	*device_get_driver(device_t dev);
u_int32_t	device_get_flags(device_t dev);
device_t	device_get_parent(device_t dev);
int	device_get_children(device_t dev, device_t **listp, int *countp);
void	*device_get_ivars(device_t dev);
void	device_set_ivars(device_t dev, void *ivars);
const	char *device_get_name(device_t dev);
const	char *device_get_nameunit(device_t dev);
void	*device_get_softc(device_t dev);
device_state_t	device_get_state(device_t dev);
int	device_get_unit(device_t dev);
int	device_is_alive(device_t dev);	/* did probe succeed? */
int	device_is_enabled(device_t dev);
int	device_is_quiet(device_t dev);
int	device_print_prettyname(device_t dev);
int	device_printf(device_t dev, const char *, ...) __printflike(2, 3);
int	device_probe_and_attach(device_t dev);
void	device_quiet(device_t dev);
void	device_set_desc(device_t dev, const char* desc);
void	device_set_desc_copy(device_t dev, const char* desc);
int	device_set_devclass(device_t dev, const char *classname);
int	device_set_driver(device_t dev, driver_t *driver);
void	device_set_flags(device_t dev, u_int32_t flags);
void	device_set_softc(device_t dev, void *softc);
int	device_set_unit(device_t dev, int unit);	/* XXX DONT USE XXX */
int	device_shutdown(device_t dev);
void	device_unbusy(device_t dev);
void	device_verbose(device_t dev);

/*
 * Access functions for devclass.
 */
int	devclass_add_driver(devclass_t dc, driver_t *driver);
int	devclass_delete_driver(devclass_t dc, driver_t *driver);
devclass_t	devclass_create(const char *classname);
devclass_t	devclass_find(const char *classname);
driver_t	*devclass_find_driver(devclass_t dc, const char *classname);
const char 	*devclass_get_name(devclass_t dc);
device_t	devclass_get_device(devclass_t dc, int unit);
void	*devclass_get_softc(devclass_t dc, int unit);
int	devclass_get_devices(devclass_t dc, device_t **listp, int *countp);
int	devclass_get_maxunit(devclass_t dc);
int	devclass_find_free_unit(devclass_t dc, int unit);

/*
 * Access functions for device resources.
 */

int	resource_int_value(const char *name, int unit, const char *resname,
			   int *result);
int	resource_long_value(const char *name, int unit, const char *resname,
			    long *result);
int	resource_string_value(const char *name, int unit, const char *resname,
			      const char **result);
int	resource_find_match(int *anchor, const char **name, int *unit,
			    const char *resname, const char *value);
int	resource_find_dev(int *anchor, const char *name, int *unit,
			  const char *resname, const char *value);
int	resource_set_int(const char *name, int unit, const char *resname,
			 int value);
int	resource_set_long(const char *name, int unit, const char *resname,
			  long value);
int	resource_set_string(const char *name, int unit, const char *resname,
			    const char *value);
/*
 * Functions for maintaining and checking consistency of
 * bus information exported to userspace.
 */
int	bus_data_generation_check(int generation);
void	bus_data_generation_update(void);

/*
 * Shorthand for constructing method tables.
 */
#define	DEVMETHOD	KOBJMETHOD

/*
 * Some common device interfaces.
 */
#include "device_if.h"
#include "bus_if.h"

struct	module;

int	driver_module_handler(struct module *, int, void *);

/*
 * Module support for automatically adding drivers to busses.
 */
struct driver_module_data {
	int		(*dmd_chainevh)(struct module *, int, void *);
	void		*dmd_chainarg;
	const char	*dmd_busname;
	driver_t	**dmd_drivers;
	int		dmd_ndrivers;
	devclass_t	*dmd_devclass;
};

#define	DRIVER_MODULE(name, busname, driver, devclass, evh, arg)	\
									\
static driver_t *name##_##busname##_driver_list[] = { &driver };	\
static struct driver_module_data name##_##busname##_driver_mod = {	\
	evh, arg,							\
	#busname,							\
	name##_##busname##_driver_list,					\
	(sizeof name##_##busname##_driver_list) /			\
		(sizeof name##_##busname##_driver_list[0]),		\
	&devclass							\
};									\
									\
static moduledata_t name##_##busname##_mod = {				\
	#busname "/" #name,						\
	driver_module_handler,						\
	&name##_##busname##_driver_mod					\
};									\
DECLARE_MODULE(name##_##busname, name##_##busname##_mod,		\
	       SI_SUB_DRIVERS, SI_ORDER_MIDDLE)

#define	MULTI_DRIVER_MODULE(name, busname, drivers, devclass, evh, arg) \
									\
static driver_t name##_##busname##_driver_list[] = drivers;		\
static struct driver_module_data name##_##busname##_driver_mod = {	\
	evh, arg,							\
	#busname,							\
	name##_##busname##_driver_list,					\
	(sizeof name##_##busname##_driver_list) /			\
		(sizeof name##_##busname##_driver_list[0]),		\
	&devclass							\
};									\
									\
static moduledata_t name##_##busname##_mod = {				\
	#busname "/" #name,						\
	driver_module_handler,						\
	&name##_##busname##_driver_mod					\
};									\
DECLARE_MODULE(name##_##busname, name##_##busname##_mod,		\
	       SI_SUB_DRIVERS, SI_ORDER_MIDDLE)

/*
 * Generic ivar accessor generation macros for bus drivers
 */
#define __BUS_ACCESSOR(varp, var, ivarp, ivar, type)			\
									\
static __inline type varp ## _get_ ## var(device_t dev)			\
{									\
	uintptr_t v;							\
	BUS_READ_IVAR(device_get_parent(dev), dev,			\
	    ivarp ## _IVAR_ ## ivar, &v);				\
	return ((type) v);						\
}									\
									\
static __inline void varp ## _set_ ## var(device_t dev, type t)		\
{									\
	uintptr_t v = (uintptr_t) t;					\
	BUS_WRITE_IVAR(device_get_parent(dev), dev,			\
	    ivarp ## _IVAR_ ## ivar, v);				\
}

#endif /* _KERNEL */

#endif /* !_SYS_BUS_H_ */
