/* $Id: hdblib.h,v 1.1 1996/04/15 10:52:09 djb1 Exp $ */

/*
 *	HDB prototypes and definitions
 *	Copyright (C) 1991 Inmos Limited
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#ifndef HDBLIB
#define HDBLIB

#include <stdio.h>

#ifndef _IMSTYPEH
#include "imstype.h"
#endif
#ifndef MEMLIB
#include "memlib.h"
#endif

/* Version has 4 1 byte fields, minor revision, ndl reader revision */
/*                              Major revision, Release Revision    */

#define HDB_LIBRARY_VERSION        0x01080002

/* ------------------------------------------------------------------------- */
/* --- Configuration constants --- */


#define HDB_MAX_BRANCHES                7    /* Max branches in boot tree */
#define HDB_EDGE_LINKS                  1    /* Device characteristics */
#define HDB_COMPUTE_LINKS               4
#define HDB_T9000_LINKS                 4
#define HDB_C104_LINKS                 32
#define HDB_C104_INTERVALS             36
#define HDB_C100_LINKS                  4

#define HDB_MAX_COMPUTE_LINKS              HDB_T9000_LINKS
#define HDB_MAX_ROUTER_LINKS               HDB_C104_LINKS
#define HDB_MAX_ROUTER_INTERVALS           HDB_C104_INTERVALS
#define HDB_MAX_PROTOCOL_CONVERTER_LINKS   HDB_C100_LINKS

#define HDB_MEMORY_ROM                  1    /* Memory usage flag in NDL */
#define HDB_MEMORY_SYSTEM               2    /* Memory usage flag in NDL */
#define HDB_MEMORY_RESERVED             4    /* Memory usage flag in NDL */
#define HDB_LEGAL_MEMORY_FLAGS_MASK	(HDB_MEMORY_ROM | HDB_MEMORY_SYSTEM | HDB_MEMORY_RESERVED)

#define HDB_UNKNOWN_WAITERR_CODE      256    /* Constant in NDL, WAITERR */ 
#define HDB_UNKNOWN_EOM_CODE          257    /* Constant in NDL, EOM     */

#define HDB_UNKNOWN_MESSAGE_TAG  (0 << 5)    /* Tag in UNKNOWN initialisation */
#define HDB_UNKNOWN_WAITERR_TAG  (1 << 5)    /* Tag in UNKNOWN initialisation */

#define HDB_INFO_NORMAL                 0    /* Levels of normal verbosity */
#define HDB_INFO_INFORMATION            1
#define HDB_INFO_VERBOSE                2
#define HDB_INFO_DEBUG                  3

#define HDB_NO_DEBUG                    HDB_INFO_NORMAL
#define HDB_VERBOSE                     HDB_INFO_INFORMATION
#define HDB_VERY_VERBOSE                HDB_INFO_VERBOSE

#define HDB_VCP_ENABLE_16_BIT_HEADERS   4    /* Mask to apply to vcp mode reg */

#define NDB_FIX_C104_COMPARATOR_BUG	1    /* Mask used in NDB files */
#define NDB_FIX_C104_SPURIOUS_ERROR_BUG 2

/* ------------------------------------------------------------------------- */
/* --- General Types --- */

typedef struct hdb_hardware_s      *hdb_hardware_t;
typedef struct hdb_handle_s        *hdb_handle_t;
typedef struct hdb_device_s        *hdb_device_t;
typedef struct hdb_partition_s     *hdb_partition_t;
typedef struct hdb_conn_s          *hdb_conn_t;
typedef struct hdb_route_s         *hdb_route_t;
typedef struct hdb_edge_s          *hdb_edge_t;
typedef struct hdb_path_s          *hdb_path_t;
typedef struct hdb_name_s          *hdb_name_t;
typedef struct hdb_basic_name_s    *hdb_basic_name_t;

typedef enum { hdb_err, hdb_ok                             } hdb_status_t;
typedef enum { hdb_sev_warning,hdb_sev_error,hdb_sev_fatal } hdb_severity_t;
typedef enum { hdb_continue, hdb_terminate                 } hdb_continue_t;

typedef hdb_continue_t (*hdb_handler_fn_t)( hdb_hardware_t handle,
                                            hdb_severity_t sev,
                                            const char *message,
                                            const char *filename,
                                            int line_number,
                                            void *user_parameter );

/* ------------------------------------------------------------------------- */
/* --- Data structures for returned data from HDB --- */

typedef enum { hdb_tseries, hdb_hseries } hdb_style_t;
typedef struct
  {
    char         *net_name;                   /* network name string */
    hdb_style_t   net_type;                   /* Old / new style, etc */
    BOOL          net_boot_from_rom;
    int           net_procs;                  /* number of procs */
    int           net_routers;                /* number of routers */
    int           net_unknowns;               /* number of unknown devices */
    int           net_protocol_converters;    /* number of protocol converters */
    int           net_edges;                  /* number of edges */
  } hdb_netdata_t;

/* --- */

typedef enum
  {
    hdb_devices,
    hdb_devices_proc,
    hdb_devices_router,
    hdb_devices_unknown,
    hdb_devices_protocol_converter,
    hdb_partitions,
    hdb_conns,
    hdb_routes,
    hdb_edges,
    hdb_paths,
    hdb_control_tree
  } hdb_applies_t;

typedef hdb_status_t (*hdb_apply_fn_t)( hdb_handle_t   handle, 
                                        void          *user_parameter );
typedef hdb_status_t (*hdb_apply_name_fn_t)( const char        *text,
                                             int                num_dimensions,
                                             const int         *dimensions,
                                             hdb_basic_name_t   base_name,
                                             void              *user_parameter);

/* --- */

typedef enum
  {
    hdb_compute,
    hdb_router,
    hdb_unknown,
    hdb_protocol_converter
  } hdb_dtypes_t;

typedef enum
  {
    hdb_UNKNOWN       = -9,
    hdb_T212          = -1,
    hdb_M212          = -2,
    hdb_T414          = -3,
    hdb_T800          = -4,
    hdb_T222          = -5,
    hdb_TA            = -6,
    hdb_TB            = -7,
    hdb_TC            = -8,
    hdb_T425          = 0,
    hdb_T805          = 10,
    hdb_T801          = 20,
    hdb_T426          = 30,
    hdb_T225          = 40,
    hdb_T400          = 50,
    hdb_T806          = 60,
    hdb_T9000         = 300,		/* This may go to 304 someday soon */
    hdb_T9000_gamma   = 302,		/* T9000 Gamma rev D */
    hdb_T9000_gamma_e = 303,		/* T9000 Gamma rev E (not lddevid) */
    hdb_C104          = 336,
    hdb_C100          = 320,
    hdb_T450	      = 1024,		/* #400 = min RMC lddevid */
    hdb_ST20	      = 2047		/* #7FF = max RMC lddevid */
  } hdb_dident_t;

typedef struct hdb_memdef_s
  {
    INT32         base;
    BIT32         length;
    BIT32         flags;
  } hdb_memdef_t;

typedef struct hdb_treenode_s hdb_treenode_t;

typedef struct
  {
    hdb_name_t            dev_name;
    hdb_dtypes_t          dev_type;
    hdb_dident_t          dev_ident;
    int                   dev_mode;
    INT32		  dev_memstart;
    int			  dev_num_links;
    int                   dev_num_memblocks;
    hdb_memdef_t         *dev_memblocks;
    BOOL                  dev_has_local_rom;
    BOOL                  dev_has_system_rom;
    BOOL                  dev_reboots_from_link;
    BOOL                  dev_boots_from_rom;
    hdb_treenode_t       *dev_bootpath_treenode;
    void                 *dev_user_data;
  } hdb_devicedata_t;

/* --- */

typedef INT16 hdb_header_t;
typedef struct
  {
    hdb_header_t send_id;
    hdb_header_t return_id;
  } hdb_devlab_t;

/* --- */

typedef INT32 hdb_reg_t;

/* --- */

typedef struct
  {
    hdb_reg_t    address;
    hdb_reg_t    mask;
    hdb_reg_t    format_control;
    hdb_reg_t    ras_bits;
    hdb_reg_t    ras_strobe;
    hdb_reg_t    cas_strobe;
    hdb_reg_t    prog_strobe;
    hdb_reg_t    write_strobe;
    hdb_reg_t    timing_control;
  } hdb_bank_regs_t;

typedef struct
  {
    hdb_reg_t         do_pmi_configured;
    hdb_reg_t         remap_boot_bank;
    hdb_reg_t         refresh_control;
    hdb_bank_regs_t   bank[4];
  } hdb_pmi_regs_t;

/* --- */

typedef struct
  {
    hdb_reg_t         do_ram_size;
    hdb_reg_t         ram_size;
  } hdb_cache_regs_t;

/* --- */

typedef struct
  {
    hdb_reg_t         command_resetlink;
    hdb_reg_t         command_startlink;
    hdb_reg_t         command_resetoutput;
    hdb_reg_t         command_wrongparity;
    hdb_reg_t         data_mode[HDB_MAX_COMPUTE_LINKS];
    BOOL              data_connected[HDB_MAX_COMPUTE_LINKS];
    hdb_reg_t         ctrl_mode[2];
    BOOL              ctrl_down_connected;
  } hdb_link_regs_t;

/* --- */

typedef struct
  {
    hdb_reg_t         dslinkpll;
  } hdb_sys_regs_t;

/* --- */

typedef struct
  {
    hdb_reg_t         hdrareabase;
    hdb_reg_t         memstartval;
    hdb_reg_t         mininvalidvchan;
    hdb_reg_t         externalrcbase;
  } hdb_cpu_regs_t;

/* --- */

typedef struct
  {
    hdb_reg_t         mode;
    hdb_reg_t         maxheader;
    hdb_reg_t         minheader;
    hdb_reg_t         offset;
  } hdb_vcplink_t;

typedef struct
  {
    hdb_reg_t         vcpcommand_start;
    hdb_reg_t         vcpcommand_stop;
    hdb_reg_t         vcpcommand_reset;

    hdb_reg_t         vcpmode;       /* Event mode in     */	/* This used to ifdeffed to appear only */
                                     /* products overview */    /* for beta's and alphas causing grief  */
							    	/* to people using this include file    */

    hdb_vcplink_t     vcplink[HDB_MAX_COMPUTE_LINKS];
  } hdb_vcp_regs_t;

/* --- */

typedef struct
  {
    int                link_number;
    int                header_length;
    unsigned char      header[4];
    INT32              bufferpointer;
  } hdb_vlcb_t;

/* --- */

typedef struct
  {
    BOOL               pmi_regs_inrom;
    BOOL               pmi_remap_bootbank;
    hdb_pmi_regs_t    *pmi_regs;

    BOOL               cache_regs_inrom;
    hdb_cache_regs_t  *cache_regs;

    BOOL               link_regs_inrom;
    hdb_link_regs_t   *link_regs;
    hdb_sys_regs_t    *sys_regs;

    BOOL               boot_vcp_regs_inrom;
    hdb_cpu_regs_t    *cpu_regs;
    hdb_vcp_regs_t    *boot_vcp_regs;
    int                num_ctrl_channels;
    int                num_boot_channels;
    hdb_vlcb_t        *boot_vclbs;
    int                bootstrap_length;
    unsigned char     *bootstrap;

    BOOL               preamble_inrom;
    int                preamble_length;   
    unsigned char     *preamble;
  } hdb_devconf_t;

/* --- */

typedef struct
  {
      BOOL           start_code_set;
      int            start_code;

      BOOL           identify_code_set;
      int            identify_code;

      BOOL           identify_response_set;
      int            identify_response;

      BOOL           expected_identity_set;
      int            expected_identity;

      BOOL           error_response_set;
      int            error_response;

      BOOL           error_handshake_set;
      int            error_handshake;

      int            initialisation_length;
      unsigned char *initialisation;
  } hdb_unknownconf_t;

/* --- */

typedef struct
  {
    hdb_reg_t         command_resetlink;
    hdb_reg_t         command_startlink;
    hdb_reg_t         command_resetoutput;
    hdb_reg_t         command_wrongparity;
    hdb_reg_t         data_mode[HDB_MAX_ROUTER_LINKS];
    BOOL              data_connected[HDB_MAX_ROUTER_LINKS];
    hdb_reg_t         ctrl_mode[2];
    BOOL              ctrl_down_connected;
  } hdb_c104_link_regs_t;

/* --- */

typedef hdb_reg_t hdb_interval_regs_t[HDB_MAX_ROUTER_INTERVALS];

typedef struct
  {
    BOOL                  packet_relevent;
    hdb_reg_t             mode;
    hdb_interval_regs_t  *interval_regs;
    hdb_reg_t             random_base;
    hdb_reg_t             random_range;
    hdb_reg_t             random_seed;
  } hdb_c104_one_packet_regs_t;

typedef struct
  {
    hdb_interval_regs_t         *commonest_interval_regs;
    hdb_reg_t                    command_resetlogic;
    hdb_c104_one_packet_regs_t   packet[HDB_MAX_ROUTER_LINKS];
  } hdb_c104_packet_regs_t;

/* --- */

typedef struct
  {
    hdb_reg_t         dslinkpll;
  } hdb_c104_sys_regs_t;

/* --- */

typedef struct
  {
    hdb_c104_link_regs_t     *link_regs;
    hdb_c104_packet_regs_t   *packet_regs;
    hdb_c104_sys_regs_t      *sys_regs;
  } hdb_routeconf_t;

/* --- */

typedef struct
  {
    hdb_reg_t         command_resetlink;
    hdb_reg_t         command_startlink;
    hdb_reg_t         command_resetoutput;
    hdb_reg_t         command_wrongparity;
    hdb_reg_t         data_mode[HDB_MAX_PROTOCOL_CONVERTER_LINKS];
    BOOL              data_connected[HDB_MAX_PROTOCOL_CONVERTER_LINKS];
    hdb_reg_t         ctrl_mode[2];
    BOOL              ctrl_down_connected;
  } hdb_c100_link_regs_t;

/* --- */

typedef struct
  {
    hdb_reg_t         dslinkpll;
  } hdb_c100_sys_regs_t;

/* --- */

typedef struct
  {
    hdb_c100_link_regs_t     *link_regs;
    hdb_c100_sys_regs_t      *sys_regs;
  } hdb_c100conf_t;

/* --- */

typedef struct
  {
    int              separators[HDB_MAX_ROUTER_INTERVALS+1];
    int              linksel   [HDB_MAX_ROUTER_INTERVALS];
    BOOL             discard   [HDB_MAX_ROUTER_INTERVALS];

    BOOL             randomising[HDB_MAX_ROUTER_LINKS];
    BOOL             deleting   [HDB_MAX_ROUTER_LINKS];

    int              random_base;
    int              random_range;

    BIT32            links_mask;
    hdb_partition_t  partition;
  } hdb_inttable_t;

typedef hdb_inttable_t *hdb_routeint_t[HDB_MAX_ROUTER_LINKS];

/* --- */

typedef int hdb_link_number_t;

typedef enum
    {
        hdb_host_link,
        hdb_edge_point,
        hdb_control_link,
        hdb_compute_link,
        hdb_router_link,
        hdb_protocol_converter_link
    } hdb_point_type_t;

typedef enum
    {
        hdb_control_connection,
        hdb_data_connection
    } hdb_conn_type_t;

typedef struct
    {
        hdb_handle_t       de;
        hdb_point_type_t   type;
        hdb_link_number_t  link;
    } hdb_conn_point_t;

typedef struct 
    {
        hdb_name_t       name;
        hdb_conn_type_t  conn_type;
        hdb_conn_point_t end[2];
    } hdb_conndata_t;

/* --- */

typedef struct
    {
        hdb_name_t           name;
        hdb_handle_t         endpoint[2];
        hdb_conn_t          *arclist[2];
    } hdb_routedata_t;

/* --- */

typedef enum
  {
    hdb_name_unknown,
    hdb_name_wrong_num_subscripts,
    hdb_name_bad_subscript,
    hdb_name_device_proc,
    hdb_name_device_router,
    hdb_name_device_unknown,
    hdb_name_device_protocol_converter,
    hdb_name_router_partition,
    hdb_name_connection,
    hdb_name_route,
    hdb_name_edge,
    hdb_name_controlport
  } hdb_name_type_t;

/* --- */

typedef struct
  {
    hdb_name_t      path_name;		/* Name or null ptr */
    BOOL            path_packetised;
    hdb_handle_t    de[2];
    int             on_link[2];         /* Output link number */
    char           *path_header[2];
    int             path_header_len[2];
    int             path_length[2];
    BOOL            path_is_randomising[2];
    void           *path_user_data;
  } hdb_pathdata_t;

/* --- */

struct hdb_treenode_s
  {
    int                   index;
    int                   end_index;
    hdb_treenode_t       *up, *down, *along;
    hdb_device_t          dev;
    int                   up_channel_num;
    hdb_path_t            up_path;
    int                   num_down_paths;
    hdb_path_t            down_path[HDB_MAX_BRANCHES];
 };

/* --- */

typedef enum
  { /* validity flags: T=Txxx, H=T9000, C=C104, P=partition, U=Unknown, L=C100 */
    hdb_devattr_type,                 /* []BYTE     THCPUL  */
    hdb_devattr_link,                 /* []EDGE     THC  L  */
    hdb_devattr_links,                /* [][2]INT      P    */
    hdb_devattr_link_speed_multiply,  /* INT         HC  L  */
    hdb_devattr_link_speed_divide,    /* []INT       HC  L  */
    hdb_devattr_control,              /* [2]EDGE     HC  L  */
    hdb_devattr_control_speed_divide, /* [2]INT      HC  L  */
    hdb_devattr_root,                 /* BOOL       TH      */
    hdb_devattr_memsize,              /* INT        T       */
    hdb_devattr_romsize,              /* INT        T       */
    hdb_devattr_memstart,             /* INT        T       */
    hdb_devattr_num_links,            /* INT        T       */
    hdb_devattr_memconfig,            /* []BYTE      H      */
    hdb_devattr_preamble,             /* []BYTE      H      */
    hdb_devattr_bootstrap,            /* []BYTE      H      */
    hdb_devattr_rom_file,             /* []BYTE     TH      */
    hdb_devattr_memory,               /* [][3]INT    H      */
    hdb_devattr_cachesize,            /* INT         H      */
    hdb_devattr_link_byte_mode,       /* [4]BOOL     H      */
    hdb_devattr_event_out,            /* [4]BOOL     H      */
    hdb_devattr_local_rom,            /* BOOL        H      */
    hdb_devattr_system_rom,           /* BOOL        H      */
    hdb_devattr_reboot_from_link,     /* BOOL        H      */
    hdb_devattr_boot_from_rom,        /* BOOL        H      */
    hdb_devattr_pmi_config_inrom,     /* BOOL        H      */
    hdb_devattr_cache_config_inrom,   /* BOOL        H      */
    hdb_devattr_linkset_inrom,        /* BOOL        H      */
    hdb_devattr_bootstrap_inrom,      /* BOOL        H      */
    hdb_devattr_link_groups,          /* [][2]INT     C     */
    hdb_devattr_interval_separator,   /* [][2]INT     CP    */
    hdb_devattr_link_select,          /* []INT        CP    */
    hdb_devattr_discard,              /* []BOOL       CP    */
    hdb_devattr_delete,               /* []BOOL       CP    */
    hdb_devattr_random,               /* []BOOL       CP    */
    hdb_devattr_random_interval,      /* [2]INT       CP    */
    hdb_devattr_start_code,           /* BYTE           U   */
    hdb_devattr_identify_code,        /* BYTE           U   */
    hdb_devattr_identify_response,    /* BYTE           U   */
    hdb_devattr_expected_identity,    /* INT            U   */
    hdb_devattr_error_response,       /* BYTE           U   */
    hdb_devattr_error_handshake,      /* BYTE           U   */
    hdb_devattr_initialisation,       /* []INT          U   */
    hdb_devattr_mode                  /* INT             L  */
  } hdb_devattr_t;

/* ------------------------------------------------------------------------- */
/* --- Global HDB functions --- */

INT32          hdb_version( void );

hdb_status_t   hdb_open( INT32              header_version,
                         hdb_hardware_t    *handle, 
                         hdb_handler_fn_t   error_function,
                         void              *user_handler_parameter );

hdb_status_t   hdb_close( hdb_hardware_t   *handle );

hdb_status_t   hdb_read_ndl( hdb_hardware_t   handle, 
                             const char      *filename );

hdb_status_t   hdb_read_ndb( hdb_hardware_t   handle, 
                             const char      *filename );

hdb_status_t   hdb_write_ndb( hdb_hardware_t   handle, 
                              const char      *filename );

hdb_status_t   hdb_list_ndb( FILE            *infile,
                             FILE            *outfile,
                             BOOL             show_record_addresses, 
                             BOOL             short_lines ); 

hdb_status_t   hdb_auto_label( hdb_hardware_t   handle,
                               BOOL             set_delete_at_edges,
                               BOOL             perform_auto_grouping,  
                               BOOL             allow_pruning );

hdb_status_t   hdb_write_labelling( hdb_hardware_t   handle,
                                    BOOL             print_delete_attributes,
                                    BOOL             print_grouping_attributes,
                                    const char      *filename );

hdb_status_t   hdb_check( hdb_hardware_t   handle );

hdb_status_t   hdb_memory_check( hdb_device_t  handle );

hdb_status_t   hdb_get_error_fn( hdb_hardware_t     handle,
                                 hdb_handler_fn_t  *error_function,
                                 void             **user_handler_parameter );

hdb_status_t   hdb_set_isearch_name( hdb_hardware_t     handle,
                                     const char        *isearch_name );

hdb_status_t   hdb_get_isearch_name( hdb_hardware_t     handle,
                                     const char       **isearch_name );


void           hdb_set_debug_level( int level );
int            hdb_get_debug_level( void );

/* ------------------------------------------------------------------------- */
/* --- HDB Constructor functions --- */

hdb_status_t   hdb_set_source_filename( hdb_hardware_t     handle,
                                        const char        *filename );

hdb_status_t   hdb_get_source_filename( hdb_hardware_t     handle,
                                        const char       **filename );

hdb_status_t   hdb_create_name( hdb_hardware_t     handle,
                                const char        *name,
                                int                num_dimensions,
                                const int         *dimensions,
                                hdb_name_type_t    type,
                                hdb_basic_name_t  *base_name );

hdb_status_t   hdb_index_name( hdb_basic_name_t   base_name,
                               const int         *subscripts,
                               hdb_handle_t      *item );

hdb_status_t   hdb_set_network_type( hdb_hardware_t     handle,
                                     hdb_style_t        type );

hdb_status_t   hdb_set_connection( hdb_handle_t connection,
                                   hdb_handle_t handle_0,
                                   int          link_num_0,
                                   BOOL         control_link_0,
                                   hdb_handle_t handle_1,
                                   int          link_num_1,
                                   BOOL         control_link_1 );

hdb_status_t   hdb_map_edge_onto_pc_ds_link( hdb_handle_t     edge,
                                             hdb_handle_t     protocol_converter,
                                             int              ds_link );

hdb_status_t   hdb_map_controlport_onto_pc_ds_link( hdb_handle_t     controlport,
                                                    BOOL             control,
                                                    hdb_handle_t     protocol_converter,
                                                    int              ds_link );

hdb_status_t   hdb_set_route( hdb_handle_t   route,
                              hdb_handle_t  *outward_arclist,
                              int            outward_arclist_length,
                              hdb_handle_t  *return_arclist,
                              int            return_arclist_length );

hdb_status_t   hdb_set_c104_partition(   hdb_handle_t     item,
                                         hdb_handle_t     partition_handle,
                                         const char      *vector,
                                         int              vector_length,
                                         int              elements );

hdb_status_t   hdb_set_scalar_attribute( hdb_handle_t     item,
                                         hdb_devattr_t    attr,
                                         int              value );

hdb_status_t   hdb_set_vector_attribute( hdb_handle_t     item,
                                         hdb_devattr_t    attr,
                                         const char      *vector,
                                         int              vector_length,
                                         int              elements );

/* ------------------------------------------------------------------------- */
/* --- Consistency check functions --- */

hdb_status_t   hdb_in_hardware_model( hdb_hardware_t   model,
                                      hdb_handle_t     item );

hdb_status_t   hdb_in_same_hardware_model( hdb_handle_t   itema,
                                           hdb_handle_t   itemb );

/* ------------------------------------------------------------------------- */
/* --- Functions to force load of memfiles, preambles, and bootstraps.   --- */

hdb_status_t   hdb_load_memfiles(   hdb_hardware_t   model );

hdb_status_t   hdb_load_bootstraps( hdb_hardware_t   model );

hdb_status_t   hdb_load_preambles(  hdb_hardware_t   model );

/* ------------------------------------------------------------------------- */
/* --- Name related functions --- */

hdb_status_t   hdb_apply_name( hdb_applies_t        type,
                               hdb_hardware_t       handle,
                               hdb_apply_name_fn_t  fn,
                               void                *user_parameter );

hdb_status_t   hdb_name_string( hdb_name_t name, char **string );

hdb_status_t   hdb_handle_string ( hdb_handle_t handle, char **string );

hdb_status_t   hdb_name_type( hdb_hardware_t    handle, 
                              const char       *name,
                              int               num_subscripts, 
                              const int        *subscripts,
                              hdb_name_type_t  *type );

hdb_status_t   hdb_name_error( hdb_hardware_t    handle,
                               const char       *name,
                               int               num_subscripts,
                               const int        *subscripts,
                               hdb_name_type_t   expected_type,
                               char            **string );

hdb_status_t hdb_endpoints_description( hdb_handle_t    handle,
                                        char          **node_name_0,
                                        int            *link_num_0,
                                        hdb_applies_t  *type_0,
                                        char          **node_name_1,
                                        int            *link_num_1,
                                        hdb_applies_t  *type_1 );

/* ------------------------------------------------------------------------- */
/* --- Interrogation functions --- */

hdb_status_t   hdb_get_net_data( hdb_hardware_t   handle, 
                                 hdb_netdata_t  **data );

hdb_status_t   hdb_get_host( hdb_hardware_t   handle, 
                             hdb_edge_t      *host_edge );

hdb_status_t   hdb_controlport_mappings( hdb_hardware_t        model,
                                         hdb_device_t         *data_device,
                                         int                  *data_ds_link,
                                         hdb_device_t         *control_device,
                                         int                  *control_ds_link );

hdb_status_t   hdb_get_root( hdb_hardware_t   handle, 
                             hdb_device_t    *root );

hdb_status_t   hdb_apply( hdb_applies_t    type, 
                          hdb_hardware_t   handle, 
                          hdb_apply_fn_t   fn,
                          void            *user_parameter );

hdb_status_t   hdb_get_device_data( hdb_device_t       devhandle, 
                                    hdb_devicedata_t **data);

hdb_status_t   hdb_get_device_rom_file( hdb_device_t       devhandle, 
                                        char             **filename );

hdb_status_t   hdb_bpw( hdb_dident_t   devtype, 
                        int           *bpw );

hdb_status_t   hdb_valid_memstart( hdb_device_t devhandle,
                                   INT32        memstart);

hdb_status_t   hdb_vcp_memstart( int     num_vlcbs,
                                 int     num_resourceable_vlcbs,
                                 int     num_input_vlcbs,
                                 INT32   header_area_size,
                                 INT32  *memstart );

hdb_status_t   hdb_get_dev_label( hdb_device_t   devhandle, 
                                  hdb_devlab_t **data );

hdb_status_t   hdb_get_proc_mem( hdb_device_t      devhandle,
                                 mem_structure_t **data );

hdb_status_t   hdb_get_proc_conf( hdb_device_t    devhandle, 
                                  hdb_devconf_t **data );

hdb_status_t   hdb_get_unknown_conf( hdb_device_t        devhandle, 
                                     hdb_unknownconf_t **data );

hdb_status_t   hdb_get_route_conf( hdb_device_t      devhandle, 
                                   hdb_routeconf_t **data );

hdb_status_t   hdb_get_protocol_converter_conf( hdb_device_t      devhandle, 
                                                hdb_c100conf_t  **data );

hdb_status_t   hdb_get_route_intervals( hdb_device_t     devhandle, 
                                        hdb_routeint_t **data );

hdb_status_t   hdb_get_conn_data( hdb_conn_t       connhandle, 
                                  hdb_conndata_t **data );

hdb_status_t   hdb_get_route_data( hdb_route_t       routehandle, 
                                   hdb_routedata_t **data );

hdb_status_t   hdb_is_connection_between( hdb_handle_t   de1,
                                          hdb_handle_t   de2 );

hdb_status_t   hdb_is_route_between( hdb_handle_t   de1,
                                     hdb_handle_t   de2 );

hdb_status_t   hdb_is_path_between( hdb_handle_t   de1,
                                    hdb_handle_t   de2 );

hdb_status_t   hdb_is_handle_pathable( hdb_handle_t   handle );

hdb_status_t   hdb_is_handle_named( hdb_handle_t   handle );

hdb_status_t   hdb_is_handle_initialised( hdb_handle_t   handle );

hdb_status_t   hdb_is_handle_data_connected( hdb_handle_t   handle );

hdb_status_t   hdb_c104_connectivity_status( hdb_handle_t   handle,
                                             BOOL          *in_data_network,
                                             BOOL          *in_control_network,
                                             BIT32         *links_used_in_control_network,
                                             int           *feedback_link_from_control_down );

hdb_status_t   hdb_handle_type( hdb_handle_t    handle, 
                                hdb_applies_t  *type );

hdb_status_t   hdb_dev_from_header( hdb_hardware_t   handle, 
                                    hdb_header_t     header,
                                    hdb_device_t    *dev );

hdb_status_t   hdb_dev_from_name( hdb_hardware_t   handle, 
                                  const char      *name, 
			          int              num_subscripts, 
                                  const int       *subscripts, 
                                  hdb_device_t    *dev );

hdb_status_t   hdb_conn_from_name( hdb_hardware_t   handle,
                                   const char      *name,
                                   int              num_subscripts,
                                   const int       *subscripts,
                                   hdb_conn_t      *connection );

hdb_status_t   hdb_route_from_name( hdb_hardware_t   handle,
                                    const char      *name,
                                    int              num_subscripts,
                                    const int       *subscripts,
                                    hdb_route_t     *route );

hdb_status_t   hdb_edge_from_name( hdb_hardware_t   handle,
                                   const char      *name,
                                   int              num_subscripts,
                                   const int       *subscripts,
                                   hdb_edge_t      *edge );

hdb_status_t   hdb_handle_from_name( hdb_hardware_t   handle,
                                     const char      *name,
                                     int              num_subscripts,
                                     const int       *subscripts,
                                     hdb_handle_t    *generic_handle );

hdb_status_t   hdb_path_from_name( hdb_hardware_t   handle,
                                   const char      *name,
                                   int              num_subscripts,
                                   const int       *subscripts,
                                   hdb_handle_t     de1,
                                   hdb_handle_t     de2,
                                   hdb_path_t      *path );

hdb_status_t   hdb_path_from_handle( hdb_handle_t     handle,
                                     hdb_handle_t     de1,
                                     hdb_handle_t     de2,
                                     hdb_path_t      *path );

hdb_status_t   hdb_path_from_conn( hdb_conn_t   conn, 
                                   hdb_path_t  *path );

hdb_status_t   hdb_path_from_route( hdb_handle_t   de1, 
                                    hdb_handle_t   de2,
                                    hdb_route_t    route, 
                                    hdb_path_t    *path );

hdb_status_t    hdb_get_equivalent_paths( hdb_path_t      base_path,
                                          int             number_of_paths_wanted,
                                          int            *number_of_equivalent_paths,
                                          hdb_path_t    **equivalent_paths );

hdb_status_t   hdb_get_path_list( hdb_handle_t   de1, 
                                  hdb_handle_t   de2, 
                                  int            maxpaths, 
                                  int           *actualpaths, 
                                  hdb_path_t   **paths);

hdb_status_t   hdb_get_path( hdb_handle_t   de1, 
                             hdb_handle_t   de2, 
                             hdb_path_t    *path );

hdb_status_t   hdb_get_path_data( hdb_path_t       pathhandle, 
                                  hdb_pathdata_t **data );

hdb_status_t   hdb_get_bootpath( hdb_hardware_t   handle, 
                                 hdb_treenode_t **data );

hdb_status_t   hdb_rom_hinit( hdb_hardware_t   handle, 
                              unsigned char  **data, 
                              INT32           *length );

hdb_status_t   hdb_get_signal_code( hdb_hardware_t   handle,
                                    BOOL             stop,
                                    unsigned char  **data, 
                                    INT32           *length );

/* ------------------------------------------------------------------------- */
/* --- Memory release functions --- */

hdb_status_t   hdb_free_rom_hinit( unsigned char  **ptr );

/* ------------------------------------------------------------------------- */
/* --- Private HDB functions --- */

void  hdb_free( void *memptr );

void *hdb_malloc( hdb_hardware_t   handle, 
                  unsigned int     size );

#endif
