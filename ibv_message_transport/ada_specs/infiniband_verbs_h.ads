pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;
with Interfaces.C.Strings;
with System;
with stddef_h;
with x86_64_linux_gnu_bits_pthreadtypes_h;

package infiniband_verbs_h is

   --  unsupported macro: BEGIN_C_DECLS extern "C" {
   --  unsupported macro: END_C_DECLS }
   --  arg-macro: function container_of (ptr, type, member)
   --    return (type *) ((uint8_t *)(ptr) - offsetof(type, member));
   --  arg-macro: function vext_field_avail (type, fld, sz)
   --    return offsetof(type, fld) < (sz);
   --  arg-macro: function verbs_get_ctx_op (ctx, op)
   --    return { struct verbs_context *vctx := verbs_get_ctx(ctx); (notvctx  or else  (vctx.sz < sizeof(*vctx) - offsetof(struct verbs_context, op))  or else  notvctx.op) ? NULL : vctx; };
   --  arg-macro: function verbs_set_ctx_op (_vctx, op, ptr)
   --    return { struct verbs_context *vctx := _vctx; if (vctx  and then  (vctx.sz >= sizeof(*vctx) - offsetof(struct verbs_context, op))) vctx.op := ptr; };
   --  arg-macro: procedure ibv_query_port (context, port_num, port_attr)
   --    ___ibv_query_port(context, port_num, port_attr)
   type ibv_gid;
   type anon_24 is record
      subnet_prefix : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:63
      interface_id : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:64
   end record;
   pragma Convention (C_Pass_By_Copy, anon_24);
   type ibv_gid_raw_array is array (0 .. 15) of aliased stdint_h.uint8_t;
   type ibv_gid (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            raw : aliased ibv_gid_raw_array;  -- /usr/include/infiniband/verbs.h:61
         when others =>
            global : aliased anon_24;  -- /usr/include/infiniband/verbs.h:65
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_gid);
   pragma Unchecked_Union (ibv_gid);  -- /usr/include/infiniband/verbs.h:60

   subtype ibv_node_type is unsigned;
   IBV_NODE_UNKNOWN : constant ibv_node_type := -1;
   IBV_NODE_CA : constant ibv_node_type := 1;
   IBV_NODE_SWITCH : constant ibv_node_type := 2;
   IBV_NODE_ROUTER : constant ibv_node_type := 3;
   IBV_NODE_RNIC : constant ibv_node_type := 4;
   IBV_NODE_USNIC : constant ibv_node_type := 5;
   IBV_NODE_USNIC_UDP : constant ibv_node_type := 6;  -- /usr/include/infiniband/verbs.h:84

   subtype ibv_transport_type is unsigned;
   IBV_TRANSPORT_UNKNOWN : constant ibv_transport_type := -1;
   IBV_TRANSPORT_IB : constant ibv_transport_type := 0;
   IBV_TRANSPORT_IWARP : constant ibv_transport_type := 1;
   IBV_TRANSPORT_USNIC : constant ibv_transport_type := 2;
   IBV_TRANSPORT_USNIC_UDP : constant ibv_transport_type := 3;  -- /usr/include/infiniband/verbs.h:94

   subtype ibv_device_cap_flags is unsigned;
   IBV_DEVICE_RESIZE_MAX_WR : constant ibv_device_cap_flags := 1;
   IBV_DEVICE_BAD_PKEY_CNTR : constant ibv_device_cap_flags := 2;
   IBV_DEVICE_BAD_QKEY_CNTR : constant ibv_device_cap_flags := 4;
   IBV_DEVICE_RAW_MULTI : constant ibv_device_cap_flags := 8;
   IBV_DEVICE_AUTO_PATH_MIG : constant ibv_device_cap_flags := 16;
   IBV_DEVICE_CHANGE_PHY_PORT : constant ibv_device_cap_flags := 32;
   IBV_DEVICE_UD_AV_PORT_ENFORCE : constant ibv_device_cap_flags := 64;
   IBV_DEVICE_CURR_QP_STATE_MOD : constant ibv_device_cap_flags := 128;
   IBV_DEVICE_SHUTDOWN_PORT : constant ibv_device_cap_flags := 256;
   IBV_DEVICE_INIT_TYPE : constant ibv_device_cap_flags := 512;
   IBV_DEVICE_PORT_ACTIVE_EVENT : constant ibv_device_cap_flags := 1024;
   IBV_DEVICE_SYS_IMAGE_GUID : constant ibv_device_cap_flags := 2048;
   IBV_DEVICE_RC_RNR_NAK_GEN : constant ibv_device_cap_flags := 4096;
   IBV_DEVICE_SRQ_RESIZE : constant ibv_device_cap_flags := 8192;
   IBV_DEVICE_N_NOTIFY_CQ : constant ibv_device_cap_flags := 16384;
   IBV_DEVICE_IP_CSUM : constant ibv_device_cap_flags := 262144;
   IBV_DEVICE_XRC : constant ibv_device_cap_flags := 1048576;
   IBV_DEVICE_MANAGED_FLOW_STEERING : constant ibv_device_cap_flags := 536870912;  -- /usr/include/infiniband/verbs.h:102

   type ibv_atomic_cap is 
     (IBV_ATOMIC_NONE,
      IBV_ATOMIC_HCA,
      IBV_ATOMIC_GLOB);
   pragma Convention (C, ibv_atomic_cap);  -- /usr/include/infiniband/verbs.h:123

   subtype ibv_device_attr_fw_ver_array is Interfaces.C.char_array (0 .. 63);
   type ibv_device_attr is record
      fw_ver : aliased ibv_device_attr_fw_ver_array;  -- /usr/include/infiniband/verbs.h:130
      node_guid : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:131
      sys_image_guid : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:132
      max_mr_size : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:133
      page_size_cap : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:134
      vendor_id : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:135
      vendor_part_id : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:136
      hw_ver : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:137
      max_qp : aliased int;  -- /usr/include/infiniband/verbs.h:138
      max_qp_wr : aliased int;  -- /usr/include/infiniband/verbs.h:139
      device_cap_flags : aliased int;  -- /usr/include/infiniband/verbs.h:140
      max_sge : aliased int;  -- /usr/include/infiniband/verbs.h:141
      max_sge_rd : aliased int;  -- /usr/include/infiniband/verbs.h:142
      max_cq : aliased int;  -- /usr/include/infiniband/verbs.h:143
      max_cqe : aliased int;  -- /usr/include/infiniband/verbs.h:144
      max_mr : aliased int;  -- /usr/include/infiniband/verbs.h:145
      max_pd : aliased int;  -- /usr/include/infiniband/verbs.h:146
      max_qp_rd_atom : aliased int;  -- /usr/include/infiniband/verbs.h:147
      max_ee_rd_atom : aliased int;  -- /usr/include/infiniband/verbs.h:148
      max_res_rd_atom : aliased int;  -- /usr/include/infiniband/verbs.h:149
      max_qp_init_rd_atom : aliased int;  -- /usr/include/infiniband/verbs.h:150
      max_ee_init_rd_atom : aliased int;  -- /usr/include/infiniband/verbs.h:151
      atomic_cap : aliased ibv_atomic_cap;  -- /usr/include/infiniband/verbs.h:152
      max_ee : aliased int;  -- /usr/include/infiniband/verbs.h:153
      max_rdd : aliased int;  -- /usr/include/infiniband/verbs.h:154
      max_mw : aliased int;  -- /usr/include/infiniband/verbs.h:155
      max_raw_ipv6_qp : aliased int;  -- /usr/include/infiniband/verbs.h:156
      max_raw_ethy_qp : aliased int;  -- /usr/include/infiniband/verbs.h:157
      max_mcast_grp : aliased int;  -- /usr/include/infiniband/verbs.h:158
      max_mcast_qp_attach : aliased int;  -- /usr/include/infiniband/verbs.h:159
      max_total_mcast_qp_attach : aliased int;  -- /usr/include/infiniband/verbs.h:160
      max_ah : aliased int;  -- /usr/include/infiniband/verbs.h:161
      max_fmr : aliased int;  -- /usr/include/infiniband/verbs.h:162
      max_map_per_fmr : aliased int;  -- /usr/include/infiniband/verbs.h:163
      max_srq : aliased int;  -- /usr/include/infiniband/verbs.h:164
      max_srq_wr : aliased int;  -- /usr/include/infiniband/verbs.h:165
      max_srq_sge : aliased int;  -- /usr/include/infiniband/verbs.h:166
      max_pkeys : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:167
      local_ca_ack_delay : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:168
      phys_port_cnt : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:169
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_device_attr);  -- /usr/include/infiniband/verbs.h:129

   subtype ibv_mtu is unsigned;
   IBV_MTU_256 : constant ibv_mtu := 1;
   IBV_MTU_512 : constant ibv_mtu := 2;
   IBV_MTU_1024 : constant ibv_mtu := 3;
   IBV_MTU_2048 : constant ibv_mtu := 4;
   IBV_MTU_4096 : constant ibv_mtu := 5;  -- /usr/include/infiniband/verbs.h:172

   type ibv_port_state is 
     (IBV_PORT_NOP,
      IBV_PORT_DOWN,
      IBV_PORT_INIT,
      IBV_PORT_ARMED,
      IBV_PORT_ACTIVE,
      IBV_PORT_ACTIVE_DEFER);
   pragma Convention (C, ibv_port_state);  -- /usr/include/infiniband/verbs.h:180

   type ibv_port_attr is record
      state : aliased ibv_port_state;  -- /usr/include/infiniband/verbs.h:196
      max_mtu : aliased ibv_mtu;  -- /usr/include/infiniband/verbs.h:197
      active_mtu : aliased ibv_mtu;  -- /usr/include/infiniband/verbs.h:198
      gid_tbl_len : aliased int;  -- /usr/include/infiniband/verbs.h:199
      port_cap_flags : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:200
      max_msg_sz : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:201
      bad_pkey_cntr : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:202
      qkey_viol_cntr : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:203
      pkey_tbl_len : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:204
      lid : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:205
      sm_lid : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:206
      lmc : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:207
      max_vl_num : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:208
      sm_sl : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:209
      subnet_timeout : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:210
      init_type_reply : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:211
      active_width : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:212
      active_speed : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:213
      phys_state : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:214
      link_layer : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:215
      reserved : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:216
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_port_attr);  -- /usr/include/infiniband/verbs.h:195

   type ibv_event_type is 
     (IBV_EVENT_CQ_ERR,
      IBV_EVENT_QP_FATAL,
      IBV_EVENT_QP_REQ_ERR,
      IBV_EVENT_QP_ACCESS_ERR,
      IBV_EVENT_COMM_EST,
      IBV_EVENT_SQ_DRAINED,
      IBV_EVENT_PATH_MIG,
      IBV_EVENT_PATH_MIG_ERR,
      IBV_EVENT_DEVICE_FATAL,
      IBV_EVENT_PORT_ACTIVE,
      IBV_EVENT_PORT_ERR,
      IBV_EVENT_LID_CHANGE,
      IBV_EVENT_PKEY_CHANGE,
      IBV_EVENT_SM_CHANGE,
      IBV_EVENT_SRQ_ERR,
      IBV_EVENT_SRQ_LIMIT_REACHED,
      IBV_EVENT_QP_LAST_WQE_REACHED,
      IBV_EVENT_CLIENT_REREGISTER,
      IBV_EVENT_GID_CHANGE);
   pragma Convention (C, ibv_event_type);  -- /usr/include/infiniband/verbs.h:219

   type ibv_async_event;
   type ibv_cq;
   type ibv_qp;
   type ibv_srq;
   type anon_26 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            cq : access ibv_cq;  -- /usr/include/infiniband/verbs.h:243
         when 1 =>
            qp : access ibv_qp;  -- /usr/include/infiniband/verbs.h:244
         when 2 =>
            srq : access ibv_srq;  -- /usr/include/infiniband/verbs.h:245
         when others =>
            port_num : aliased int;  -- /usr/include/infiniband/verbs.h:246
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_26);
   pragma Unchecked_Union (anon_26);type ibv_async_event is record
      element : aliased anon_26;  -- /usr/include/infiniband/verbs.h:247
      event_type : aliased ibv_event_type;  -- /usr/include/infiniband/verbs.h:248
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_async_event);  -- /usr/include/infiniband/verbs.h:241

   type ibv_wc_status is 
     (IBV_WC_SUCCESS,
      IBV_WC_LOC_LEN_ERR,
      IBV_WC_LOC_QP_OP_ERR,
      IBV_WC_LOC_EEC_OP_ERR,
      IBV_WC_LOC_PROT_ERR,
      IBV_WC_WR_FLUSH_ERR,
      IBV_WC_MW_BIND_ERR,
      IBV_WC_BAD_RESP_ERR,
      IBV_WC_LOC_ACCESS_ERR,
      IBV_WC_REM_INV_REQ_ERR,
      IBV_WC_REM_ACCESS_ERR,
      IBV_WC_REM_OP_ERR,
      IBV_WC_RETRY_EXC_ERR,
      IBV_WC_RNR_RETRY_EXC_ERR,
      IBV_WC_LOC_RDD_VIOL_ERR,
      IBV_WC_REM_INV_RD_REQ_ERR,
      IBV_WC_REM_ABORT_ERR,
      IBV_WC_INV_EECN_ERR,
      IBV_WC_INV_EEC_STATE_ERR,
      IBV_WC_FATAL_ERR,
      IBV_WC_RESP_TIMEOUT_ERR,
      IBV_WC_GENERAL_ERR);
   pragma Convention (C, ibv_wc_status);  -- /usr/include/infiniband/verbs.h:251

   function ibv_wc_status_str (status : ibv_wc_status) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/infiniband/verbs.h:275
   pragma Import (C, ibv_wc_status_str, "ibv_wc_status_str");

   subtype ibv_wc_opcode is unsigned;
   IBV_WC_SEND : constant ibv_wc_opcode := 0;
   IBV_WC_RDMA_WRITE : constant ibv_wc_opcode := 1;
   IBV_WC_RDMA_READ : constant ibv_wc_opcode := 2;
   IBV_WC_COMP_SWAP : constant ibv_wc_opcode := 3;
   IBV_WC_FETCH_ADD : constant ibv_wc_opcode := 4;
   IBV_WC_BIND_MW : constant ibv_wc_opcode := 5;
   IBV_WC_RECV : constant ibv_wc_opcode := 128;
   IBV_WC_RECV_RDMA_WITH_IMM : constant ibv_wc_opcode := 129;  -- /usr/include/infiniband/verbs.h:277

   subtype ibv_wc_flags is unsigned;
   IBV_WC_GRH : constant ibv_wc_flags := 1;
   IBV_WC_WITH_IMM : constant ibv_wc_flags := 2;  -- /usr/include/infiniband/verbs.h:292

   type ibv_wc is record
      wr_id : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:298
      status : aliased ibv_wc_status;  -- /usr/include/infiniband/verbs.h:299
      opcode : aliased ibv_wc_opcode;  -- /usr/include/infiniband/verbs.h:300
      vendor_err : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:301
      byte_len : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:302
      imm_data : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:303
      qp_num : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:304
      src_qp : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:305
      wc_flags : aliased int;  -- /usr/include/infiniband/verbs.h:306
      pkey_index : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:307
      slid : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:308
      sl : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:309
      dlid_path_bits : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:310
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_wc);  -- /usr/include/infiniband/verbs.h:297

   subtype ibv_access_flags is unsigned;
   IBV_ACCESS_LOCAL_WRITE : constant ibv_access_flags := 1;
   IBV_ACCESS_REMOTE_WRITE : constant ibv_access_flags := 2;
   IBV_ACCESS_REMOTE_READ : constant ibv_access_flags := 4;
   IBV_ACCESS_REMOTE_ATOMIC : constant ibv_access_flags := 8;
   IBV_ACCESS_MW_BIND : constant ibv_access_flags := 16;  -- /usr/include/infiniband/verbs.h:313

   type ibv_context;
   type ibv_pd is record
      context : access ibv_context;  -- /usr/include/infiniband/verbs.h:322
      handle : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:323
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_pd);  -- /usr/include/infiniband/verbs.h:321

   subtype ibv_xrcd_init_attr_mask is unsigned;
   IBV_XRCD_INIT_ATTR_FD : constant ibv_xrcd_init_attr_mask := 1;
   IBV_XRCD_INIT_ATTR_OFLAGS : constant ibv_xrcd_init_attr_mask := 2;
   IBV_XRCD_INIT_ATTR_RESERVED : constant ibv_xrcd_init_attr_mask := 4;  -- /usr/include/infiniband/verbs.h:326

   type ibv_xrcd_init_attr is record
      comp_mask : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:333
      fd : aliased int;  -- /usr/include/infiniband/verbs.h:334
      oflags : aliased int;  -- /usr/include/infiniband/verbs.h:335
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_xrcd_init_attr);  -- /usr/include/infiniband/verbs.h:332

   type ibv_xrcd is record
      context : access ibv_context;  -- /usr/include/infiniband/verbs.h:339
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_xrcd);  -- /usr/include/infiniband/verbs.h:338

   subtype ibv_rereg_mr_flags is unsigned;
   IBV_REREG_MR_CHANGE_TRANSLATION : constant ibv_rereg_mr_flags := 1;
   IBV_REREG_MR_CHANGE_PD : constant ibv_rereg_mr_flags := 2;
   IBV_REREG_MR_CHANGE_ACCESS : constant ibv_rereg_mr_flags := 4;
   IBV_REREG_MR_KEEP_VALID : constant ibv_rereg_mr_flags := 8;  -- /usr/include/infiniband/verbs.h:342

   type ibv_mr is record
      context : access ibv_context;  -- /usr/include/infiniband/verbs.h:350
      pd : access ibv_pd;  -- /usr/include/infiniband/verbs.h:351
      addr : System.Address;  -- /usr/include/infiniband/verbs.h:352
      length : aliased stddef_h.size_t;  -- /usr/include/infiniband/verbs.h:353
      handle : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:354
      lkey : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:355
      rkey : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:356
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_mr);  -- /usr/include/infiniband/verbs.h:349

   subtype ibv_mw_type is unsigned;
   IBV_MW_TYPE_1 : constant ibv_mw_type := 1;
   IBV_MW_TYPE_2 : constant ibv_mw_type := 2;  -- /usr/include/infiniband/verbs.h:359

   type ibv_mw is record
      context : access ibv_context;  -- /usr/include/infiniband/verbs.h:365
      pd : access ibv_pd;  -- /usr/include/infiniband/verbs.h:366
      rkey : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:367
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_mw);  -- /usr/include/infiniband/verbs.h:364

   type ibv_global_route is record
      dgid : aliased ibv_gid;  -- /usr/include/infiniband/verbs.h:371
      flow_label : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:372
      sgid_index : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:373
      hop_limit : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:374
      traffic_class : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:375
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_global_route);  -- /usr/include/infiniband/verbs.h:370

   type ibv_grh is record
      version_tclass_flow : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:379
      paylen : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:380
      next_hdr : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:381
      hop_limit : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:382
      sgid : aliased ibv_gid;  -- /usr/include/infiniband/verbs.h:383
      dgid : aliased ibv_gid;  -- /usr/include/infiniband/verbs.h:384
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_grh);  -- /usr/include/infiniband/verbs.h:378

   subtype ibv_rate is unsigned;
   IBV_RATE_MAX : constant ibv_rate := 0;
   IBV_RATE_2_5_GBPS : constant ibv_rate := 2;
   IBV_RATE_5_GBPS : constant ibv_rate := 5;
   IBV_RATE_10_GBPS : constant ibv_rate := 3;
   IBV_RATE_20_GBPS : constant ibv_rate := 6;
   IBV_RATE_30_GBPS : constant ibv_rate := 4;
   IBV_RATE_40_GBPS : constant ibv_rate := 7;
   IBV_RATE_60_GBPS : constant ibv_rate := 8;
   IBV_RATE_80_GBPS : constant ibv_rate := 9;
   IBV_RATE_120_GBPS : constant ibv_rate := 10;
   IBV_RATE_14_GBPS : constant ibv_rate := 11;
   IBV_RATE_56_GBPS : constant ibv_rate := 12;
   IBV_RATE_112_GBPS : constant ibv_rate := 13;
   IBV_RATE_168_GBPS : constant ibv_rate := 14;
   IBV_RATE_25_GBPS : constant ibv_rate := 15;
   IBV_RATE_100_GBPS : constant ibv_rate := 16;
   IBV_RATE_200_GBPS : constant ibv_rate := 17;
   IBV_RATE_300_GBPS : constant ibv_rate := 18;  -- /usr/include/infiniband/verbs.h:387

   function ibv_rate_to_mult (rate : ibv_rate) return int;  -- /usr/include/infiniband/verbs.h:414
   pragma Import (C, ibv_rate_to_mult, "ibv_rate_to_mult");

   function mult_to_ibv_rate (mult : int) return ibv_rate;  -- /usr/include/infiniband/verbs.h:420
   pragma Import (C, mult_to_ibv_rate, "mult_to_ibv_rate");

   function ibv_rate_to_mbps (rate : ibv_rate) return int;  -- /usr/include/infiniband/verbs.h:427
   pragma Import (C, ibv_rate_to_mbps, "ibv_rate_to_mbps");

   function mbps_to_ibv_rate (mbps : int) return ibv_rate;  -- /usr/include/infiniband/verbs.h:433
   pragma Import (C, mbps_to_ibv_rate, "mbps_to_ibv_rate");

   type ibv_ah_attr is record
      grh : aliased ibv_global_route;  -- /usr/include/infiniband/verbs.h:436
      dlid : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:437
      sl : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:438
      src_path_bits : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:439
      static_rate : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:440
      is_global : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:441
      port_num : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:442
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_ah_attr);  -- /usr/include/infiniband/verbs.h:435

   subtype ibv_srq_attr_mask is unsigned;
   IBV_SRQ_MAX_WR : constant ibv_srq_attr_mask := 1;
   IBV_SRQ_LIMIT : constant ibv_srq_attr_mask := 2;  -- /usr/include/infiniband/verbs.h:445

   type ibv_srq_attr is record
      max_wr : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:451
      max_sge : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:452
      srq_limit : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:453
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_srq_attr);  -- /usr/include/infiniband/verbs.h:450

   type ibv_srq_init_attr is record
      srq_context : System.Address;  -- /usr/include/infiniband/verbs.h:457
      attr : aliased ibv_srq_attr;  -- /usr/include/infiniband/verbs.h:458
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_srq_init_attr);  -- /usr/include/infiniband/verbs.h:456

   type ibv_srq_type is 
     (IBV_SRQT_BASIC,
      IBV_SRQT_XRC);
   pragma Convention (C, ibv_srq_type);  -- /usr/include/infiniband/verbs.h:461

   subtype ibv_srq_init_attr_mask is unsigned;
   IBV_SRQ_INIT_ATTR_TYPE : constant ibv_srq_init_attr_mask := 1;
   IBV_SRQ_INIT_ATTR_PD : constant ibv_srq_init_attr_mask := 2;
   IBV_SRQ_INIT_ATTR_XRCD : constant ibv_srq_init_attr_mask := 4;
   IBV_SRQ_INIT_ATTR_CQ : constant ibv_srq_init_attr_mask := 8;
   IBV_SRQ_INIT_ATTR_RESERVED : constant ibv_srq_init_attr_mask := 16;  -- /usr/include/infiniband/verbs.h:466

   type ibv_srq_init_attr_ex is record
      srq_context : System.Address;  -- /usr/include/infiniband/verbs.h:475
      attr : aliased ibv_srq_attr;  -- /usr/include/infiniband/verbs.h:476
      comp_mask : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:478
      srq_type : aliased ibv_srq_type;  -- /usr/include/infiniband/verbs.h:479
      pd : access ibv_pd;  -- /usr/include/infiniband/verbs.h:480
      xrcd : access ibv_xrcd;  -- /usr/include/infiniband/verbs.h:481
      cq : access ibv_cq;  -- /usr/include/infiniband/verbs.h:482
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_srq_init_attr_ex);  -- /usr/include/infiniband/verbs.h:474

   subtype ibv_qp_type is unsigned;
   IBV_QPT_RC : constant ibv_qp_type := 2;
   IBV_QPT_UC : constant ibv_qp_type := 3;
   IBV_QPT_UD : constant ibv_qp_type := 4;
   IBV_QPT_RAW_PACKET : constant ibv_qp_type := 8;
   IBV_QPT_XRC_SEND : constant ibv_qp_type := 9;
   IBV_QPT_XRC_RECV : constant ibv_qp_type := 10;  -- /usr/include/infiniband/verbs.h:485

   type ibv_qp_cap is record
      max_send_wr : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:495
      max_recv_wr : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:496
      max_send_sge : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:497
      max_recv_sge : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:498
      max_inline_data : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:499
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_qp_cap);  -- /usr/include/infiniband/verbs.h:494

   type ibv_qp_init_attr is record
      qp_context : System.Address;  -- /usr/include/infiniband/verbs.h:503
      send_cq : access ibv_cq;  -- /usr/include/infiniband/verbs.h:504
      recv_cq : access ibv_cq;  -- /usr/include/infiniband/verbs.h:505
      srq : access ibv_srq;  -- /usr/include/infiniband/verbs.h:506
      cap : aliased ibv_qp_cap;  -- /usr/include/infiniband/verbs.h:507
      qp_type : aliased ibv_qp_type;  -- /usr/include/infiniband/verbs.h:508
      sq_sig_all : aliased int;  -- /usr/include/infiniband/verbs.h:509
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_qp_init_attr);  -- /usr/include/infiniband/verbs.h:502

   subtype ibv_qp_init_attr_mask is unsigned;
   IBV_QP_INIT_ATTR_PD : constant ibv_qp_init_attr_mask := 1;
   IBV_QP_INIT_ATTR_XRCD : constant ibv_qp_init_attr_mask := 2;
   IBV_QP_INIT_ATTR_RESERVED : constant ibv_qp_init_attr_mask := 4;  -- /usr/include/infiniband/verbs.h:512

   type ibv_qp_init_attr_ex is record
      qp_context : System.Address;  -- /usr/include/infiniband/verbs.h:519
      send_cq : access ibv_cq;  -- /usr/include/infiniband/verbs.h:520
      recv_cq : access ibv_cq;  -- /usr/include/infiniband/verbs.h:521
      srq : access ibv_srq;  -- /usr/include/infiniband/verbs.h:522
      cap : aliased ibv_qp_cap;  -- /usr/include/infiniband/verbs.h:523
      qp_type : aliased ibv_qp_type;  -- /usr/include/infiniband/verbs.h:524
      sq_sig_all : aliased int;  -- /usr/include/infiniband/verbs.h:525
      comp_mask : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:527
      pd : access ibv_pd;  -- /usr/include/infiniband/verbs.h:528
      xrcd : access ibv_xrcd;  -- /usr/include/infiniband/verbs.h:529
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_qp_init_attr_ex);  -- /usr/include/infiniband/verbs.h:518

   subtype ibv_qp_open_attr_mask is unsigned;
   IBV_QP_OPEN_ATTR_NUM : constant ibv_qp_open_attr_mask := 1;
   IBV_QP_OPEN_ATTR_XRCD : constant ibv_qp_open_attr_mask := 2;
   IBV_QP_OPEN_ATTR_CONTEXT : constant ibv_qp_open_attr_mask := 4;
   IBV_QP_OPEN_ATTR_TYPE : constant ibv_qp_open_attr_mask := 8;
   IBV_QP_OPEN_ATTR_RESERVED : constant ibv_qp_open_attr_mask := 16;  -- /usr/include/infiniband/verbs.h:532

   type ibv_qp_open_attr is record
      comp_mask : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:541
      qp_num : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:542
      xrcd : access ibv_xrcd;  -- /usr/include/infiniband/verbs.h:543
      qp_context : System.Address;  -- /usr/include/infiniband/verbs.h:544
      qp_type : aliased ibv_qp_type;  -- /usr/include/infiniband/verbs.h:545
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_qp_open_attr);  -- /usr/include/infiniband/verbs.h:540

   subtype ibv_qp_attr_mask is unsigned;
   IBV_QP_STATE_CONST : constant ibv_qp_attr_mask := 1;
   IBV_QP_CUR_STATE : constant ibv_qp_attr_mask := 2;
   IBV_QP_EN_SQD_ASYNC_NOTIFY : constant ibv_qp_attr_mask := 4;
   IBV_QP_ACCESS_FLAGS : constant ibv_qp_attr_mask := 8;
   IBV_QP_PKEY_INDEX : constant ibv_qp_attr_mask := 16;
   IBV_QP_PORT : constant ibv_qp_attr_mask := 32;
   IBV_QP_QKEY : constant ibv_qp_attr_mask := 64;
   IBV_QP_AV : constant ibv_qp_attr_mask := 128;
   IBV_QP_PATH_MTU : constant ibv_qp_attr_mask := 256;
   IBV_QP_TIMEOUT : constant ibv_qp_attr_mask := 512;
   IBV_QP_RETRY_CNT : constant ibv_qp_attr_mask := 1024;
   IBV_QP_RNR_RETRY : constant ibv_qp_attr_mask := 2048;
   IBV_QP_RQ_PSN : constant ibv_qp_attr_mask := 4096;
   IBV_QP_MAX_QP_RD_ATOMIC : constant ibv_qp_attr_mask := 8192;
   IBV_QP_ALT_PATH : constant ibv_qp_attr_mask := 16384;
   IBV_QP_MIN_RNR_TIMER : constant ibv_qp_attr_mask := 32768;
   IBV_QP_SQ_PSN : constant ibv_qp_attr_mask := 65536;
   IBV_QP_MAX_DEST_RD_ATOMIC : constant ibv_qp_attr_mask := 131072;
   IBV_QP_PATH_MIG_STATE : constant ibv_qp_attr_mask := 262144;
   IBV_QP_CAP_ENUM : constant ibv_qp_attr_mask := 524288;
   IBV_QP_DEST_QPN : constant ibv_qp_attr_mask := 1048576;  -- /usr/include/infiniband/verbs.h:548

   type ibv_qp_state is 
     (IBV_QPS_RESET,
      IBV_QPS_INIT,
      IBV_QPS_RTR,
      IBV_QPS_RTS,
      IBV_QPS_SQD,
      IBV_QPS_SQE,
      IBV_QPS_ERR,
      IBV_QPS_UNKNOWN);
   pragma Convention (C, ibv_qp_state);  -- /usr/include/infiniband/verbs.h:572

   type ibv_mig_state is 
     (IBV_MIG_MIGRATED,
      IBV_MIG_REARM,
      IBV_MIG_ARMED);
   pragma Convention (C, ibv_mig_state);  -- /usr/include/infiniband/verbs.h:583

   type ibv_qp_attr is record
      qp_state : aliased ibv_qp_state;  -- /usr/include/infiniband/verbs.h:590
      cur_qp_state : aliased ibv_qp_state;  -- /usr/include/infiniband/verbs.h:591
      path_mtu : aliased ibv_mtu;  -- /usr/include/infiniband/verbs.h:592
      path_mig_state : aliased ibv_mig_state;  -- /usr/include/infiniband/verbs.h:593
      qkey : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:594
      rq_psn : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:595
      sq_psn : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:596
      dest_qp_num : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:597
      qp_access_flags : aliased int;  -- /usr/include/infiniband/verbs.h:598
      cap : aliased ibv_qp_cap;  -- /usr/include/infiniband/verbs.h:599
      ah_attr : aliased ibv_ah_attr;  -- /usr/include/infiniband/verbs.h:600
      alt_ah_attr : aliased ibv_ah_attr;  -- /usr/include/infiniband/verbs.h:601
      pkey_index : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:602
      alt_pkey_index : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:603
      en_sqd_async_notify : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:604
      sq_draining : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:605
      max_rd_atomic : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:606
      max_dest_rd_atomic : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:607
      min_rnr_timer : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:608
      port_num : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:609
      timeout : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:610
      retry_cnt : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:611
      rnr_retry : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:612
      alt_port_num : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:613
      alt_timeout : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:614
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_qp_attr);  -- /usr/include/infiniband/verbs.h:589

   type ibv_wr_opcode is 
     (IBV_WR_RDMA_WRITE,
      IBV_WR_RDMA_WRITE_WITH_IMM,
      IBV_WR_SEND,
      IBV_WR_SEND_WITH_IMM,
      IBV_WR_RDMA_READ,
      IBV_WR_ATOMIC_CMP_AND_SWP,
      IBV_WR_ATOMIC_FETCH_AND_ADD);
   pragma Convention (C, ibv_wr_opcode);  -- /usr/include/infiniband/verbs.h:617

   subtype ibv_send_flags is unsigned;
   IBV_SEND_FENCE : constant ibv_send_flags := 1;
   IBV_SEND_SIGNALED : constant ibv_send_flags := 2;
   IBV_SEND_SOLICITED : constant ibv_send_flags := 4;
   IBV_SEND_INLINE : constant ibv_send_flags := 8;
   IBV_SEND_IP_CSUM : constant ibv_send_flags := 16;  -- /usr/include/infiniband/verbs.h:627

   type ibv_sge is record
      addr : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:636
      length : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:637
      lkey : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:638
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_sge);  -- /usr/include/infiniband/verbs.h:635

   type ibv_send_wr;
   type anon_27;
   type anon_28 is record
      remote_addr : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:651
      rkey : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:652
   end record;
   pragma Convention (C_Pass_By_Copy, anon_28);
   type anon_29 is record
      remote_addr : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:655
      compare_add : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:656
      swap : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:657
      rkey : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:658
   end record;
   pragma Convention (C_Pass_By_Copy, anon_29);
   type ibv_ah;
   type anon_30 is record
      ah : access ibv_ah;  -- /usr/include/infiniband/verbs.h:661
      remote_qpn : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:662
      remote_qkey : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:663
   end record;
   pragma Convention (C_Pass_By_Copy, anon_30);
   type anon_27 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            rdma : aliased anon_28;  -- /usr/include/infiniband/verbs.h:653
         when 1 =>
            atomic : aliased anon_29;  -- /usr/include/infiniband/verbs.h:659
         when others =>
            ud : aliased anon_30;  -- /usr/include/infiniband/verbs.h:664
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_27);
   pragma Unchecked_Union (anon_27);type anon_31;
   type anon_32 is record
      remote_srqn : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:668
   end record;
   pragma Convention (C_Pass_By_Copy, anon_32);
   type anon_31 (discr : unsigned := 0) is record
      case discr is
         when others =>
            xrc : aliased anon_32;  -- /usr/include/infiniband/verbs.h:669
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_31);
   pragma Unchecked_Union (anon_31);type ibv_send_wr is record
      wr_id : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:642
      next : access ibv_send_wr;  -- /usr/include/infiniband/verbs.h:643
      sg_list : access ibv_sge;  -- /usr/include/infiniband/verbs.h:644
      num_sge : aliased int;  -- /usr/include/infiniband/verbs.h:645
      opcode : aliased ibv_wr_opcode;  -- /usr/include/infiniband/verbs.h:646
      send_flags : aliased int;  -- /usr/include/infiniband/verbs.h:647
      imm_data : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:648
      wr : aliased anon_27;  -- /usr/include/infiniband/verbs.h:665
      qp_type : aliased anon_31;  -- /usr/include/infiniband/verbs.h:670
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_send_wr);  -- /usr/include/infiniband/verbs.h:641

   type ibv_recv_wr is record
      wr_id : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:674
      next : access ibv_recv_wr;  -- /usr/include/infiniband/verbs.h:675
      sg_list : access ibv_sge;  -- /usr/include/infiniband/verbs.h:676
      num_sge : aliased int;  -- /usr/include/infiniband/verbs.h:677
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_recv_wr);  -- /usr/include/infiniband/verbs.h:673

   type ibv_mw_bind is record
      wr_id : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:681
      mr : access ibv_mr;  -- /usr/include/infiniband/verbs.h:682
      addr : System.Address;  -- /usr/include/infiniband/verbs.h:683
      length : aliased stddef_h.size_t;  -- /usr/include/infiniband/verbs.h:684
      send_flags : aliased int;  -- /usr/include/infiniband/verbs.h:685
      mw_access_flags : aliased int;  -- /usr/include/infiniband/verbs.h:686
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_mw_bind);  -- /usr/include/infiniband/verbs.h:680

   type ibv_srq is record
      context : access ibv_context;  -- /usr/include/infiniband/verbs.h:690
      srq_context : System.Address;  -- /usr/include/infiniband/verbs.h:691
      pd : access ibv_pd;  -- /usr/include/infiniband/verbs.h:692
      handle : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:693
      mutex : aliased x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t;  -- /usr/include/infiniband/verbs.h:695
      cond : aliased x86_64_linux_gnu_bits_pthreadtypes_h.pthread_cond_t;  -- /usr/include/infiniband/verbs.h:696
      events_completed : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:697
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_srq);  -- /usr/include/infiniband/verbs.h:689

   type ibv_qp is record
      context : access ibv_context;  -- /usr/include/infiniband/verbs.h:701
      qp_context : System.Address;  -- /usr/include/infiniband/verbs.h:702
      pd : access ibv_pd;  -- /usr/include/infiniband/verbs.h:703
      send_cq : access ibv_cq;  -- /usr/include/infiniband/verbs.h:704
      recv_cq : access ibv_cq;  -- /usr/include/infiniband/verbs.h:705
      srq : access ibv_srq;  -- /usr/include/infiniband/verbs.h:706
      handle : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:707
      qp_num : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:708
      state : aliased ibv_qp_state;  -- /usr/include/infiniband/verbs.h:709
      qp_type : aliased ibv_qp_type;  -- /usr/include/infiniband/verbs.h:710
      mutex : aliased x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t;  -- /usr/include/infiniband/verbs.h:712
      cond : aliased x86_64_linux_gnu_bits_pthreadtypes_h.pthread_cond_t;  -- /usr/include/infiniband/verbs.h:713
      events_completed : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:714
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_qp);  -- /usr/include/infiniband/verbs.h:700

   type ibv_comp_channel is record
      context : access ibv_context;  -- /usr/include/infiniband/verbs.h:718
      fd : aliased int;  -- /usr/include/infiniband/verbs.h:719
      refcnt : aliased int;  -- /usr/include/infiniband/verbs.h:720
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_comp_channel);  -- /usr/include/infiniband/verbs.h:717

   type ibv_cq is record
      context : access ibv_context;  -- /usr/include/infiniband/verbs.h:724
      channel : access ibv_comp_channel;  -- /usr/include/infiniband/verbs.h:725
      cq_context : System.Address;  -- /usr/include/infiniband/verbs.h:726
      handle : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:727
      cqe : aliased int;  -- /usr/include/infiniband/verbs.h:728
      mutex : aliased x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t;  -- /usr/include/infiniband/verbs.h:730
      cond : aliased x86_64_linux_gnu_bits_pthreadtypes_h.pthread_cond_t;  -- /usr/include/infiniband/verbs.h:731
      comp_events_completed : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:732
      async_events_completed : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:733
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_cq);  -- /usr/include/infiniband/verbs.h:723

   type ibv_ah is record
      context : access ibv_context;  -- /usr/include/infiniband/verbs.h:737
      pd : access ibv_pd;  -- /usr/include/infiniband/verbs.h:738
      handle : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:739
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_ah);  -- /usr/include/infiniband/verbs.h:736

   subtype ibv_flow_flags is unsigned;
   IBV_FLOW_ATTR_FLAGS_ALLOW_LOOP_BACK : constant ibv_flow_flags := 1;  -- /usr/include/infiniband/verbs.h:742

   type ibv_flow_attr_type is 
     (IBV_FLOW_ATTR_NORMAL,
      IBV_FLOW_ATTR_ALL_DEFAULT,
      IBV_FLOW_ATTR_MC_DEFAULT);
   pragma Convention (C, ibv_flow_attr_type);  -- /usr/include/infiniband/verbs.h:746

   subtype ibv_flow_spec_type is unsigned;
   IBV_FLOW_SPEC_ETH_CONST : constant ibv_flow_spec_type := 32;
   IBV_FLOW_SPEC_IPV4_CONST : constant ibv_flow_spec_type := 48;
   IBV_FLOW_SPEC_TCP : constant ibv_flow_spec_type := 64;
   IBV_FLOW_SPEC_UDP : constant ibv_flow_spec_type := 65;  -- /usr/include/infiniband/verbs.h:759

   type ibv_flow_eth_filter_dst_mac_array is array (0 .. 5) of aliased stdint_h.uint8_t;
   type ibv_flow_eth_filter_src_mac_array is array (0 .. 5) of aliased stdint_h.uint8_t;
   type ibv_flow_eth_filter is record
      dst_mac : aliased ibv_flow_eth_filter_dst_mac_array;  -- /usr/include/infiniband/verbs.h:767
      src_mac : aliased ibv_flow_eth_filter_src_mac_array;  -- /usr/include/infiniband/verbs.h:768
      ether_type : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:769
      vlan_tag : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:773
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_flow_eth_filter);  -- /usr/include/infiniband/verbs.h:766

   type ibv_flow_spec_eth is record
      c_type : aliased ibv_flow_spec_type;  -- /usr/include/infiniband/verbs.h:777
      size : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:778
      val : aliased ibv_flow_eth_filter;  -- /usr/include/infiniband/verbs.h:779
      mask : aliased ibv_flow_eth_filter;  -- /usr/include/infiniband/verbs.h:780
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_flow_spec_eth);  -- /usr/include/infiniband/verbs.h:776

   type ibv_flow_ipv4_filter is record
      src_ip : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:784
      dst_ip : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:785
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_flow_ipv4_filter);  -- /usr/include/infiniband/verbs.h:783

   type ibv_flow_spec_ipv4 is record
      c_type : aliased ibv_flow_spec_type;  -- /usr/include/infiniband/verbs.h:789
      size : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:790
      val : aliased ibv_flow_ipv4_filter;  -- /usr/include/infiniband/verbs.h:791
      mask : aliased ibv_flow_ipv4_filter;  -- /usr/include/infiniband/verbs.h:792
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_flow_spec_ipv4);  -- /usr/include/infiniband/verbs.h:788

   type ibv_flow_tcp_udp_filter is record
      dst_port : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:796
      src_port : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:797
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_flow_tcp_udp_filter);  -- /usr/include/infiniband/verbs.h:795

   type ibv_flow_spec_tcp_udp is record
      c_type : aliased ibv_flow_spec_type;  -- /usr/include/infiniband/verbs.h:801
      size : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:802
      val : aliased ibv_flow_tcp_udp_filter;  -- /usr/include/infiniband/verbs.h:803
      mask : aliased ibv_flow_tcp_udp_filter;  -- /usr/include/infiniband/verbs.h:804
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_flow_spec_tcp_udp);  -- /usr/include/infiniband/verbs.h:800

   type ibv_flow_spec;
   type anon_33;
   type anon_34 is record
      c_type : aliased ibv_flow_spec_type;  -- /usr/include/infiniband/verbs.h:810
      size : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:811
   end record;
   pragma Convention (C_Pass_By_Copy, anon_34);
   type anon_33 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            hdr : aliased anon_34;  -- /usr/include/infiniband/verbs.h:812
         when 1 =>
            eth : aliased ibv_flow_spec_eth;  -- /usr/include/infiniband/verbs.h:813
         when 2 =>
            ipv4 : aliased ibv_flow_spec_ipv4;  -- /usr/include/infiniband/verbs.h:814
         when others =>
            tcp_udp : aliased ibv_flow_spec_tcp_udp;  -- /usr/include/infiniband/verbs.h:815
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_33);
   pragma Unchecked_Union (anon_33);type ibv_flow_spec is record
      parent : aliased anon_33;
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_flow_spec);  -- /usr/include/infiniband/verbs.h:807

   type ibv_flow_attr is record
      comp_mask : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:820
      c_type : aliased ibv_flow_attr_type;  -- /usr/include/infiniband/verbs.h:821
      size : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:822
      priority : aliased stdint_h.uint16_t;  -- /usr/include/infiniband/verbs.h:823
      num_of_specs : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:824
      port : aliased stdint_h.uint8_t;  -- /usr/include/infiniband/verbs.h:825
      flags : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:826
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_flow_attr);  -- /usr/include/infiniband/verbs.h:819

   type ibv_flow is record
      comp_mask : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:834
      context : access ibv_context;  -- /usr/include/infiniband/verbs.h:835
      handle : aliased stdint_h.uint32_t;  -- /usr/include/infiniband/verbs.h:836
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_flow);  -- /usr/include/infiniband/verbs.h:833

   type ibv_device_ops is record
      alloc_context : access function (arg1 : System.Address; arg2 : int) return access ibv_context;  -- /usr/include/infiniband/verbs.h:843
      free_context : access procedure (arg1 : access ibv_context);  -- /usr/include/infiniband/verbs.h:844
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_device_ops);  -- /usr/include/infiniband/verbs.h:842

   subtype ibv_device_name_array is Interfaces.C.char_array (0 .. 63);
   subtype ibv_device_dev_name_array is Interfaces.C.char_array (0 .. 63);
   subtype ibv_device_dev_path_array is Interfaces.C.char_array (0 .. 255);
   subtype ibv_device_ibdev_path_array is Interfaces.C.char_array (0 .. 255);
   type ibv_device is record
      ops : aliased ibv_device_ops;  -- /usr/include/infiniband/verbs.h:853
      node_type : aliased ibv_node_type;  -- /usr/include/infiniband/verbs.h:854
      transport_type : aliased ibv_transport_type;  -- /usr/include/infiniband/verbs.h:855
      name : aliased ibv_device_name_array;  -- /usr/include/infiniband/verbs.h:857
      dev_name : aliased ibv_device_dev_name_array;  -- /usr/include/infiniband/verbs.h:859
      dev_path : aliased ibv_device_dev_path_array;  -- /usr/include/infiniband/verbs.h:861
      ibdev_path : aliased ibv_device_ibdev_path_array;  -- /usr/include/infiniband/verbs.h:863
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_device);  -- /usr/include/infiniband/verbs.h:852

   type verbs_device is record
      device : aliased ibv_device;  -- /usr/include/infiniband/verbs.h:867
      sz : aliased stddef_h.size_t;  -- /usr/include/infiniband/verbs.h:868
      size_of_context : aliased stddef_h.size_t;  -- /usr/include/infiniband/verbs.h:869
      init_context : access function
           (arg1 : access verbs_device;
            arg2 : access ibv_context;
            arg3 : int) return int;  -- /usr/include/infiniband/verbs.h:871
      uninit_context : access procedure (arg1 : access verbs_device; arg2 : access ibv_context);  -- /usr/include/infiniband/verbs.h:873
   end record;
   pragma Convention (C_Pass_By_Copy, verbs_device);  -- /usr/include/infiniband/verbs.h:866

   type ibv_context_ops is record
      query_device : access function (arg1 : access ibv_context; arg2 : access ibv_device_attr) return int;  -- /usr/include/infiniband/verbs.h:879
      query_port : access function
           (arg1 : access ibv_context;
            arg2 : stdint_h.uint8_t;
            arg3 : access ibv_port_attr) return int;  -- /usr/include/infiniband/verbs.h:881
      alloc_pd : access function (arg1 : access ibv_context) return access ibv_pd;  -- /usr/include/infiniband/verbs.h:882
      dealloc_pd : access function (arg1 : access ibv_pd) return int;  -- /usr/include/infiniband/verbs.h:883
      reg_mr : access function
           (arg1 : access ibv_pd;
            arg2 : System.Address;
            arg3 : stddef_h.size_t;
            arg4 : int) return access ibv_mr;  -- /usr/include/infiniband/verbs.h:885
      rereg_mr : access function
           (arg1 : access ibv_mr;
            arg2 : int;
            arg3 : access ibv_pd;
            arg4 : System.Address;
            arg5 : stddef_h.size_t;
            arg6 : int) return access ibv_mr;  -- /usr/include/infiniband/verbs.h:890
      dereg_mr : access function (arg1 : access ibv_mr) return int;  -- /usr/include/infiniband/verbs.h:891
      alloc_mw : access function (arg1 : access ibv_pd; arg2 : ibv_mw_type) return access ibv_mw;  -- /usr/include/infiniband/verbs.h:892
      bind_mw : access function
           (arg1 : access ibv_qp;
            arg2 : access ibv_mw;
            arg3 : access ibv_mw_bind) return int;  -- /usr/include/infiniband/verbs.h:894
      dealloc_mw : access function (arg1 : access ibv_mw) return int;  -- /usr/include/infiniband/verbs.h:895
      create_cq : access function
           (arg1 : access ibv_context;
            arg2 : int;
            arg3 : access ibv_comp_channel;
            arg4 : int) return access ibv_cq;  -- /usr/include/infiniband/verbs.h:898
      poll_cq : access function
           (arg1 : access ibv_cq;
            arg2 : int;
            arg3 : access ibv_wc) return int;  -- /usr/include/infiniband/verbs.h:899
      req_notify_cq : access function (arg1 : access ibv_cq; arg2 : int) return int;  -- /usr/include/infiniband/verbs.h:900
      cq_event : access procedure (arg1 : access ibv_cq);  -- /usr/include/infiniband/verbs.h:901
      resize_cq : access function (arg1 : access ibv_cq; arg2 : int) return int;  -- /usr/include/infiniband/verbs.h:902
      destroy_cq : access function (arg1 : access ibv_cq) return int;  -- /usr/include/infiniband/verbs.h:903
      create_srq : access function (arg1 : access ibv_pd; arg2 : access ibv_srq_init_attr) return access ibv_srq;  -- /usr/include/infiniband/verbs.h:905
      modify_srq : access function
           (arg1 : access ibv_srq;
            arg2 : access ibv_srq_attr;
            arg3 : int) return int;  -- /usr/include/infiniband/verbs.h:908
      query_srq : access function (arg1 : access ibv_srq; arg2 : access ibv_srq_attr) return int;  -- /usr/include/infiniband/verbs.h:910
      destroy_srq : access function (arg1 : access ibv_srq) return int;  -- /usr/include/infiniband/verbs.h:911
      post_srq_recv : access function
           (arg1 : access ibv_srq;
            arg2 : access ibv_recv_wr;
            arg3 : System.Address) return int;  -- /usr/include/infiniband/verbs.h:914
      create_qp : access function (arg1 : access ibv_pd; arg2 : access ibv_qp_init_attr) return access ibv_qp;  -- /usr/include/infiniband/verbs.h:915
      query_qp : access function
           (arg1 : access ibv_qp;
            arg2 : access ibv_qp_attr;
            arg3 : int;
            arg4 : access ibv_qp_init_attr) return int;  -- /usr/include/infiniband/verbs.h:918
      modify_qp : access function
           (arg1 : access ibv_qp;
            arg2 : access ibv_qp_attr;
            arg3 : int) return int;  -- /usr/include/infiniband/verbs.h:920
      destroy_qp : access function (arg1 : access ibv_qp) return int;  -- /usr/include/infiniband/verbs.h:921
      post_send : access function
           (arg1 : access ibv_qp;
            arg2 : access ibv_send_wr;
            arg3 : System.Address) return int;  -- /usr/include/infiniband/verbs.h:923
      post_recv : access function
           (arg1 : access ibv_qp;
            arg2 : access ibv_recv_wr;
            arg3 : System.Address) return int;  -- /usr/include/infiniband/verbs.h:925
      create_ah : access function (arg1 : access ibv_pd; arg2 : access ibv_ah_attr) return access ibv_ah;  -- /usr/include/infiniband/verbs.h:926
      destroy_ah : access function (arg1 : access ibv_ah) return int;  -- /usr/include/infiniband/verbs.h:927
      attach_mcast : access function
           (arg1 : access ibv_qp;
            arg2 : access constant ibv_gid;
            arg3 : stdint_h.uint16_t) return int;  -- /usr/include/infiniband/verbs.h:929
      detach_mcast : access function
           (arg1 : access ibv_qp;
            arg2 : access constant ibv_gid;
            arg3 : stdint_h.uint16_t) return int;  -- /usr/include/infiniband/verbs.h:931
      async_event : access procedure (arg1 : access ibv_async_event);  -- /usr/include/infiniband/verbs.h:932
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_context_ops);  -- /usr/include/infiniband/verbs.h:877

   type ibv_context is record
      device : access ibv_device;  -- /usr/include/infiniband/verbs.h:936
      ops : aliased ibv_context_ops;  -- /usr/include/infiniband/verbs.h:937
      cmd_fd : aliased int;  -- /usr/include/infiniband/verbs.h:938
      async_fd : aliased int;  -- /usr/include/infiniband/verbs.h:939
      num_comp_vectors : aliased int;  -- /usr/include/infiniband/verbs.h:940
      mutex : aliased x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t;  -- /usr/include/infiniband/verbs.h:941
      abi_compat : System.Address;  -- /usr/include/infiniband/verbs.h:942
   end record;
   pragma Convention (C_Pass_By_Copy, ibv_context);  -- /usr/include/infiniband/verbs.h:935

   subtype verbs_context_mask is unsigned;
   VERBS_CONTEXT_XRCD : constant verbs_context_mask := 1;
   VERBS_CONTEXT_SRQ : constant verbs_context_mask := 2;
   VERBS_CONTEXT_QP : constant verbs_context_mask := 4;
   VERBS_CONTEXT_CREATE_FLOW : constant verbs_context_mask := 8;
   VERBS_CONTEXT_DESTROY_FLOW : constant verbs_context_mask := 16;
   VERBS_CONTEXT_RESERVED : constant verbs_context_mask := 32;  -- /usr/include/infiniband/verbs.h:945

   type verbs_context is record
      drv_ibv_destroy_flow : access function (arg1 : access ibv_flow) return int;  -- /usr/include/infiniband/verbs.h:956
      lib_ibv_destroy_flow : access function (arg1 : access ibv_flow) return int;  -- /usr/include/infiniband/verbs.h:957
      drv_ibv_create_flow : access function (arg1 : access ibv_qp; arg2 : access ibv_flow_attr) return access ibv_flow;  -- /usr/include/infiniband/verbs.h:960
      lib_ibv_create_flow : access function (arg1 : access ibv_qp; arg2 : access ibv_flow_attr) return access ibv_flow;  -- /usr/include/infiniband/verbs.h:963
      open_qp : access function (arg1 : access ibv_context; arg2 : access ibv_qp_open_attr) return access ibv_qp;  -- /usr/include/infiniband/verbs.h:965
      create_qp_ex : access function (arg1 : access ibv_context; arg2 : access ibv_qp_init_attr_ex) return access ibv_qp;  -- /usr/include/infiniband/verbs.h:967
      get_srq_num : access function (arg1 : access ibv_srq; arg2 : access stdint_h.uint32_t) return int;  -- /usr/include/infiniband/verbs.h:968
      create_srq_ex : access function (arg1 : access ibv_context; arg2 : access ibv_srq_init_attr_ex) return access ibv_srq;  -- /usr/include/infiniband/verbs.h:970
      open_xrcd : access function (arg1 : access ibv_context; arg2 : access ibv_xrcd_init_attr) return access ibv_xrcd;  -- /usr/include/infiniband/verbs.h:972
      close_xrcd : access function (arg1 : access ibv_xrcd) return int;  -- /usr/include/infiniband/verbs.h:973
      has_comp_mask : aliased stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:974
      sz : aliased stddef_h.size_t;  -- /usr/include/infiniband/verbs.h:975
      context : aliased ibv_context;  -- /usr/include/infiniband/verbs.h:976
   end record;
   pragma Convention (C_Pass_By_Copy, verbs_context);  -- /usr/include/infiniband/verbs.h:954

   function verbs_get_ctx (ctx : access ibv_context) return access verbs_context;  -- /usr/include/infiniband/verbs.h:979
   pragma Import (C, verbs_get_ctx, "verbs_get_ctx");

   function verbs_get_device (dev : access constant ibv_device) return access verbs_device;  -- /usr/include/infiniband/verbs.h:995
   pragma Import (C, verbs_get_device, "verbs_get_device");

   function ibv_get_device_list (num_devices : access int) return System.Address;  -- /usr/include/infiniband/verbs.h:1010
   pragma Import (C, ibv_get_device_list, "ibv_get_device_list");

   procedure ibv_free_device_list (list : System.Address);  -- /usr/include/infiniband/verbs.h:1020
   pragma Import (C, ibv_free_device_list, "ibv_free_device_list");

   function ibv_get_device_name (device : access ibv_device) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/infiniband/verbs.h:1025
   pragma Import (C, ibv_get_device_name, "ibv_get_device_name");

   function ibv_get_device_guid (device : access ibv_device) return stdint_h.uint64_t;  -- /usr/include/infiniband/verbs.h:1030
   pragma Import (C, ibv_get_device_guid, "ibv_get_device_guid");

   function ibv_open_device (device : access ibv_device) return access ibv_context;  -- /usr/include/infiniband/verbs.h:1035
   pragma Import (C, ibv_open_device, "ibv_open_device");

   function ibv_close_device (context : access ibv_context) return int;  -- /usr/include/infiniband/verbs.h:1040
   pragma Import (C, ibv_close_device, "ibv_close_device");

   function ibv_get_async_event (context : access ibv_context; event : access ibv_async_event) return int;  -- /usr/include/infiniband/verbs.h:1049
   pragma Import (C, ibv_get_async_event, "ibv_get_async_event");

   procedure ibv_ack_async_event (event : access ibv_async_event);  -- /usr/include/infiniband/verbs.h:1062
   pragma Import (C, ibv_ack_async_event, "ibv_ack_async_event");

   function ibv_query_device (context : access ibv_context; device_attr : access ibv_device_attr) return int;  -- /usr/include/infiniband/verbs.h:1067
   pragma Import (C, ibv_query_device, "ibv_query_device");

   function ibv_query_port
     (context : access ibv_context;
      port_num : stdint_h.uint8_t;
      port_attr : access ibv_port_attr) return int;  -- /usr/include/infiniband/verbs.h:1073
   pragma Import (C, ibv_query_port, "ibv_query_port");

   --  skipped func ___ibv_query_port

   function ibv_query_gid
     (context : access ibv_context;
      port_num : stdint_h.uint8_t;
      index : int;
      gid : access ibv_gid) return int;  -- /usr/include/infiniband/verbs.h:1093
   pragma Import (C, ibv_query_gid, "ibv_query_gid");

   function ibv_query_pkey
     (context : access ibv_context;
      port_num : stdint_h.uint8_t;
      index : int;
      pkey : access stdint_h.uint16_t) return int;  -- /usr/include/infiniband/verbs.h:1099
   pragma Import (C, ibv_query_pkey, "ibv_query_pkey");

   function ibv_alloc_pd (context : access ibv_context) return access ibv_pd;  -- /usr/include/infiniband/verbs.h:1105
   pragma Import (C, ibv_alloc_pd, "ibv_alloc_pd");

   function ibv_dealloc_pd (pd : access ibv_pd) return int;  -- /usr/include/infiniband/verbs.h:1110
   pragma Import (C, ibv_dealloc_pd, "ibv_dealloc_pd");

   function ibv_create_flow (qp : access ibv_qp; flow : access ibv_flow_attr) return access ibv_flow;  -- /usr/include/infiniband/verbs.h:1112
   pragma Import (C, ibv_create_flow, "ibv_create_flow");

   function ibv_destroy_flow (flow_id : access ibv_flow) return int;  -- /usr/include/infiniband/verbs.h:1123
   pragma Import (C, ibv_destroy_flow, "ibv_destroy_flow");

   function ibv_open_xrcd (context : access ibv_context; xrcd_init_attr : access ibv_xrcd_init_attr) return access ibv_xrcd;  -- /usr/include/infiniband/verbs.h:1136
   pragma Import (C, ibv_open_xrcd, "ibv_open_xrcd");

   function ibv_close_xrcd (xrcd : access ibv_xrcd) return int;  -- /usr/include/infiniband/verbs.h:1149
   pragma Import (C, ibv_close_xrcd, "ibv_close_xrcd");

   function ibv_reg_mr
     (pd : access ibv_pd;
      addr : System.Address;
      length : stddef_h.size_t;
      c_access : int) return access ibv_mr;  -- /usr/include/infiniband/verbs.h:1158
   pragma Import (C, ibv_reg_mr, "ibv_reg_mr");

   function ibv_dereg_mr (mr : access ibv_mr) return int;  -- /usr/include/infiniband/verbs.h:1164
   pragma Import (C, ibv_dereg_mr, "ibv_dereg_mr");

   function ibv_create_comp_channel (context : access ibv_context) return access ibv_comp_channel;  -- /usr/include/infiniband/verbs.h:1169
   pragma Import (C, ibv_create_comp_channel, "ibv_create_comp_channel");

   function ibv_destroy_comp_channel (channel : access ibv_comp_channel) return int;  -- /usr/include/infiniband/verbs.h:1174
   pragma Import (C, ibv_destroy_comp_channel, "ibv_destroy_comp_channel");

   function ibv_create_cq
     (context : access ibv_context;
      cqe : int;
      cq_context : System.Address;
      channel : access ibv_comp_channel;
      comp_vector : int) return access ibv_cq;  -- /usr/include/infiniband/verbs.h:1186
   pragma Import (C, ibv_create_cq, "ibv_create_cq");

   function ibv_resize_cq (cq : access ibv_cq; cqe : int) return int;  -- /usr/include/infiniband/verbs.h:1198
   pragma Import (C, ibv_resize_cq, "ibv_resize_cq");

   function ibv_destroy_cq (cq : access ibv_cq) return int;  -- /usr/include/infiniband/verbs.h:1203
   pragma Import (C, ibv_destroy_cq, "ibv_destroy_cq");

   function ibv_get_cq_event
     (channel : access ibv_comp_channel;
      cq : System.Address;
      cq_context : System.Address) return int;  -- /usr/include/infiniband/verbs.h:1214
   pragma Import (C, ibv_get_cq_event, "ibv_get_cq_event");

   procedure ibv_ack_cq_events (cq : access ibv_cq; nevents : unsigned);  -- /usr/include/infiniband/verbs.h:1230
   pragma Import (C, ibv_ack_cq_events, "ibv_ack_cq_events");

   function ibv_poll_cq
     (cq : access ibv_cq;
      num_entries : int;
      wc : access ibv_wc) return int;  -- /usr/include/infiniband/verbs.h:1245
   pragma Import (C, ibv_poll_cq, "ibv_poll_cq");

   function ibv_req_notify_cq (cq : access ibv_cq; solicited_only : int) return int;  -- /usr/include/infiniband/verbs.h:1259
   pragma Import (C, ibv_req_notify_cq, "ibv_req_notify_cq");

   function ibv_create_srq (pd : access ibv_pd; srq_init_attr : access ibv_srq_init_attr) return access ibv_srq;  -- /usr/include/infiniband/verbs.h:1275
   pragma Import (C, ibv_create_srq, "ibv_create_srq");

   function ibv_create_srq_ex (context : access ibv_context; srq_init_attr_ex : access ibv_srq_init_attr_ex) return access ibv_srq;  -- /usr/include/infiniband/verbs.h:1279
   pragma Import (C, ibv_create_srq_ex, "ibv_create_srq_ex");

   function ibv_modify_srq
     (srq : access ibv_srq;
      srq_attr : access ibv_srq_attr;
      srq_attr_mask : int) return int;  -- /usr/include/infiniband/verbs.h:1312
   pragma Import (C, ibv_modify_srq, "ibv_modify_srq");

   function ibv_query_srq (srq : access ibv_srq; srq_attr : access ibv_srq_attr) return int;  -- /usr/include/infiniband/verbs.h:1322
   pragma Import (C, ibv_query_srq, "ibv_query_srq");

   function ibv_get_srq_num (srq : access ibv_srq; srq_num : access stdint_h.uint32_t) return int;  -- /usr/include/infiniband/verbs.h:1324
   pragma Import (C, ibv_get_srq_num, "ibv_get_srq_num");

   function ibv_destroy_srq (srq : access ibv_srq) return int;  -- /usr/include/infiniband/verbs.h:1338
   pragma Import (C, ibv_destroy_srq, "ibv_destroy_srq");

   function ibv_post_srq_recv
     (srq : access ibv_srq;
      recv_wr : access ibv_recv_wr;
      bad_recv_wr : System.Address) return int;  -- /usr/include/infiniband/verbs.h:1347
   pragma Import (C, ibv_post_srq_recv, "ibv_post_srq_recv");

   function ibv_create_qp (pd : access ibv_pd; qp_init_attr : access ibv_qp_init_attr) return access ibv_qp;  -- /usr/include/infiniband/verbs.h:1357
   pragma Import (C, ibv_create_qp, "ibv_create_qp");

   function ibv_create_qp_ex (context : access ibv_context; qp_init_attr_ex : access ibv_qp_init_attr_ex) return access ibv_qp;  -- /usr/include/infiniband/verbs.h:1361
   pragma Import (C, ibv_create_qp_ex, "ibv_create_qp_ex");

   function ibv_open_qp (context : access ibv_context; qp_open_attr : access ibv_qp_open_attr) return access ibv_qp;  -- /usr/include/infiniband/verbs.h:1382
   pragma Import (C, ibv_open_qp, "ibv_open_qp");

   function ibv_modify_qp
     (qp : access ibv_qp;
      attr : access ibv_qp_attr;
      attr_mask : int) return int;  -- /usr/include/infiniband/verbs.h:1395
   pragma Import (C, ibv_modify_qp, "ibv_modify_qp");

   function ibv_query_qp
     (qp : access ibv_qp;
      attr : access ibv_qp_attr;
      attr_mask : int;
      init_attr : access ibv_qp_init_attr) return int;  -- /usr/include/infiniband/verbs.h:1409
   pragma Import (C, ibv_query_qp, "ibv_query_qp");

   function ibv_destroy_qp (qp : access ibv_qp) return int;  -- /usr/include/infiniband/verbs.h:1416
   pragma Import (C, ibv_destroy_qp, "ibv_destroy_qp");

   function ibv_post_send
     (qp : access ibv_qp;
      wr : access ibv_send_wr;
      bad_wr : System.Address) return int;  -- /usr/include/infiniband/verbs.h:1424
   pragma Import (C, ibv_post_send, "ibv_post_send");

   function ibv_post_recv
     (qp : access ibv_qp;
      wr : access ibv_recv_wr;
      bad_wr : System.Address) return int;  -- /usr/include/infiniband/verbs.h:1433
   pragma Import (C, ibv_post_recv, "ibv_post_recv");

   function ibv_create_ah (pd : access ibv_pd; attr : access ibv_ah_attr) return access ibv_ah;  -- /usr/include/infiniband/verbs.h:1442
   pragma Import (C, ibv_create_ah, "ibv_create_ah");

   function ibv_init_ah_from_wc
     (context : access ibv_context;
      port_num : stdint_h.uint8_t;
      wc : access ibv_wc;
      grh : access ibv_grh;
      ah_attr : access ibv_ah_attr) return int;  -- /usr/include/infiniband/verbs.h:1455
   pragma Import (C, ibv_init_ah_from_wc, "ibv_init_ah_from_wc");

   function ibv_create_ah_from_wc
     (pd : access ibv_pd;
      wc : access ibv_wc;
      grh : access ibv_grh;
      port_num : stdint_h.uint8_t) return access ibv_ah;  -- /usr/include/infiniband/verbs.h:1471
   pragma Import (C, ibv_create_ah_from_wc, "ibv_create_ah_from_wc");

   function ibv_destroy_ah (ah : access ibv_ah) return int;  -- /usr/include/infiniband/verbs.h:1477
   pragma Import (C, ibv_destroy_ah, "ibv_destroy_ah");

   function ibv_attach_mcast
     (qp : access ibv_qp;
      gid : access constant ibv_gid;
      lid : stdint_h.uint16_t) return int;  -- /usr/include/infiniband/verbs.h:1490
   pragma Import (C, ibv_attach_mcast, "ibv_attach_mcast");

   function ibv_detach_mcast
     (qp : access ibv_qp;
      gid : access constant ibv_gid;
      lid : stdint_h.uint16_t) return int;  -- /usr/include/infiniband/verbs.h:1498
   pragma Import (C, ibv_detach_mcast, "ibv_detach_mcast");

   function ibv_fork_init return int;  -- /usr/include/infiniband/verbs.h:1506
   pragma Import (C, ibv_fork_init, "ibv_fork_init");

   function ibv_node_type_str (node_type : ibv_node_type) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/infiniband/verbs.h:1511
   pragma Import (C, ibv_node_type_str, "ibv_node_type_str");

   function ibv_port_state_str (port_state : ibv_port_state) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/infiniband/verbs.h:1516
   pragma Import (C, ibv_port_state_str, "ibv_port_state_str");

   function ibv_event_type_str (event : ibv_event_type) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/infiniband/verbs.h:1521
   pragma Import (C, ibv_event_type_str, "ibv_event_type_str");

end infiniband_verbs_h;
