
enum {

  /* compiled object types: (internal) */
  scheme_toplevel_type,
  scheme_local_type, 
  scheme_local_unbox_type,
  scheme_syntax_type,
  scheme_application_type,
  scheme_application2_type,
  scheme_application3_type,
  scheme_sequence_type,
  scheme_branch_type,
  scheme_unclosed_procedure_type,
  scheme_let_value_type, /* 10 */
  scheme_let_void_type,
  scheme_letrec_type,
  scheme_let_one_type,
  scheme_with_cont_mark_type,

  _scheme_values_types_, /* All following types are values */
  
  /* intermediate compiled: */
  scheme_compiled_unclosed_procedure_type,
  scheme_compiled_let_value_type,
  scheme_compiled_let_void_type,
  scheme_compiled_syntax_type,
  scheme_compiled_toplevel_type,  /* 20 */
  scheme_compiled_quote_syntax_type,

  scheme_quote_compilation_type, /* used while writing, only */

  /* Registered in prefix table: */
  scheme_variable_type,
  scheme_module_variable_type, /* link replaces with scheme_variable_type */

  _scheme_compiled_values_types_,

  /* procedure types */
  scheme_prim_type,
  scheme_closed_prim_type,
  scheme_closure_type,
  scheme_case_closure_type,
  scheme_cont_type, /* 30 */
  scheme_escaping_cont_type,
  scheme_proc_struct_type,

  /* structure types (overlaps with procs) */
  scheme_structure_type,

  /* basic types */
  scheme_char_type,
  scheme_integer_type,
  scheme_bignum_type,
  scheme_rational_type,
  scheme_float_type,
  scheme_double_type,
  scheme_complex_izi_type, /* 40 */
  scheme_complex_type,
  scheme_string_type,
  scheme_symbol_type,
  scheme_null_type,
  scheme_pair_type,
  scheme_vector_type,
  scheme_inspector_type,
  scheme_input_port_type,
  scheme_output_port_type,
  scheme_eof_type, /* 50 */
  scheme_true_type,
  scheme_false_type,
  scheme_void_type,
  scheme_syntax_compiler_type,
  scheme_macro_type,
  scheme_box_type,
  scheme_thread_type,
  scheme_stx_offset_type, 
  scheme_cont_mark_set_type, 
  scheme_sema_type, /* 60 */
  scheme_hash_table_type,
  scheme_c_pointer_type,
  scheme_weak_box_type,
  scheme_struct_type_type,
  scheme_module_index_type,
  scheme_set_macro_type,
  scheme_listener_type,
  scheme_namespace_type, 
  scheme_config_type,
  scheme_stx_type,  /* 70 */
  scheme_will_executor_type,
  scheme_custodian_type,
  scheme_random_state_type,
  scheme_regexp_type,
  scheme_bucket_type,
  scheme_bucket_table_type,
  scheme_subprocess_type,
  scheme_compilation_top_type,
  scheme_wrap_chunk_type,
  scheme_eval_waiting_type, /* 80 */
  scheme_tail_call_waiting_type,
  scheme_undefined_type,
  scheme_struct_property_type,
  scheme_multiple_values_type,
  scheme_placeholder_type,
  scheme_case_lambda_sequence_type,
  scheme_begin0_sequence_type,
  scheme_rename_table_type,
  scheme_module_type,
  scheme_svector_type, /* 90 */
  scheme_lazy_macro_type,
  scheme_resolve_prefix_type,
  scheme_security_guard_type,
  scheme_indent_type,
  scheme_udp_type,
  scheme_udp_waitable_type,
  scheme_id_macro_type,  
  scheme_waitable_set_type,
  scheme_wrapped_waitable_type,
  scheme_nack_guard_waitable_type, /* 100 */
  scheme_semaphore_repost_type,
  scheme_channel_type,
  scheme_channel_put_type,
  scheme_thread_resume_type,
  scheme_thread_suspend_type,
  scheme_thread_dead_type,
  scheme_poll_waitable_type,
  scheme_nack_waitable_type,
  scheme_module_registry_type,
  scheme_thread_set_type,   /* 110 */

  scheme_reserved1_type,
  scheme_reserved2_type,
  scheme_reserved3_type,

#ifdef MZTAG_REQUIRED
  _scheme_last_normal_type_,

  scheme_rt_comp_env,       
  scheme_rt_constant_binding,
  scheme_rt_resolve_info,
  scheme_rt_compile_info,
  scheme_rt_cont_mark,
  scheme_rt_saved_stack,  /* 120 */
  scheme_rt_eval_in_env,
  scheme_rt_reply_item,
  scheme_rt_closure_info,
  scheme_rt_overflow,
  scheme_rt_dyn_wind_cell,
  scheme_rt_cont_mark_chain,
  scheme_rt_dyn_wind_info,
  scheme_rt_dyn_wind,
  scheme_rt_dup_check,
  scheme_rt_thread_memory,  /* 130 */
  scheme_rt_input_file,
  scheme_rt_input_fd,
  scheme_rt_oskit_console_input,
  scheme_rt_tested_input_file,
  scheme_rt_tested_output_file,
  scheme_rt_indexed_string,
  scheme_rt_output_file,
  scheme_rt_load_handler_data,
  scheme_rt_load_data,
  scheme_rt_pipe,        /* 140 */
  scheme_rt_beos_process,
  scheme_rt_system_child,
  scheme_rt_tcp,
  scheme_rt_write_data,
  scheme_rt_tcp_select_info,
  scheme_rt_namespace_option,
  scheme_rt_param_data, 
  scheme_rt_will,
  scheme_rt_will_registration,
  scheme_rt_breakable_wait, /* 150 */
  scheme_rt_sema_waiter,
  scheme_rt_struct_proc_info,
  scheme_rt_linker_name,
  scheme_rt_param_map,
  scheme_rt_finalization,
  scheme_rt_finalizations,
  scheme_rt_cpp_object,
  scheme_rt_cpp_array_object,
  scheme_rt_stack_object, 
  scheme_rt_preallocated_object,  /* 160 */
  scheme_thread_hop_type,
  scheme_rt_breakable,
  scheme_rt_srcloc,
  scheme_rt_waitable,
  scheme_rt_waiting,
  scheme_rt_comp_prefix,
  scheme_rt_user_input,
  scheme_rt_user_output,
  scheme_rt_compact_port,
  scheme_rt_read_special_dw,  /* 170 */
  scheme_rt_regwork,
  scheme_rt_buf_holder,
#endif

  _scheme_last_type_
};
