
enum {

  /* compiled object types: (internal) */
  scheme_variable_type,
  scheme_local_type, 
  scheme_local_unbox_type,
  scheme_syntax_type,
  scheme_application_type,
  scheme_sequence_type,
  scheme_branch_type,
  scheme_unclosed_procedure_type,
  scheme_let_value_type,
  scheme_let_void_type,
  scheme_letrec_type, /* 10 */
  scheme_let_one_type,
  scheme_with_cont_mark_type,
  scheme_module_variable_type, /* link changes to scheme_variable_type */

  _scheme_values_types_, /* All following types are values */
  
  /* intermediate compiled: */
  scheme_compiled_unclosed_procedure_type,
  scheme_compiled_let_value_type,
  scheme_compiled_let_void_type,
  scheme_compiled_syntax_type,

  scheme_quote_compilation_type, /* used while writing, only */

  _scheme_compiled_values_types_, /* 20 */

  /* procedure types */
  scheme_prim_type,
  scheme_closed_prim_type,
  scheme_linked_closure_type,
  scheme_case_closure_type,
  scheme_cont_type,
  scheme_escaping_cont_type,

  /* basic types */
  scheme_char_type, /* 27 */
  scheme_integer_type,
  scheme_bignum_type,
  scheme_rational_type, /* 30 */
  scheme_float_type,
  scheme_double_type,
  scheme_complex_izi_type,
  scheme_complex_type,
  scheme_string_type,
  scheme_symbol_type,
  scheme_null_type,
  scheme_pair_type,
  scheme_vector_type,
  scheme_inspector_type,   /* 40 */
  scheme_input_port_type,
  scheme_output_port_type,
  scheme_eof_type,
  scheme_true_type,
  scheme_false_type, 
  scheme_void_type,
  scheme_syntax_compiler_type,
  scheme_macro_type,
  scheme_box_type,
  scheme_thread_type,     /* 50 */
  scheme_structure_type,
  scheme_generic_type, 
  scheme_cont_mark_set_type, 
  scheme_sema_type,
  scheme_hash_table_type,
  scheme_generic_data_type,
  scheme_weak_box_type,
  scheme_struct_type_type,
  scheme_module_index_type,
  scheme_id_macro_type,   /* 60 */
  scheme_listener_type,
  scheme_namespace_type, 
  scheme_config_type,
  scheme_stx_type, 
  scheme_will_executor_type,
  scheme_custodian_type,
  scheme_random_state_type,
  scheme_regexp_type,

  /* These reserved types will let us add types
     without forcing recompilation of compiled MzScheme code */
  scheme_reserved_1_type,
  scheme_reserved_2_type,  /* 70 */
  scheme_reserved_3_type,

  /* more internal types: */
  scheme_compilation_top_type,
  scheme_envunbox_type,
  scheme_eval_waiting_type,
  scheme_tail_call_waiting_type,
  scheme_undefined_type,
  scheme_struct_property_type,
  scheme_multiple_values_type,
  scheme_placeholder_type,
  scheme_case_lambda_sequence_type,   /* 80 */
  scheme_begin0_sequence_type,
  scheme_rename_table_type,
  scheme_module_type,
  scheme_svector_type,

#ifdef MZTAG_REQUIRED
  _scheme_last_normal_type_,

  scheme_rt_comp_env,
  scheme_rt_constant_binding,
  scheme_rt_resolve_info,
  scheme_rt_compile_info,
  scheme_rt_cont_mark, /* 90 */
  scheme_rt_saved_stack,
  scheme_rt_eval_in_env,
  scheme_rt_reply_item,
  scheme_rt_closure_info,
  scheme_rt_overflow,
  scheme_rt_dyn_wind_cell,
  scheme_rt_cont_mark_chain,
  scheme_rt_dyn_wind_info,
  scheme_rt_dyn_wind,
  scheme_rt_dup_check, /* 100 */
  scheme_rt_thread_memory,
  scheme_rt_input_file,
  scheme_rt_input_fd,
  scheme_rt_oskit_console_input,
  scheme_rt_tested_input_file,
  scheme_rt_tested_output_file,
  scheme_rt_indexed_string,
  scheme_rt_output_file,
  scheme_rt_load_handler_data,
  scheme_rt_load_data, /* 110 */
  scheme_rt_pipe,
  scheme_rt_beos_process,
  scheme_rt_system_child,
  scheme_rt_tcp,
  scheme_rt_write_data,
  scheme_rt_tcp_select_info,
  scheme_rt_namespace_option,
  scheme_rt_param_data,
  scheme_rt_will,
  scheme_rt_will_registration, /* 120 */
  scheme_rt_breakable_wait,
  scheme_rt_sema_waiter,
  scheme_rt_struct_proc_info,
  scheme_rt_linker_name,
  scheme_rt_param_map,
  scheme_rt_finalization,
  scheme_rt_finalizations,
  scheme_rt_cpp_object,
  scheme_rt_cpp_array_object,
  scheme_rt_stack_object, /* 130 */
  scheme_rt_preallocated_object,
  scheme_thread_hop_type,
  scheme_rt_breakable,
#endif

  _scheme_last_type_
};
