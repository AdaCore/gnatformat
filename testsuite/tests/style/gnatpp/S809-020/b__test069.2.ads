pragma Ada_95;
pragma Warnings (Off);
with System;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: Pro 17.0w (20160318-49)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_test069" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#f2b138d6#;
   pragma Export (C, u00001, "test069B");
   u00002 : constant Version_32 := 16#93738ab9#;
   pragma Export (C, u00002, "test069S");
   u00003 : constant Version_32 := 16#b6df930e#;
   pragma Export (C, u00003, "system__standard_libraryB");
   u00004 : constant Version_32 := 16#ddf3267e#;
   pragma Export (C, u00004, "system__standard_libraryS");
   u00005 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00005, "adaS");
   u00006 : constant Version_32 := 16#4c140a9c#;
   pragma Export (C, u00006, "ada__text_ioB");
   u00007 : constant Version_32 := 16#c3f01c15#;
   pragma Export (C, u00007, "ada__text_ioS");
   u00008 : constant Version_32 := 16#da4d2671#;
   pragma Export (C, u00008, "ada__exceptionsB");
   u00009 : constant Version_32 := 16#4c8cceba#;
   pragma Export (C, u00009, "ada__exceptionsS");
   u00010 : constant Version_32 := 16#e947e6a9#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerB");
   u00011 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerS");
   u00012 : constant Version_32 := 16#2da59038#;
   pragma Export (C, u00012, "systemS");
   u00013 : constant Version_32 := 16#5f84b5ab#;
   pragma Export (C, u00013, "system__soft_linksB");
   u00014 : constant Version_32 := 16#b321486d#;
   pragma Export (C, u00014, "system__soft_linksS");
   u00015 : constant Version_32 := 16#b01dad17#;
   pragma Export (C, u00015, "system__parametersB");
   u00016 : constant Version_32 := 16#538f9d47#;
   pragma Export (C, u00016, "system__parametersS");
   u00017 : constant Version_32 := 16#0f0cb66d#;
   pragma Export (C, u00017, "system__secondary_stackB");
   u00018 : constant Version_32 := 16#86c45f51#;
   pragma Export (C, u00018, "system__secondary_stackS");
   u00019 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00019, "system__storage_elementsB");
   u00020 : constant Version_32 := 16#0066da3c#;
   pragma Export (C, u00020, "system__storage_elementsS");
   u00021 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00021, "system__stack_checkingB");
   u00022 : constant Version_32 := 16#a31afbd0#;
   pragma Export (C, u00022, "system__stack_checkingS");
   u00023 : constant Version_32 := 16#87a448ff#;
   pragma Export (C, u00023, "system__exception_tableB");
   u00024 : constant Version_32 := 16#700bf97a#;
   pragma Export (C, u00024, "system__exception_tableS");
   u00025 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00025, "system__exceptionsB");
   u00026 : constant Version_32 := 16#45c6fdce#;
   pragma Export (C, u00026, "system__exceptionsS");
   u00027 : constant Version_32 := 16#4c9e814d#;
   pragma Export (C, u00027, "system__exceptions__machineS");
   u00028 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00028, "system__exceptions_debugB");
   u00029 : constant Version_32 := 16#532f69fc#;
   pragma Export (C, u00029, "system__exceptions_debugS");
   u00030 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00030, "system__img_intB");
   u00031 : constant Version_32 := 16#2f7e70fa#;
   pragma Export (C, u00031, "system__img_intS");
   u00032 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00032, "system__tracebackB");
   u00033 : constant Version_32 := 16#73874efc#;
   pragma Export (C, u00033, "system__tracebackS");
   u00034 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00034, "system__traceback_entriesB");
   u00035 : constant Version_32 := 16#2dfe6648#;
   pragma Export (C, u00035, "system__traceback_entriesS");
   u00036 : constant Version_32 := 16#d2b6296c#;
   pragma Export (C, u00036, "system__traceback__symbolicB");
   u00037 : constant Version_32 := 16#dd19f67a#;
   pragma Export (C, u00037, "system__traceback__symbolicS");
   u00038 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00038, "ada__exceptions__tracebackB");
   u00039 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00039, "ada__exceptions__tracebackS");
   u00040 : constant Version_32 := 16#5d344636#;
   pragma Export (C, u00040, "system__crtlS");
   u00041 : constant Version_32 := 16#b6a35849#;
   pragma Export (C, u00041, "system__dwarf_linesB");
   u00042 : constant Version_32 := 16#44249c75#;
   pragma Export (C, u00042, "system__dwarf_linesS");
   u00043 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00043, "ada__charactersS");
   u00044 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00044, "ada__characters__handlingB");
   u00045 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00045, "ada__characters__handlingS");
   u00046 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00046, "ada__characters__latin_1S");
   u00047 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00047, "ada__stringsS");
   u00048 : constant Version_32 := 16#e2ea8656#;
   pragma Export (C, u00048, "ada__strings__mapsB");
   u00049 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00049, "ada__strings__mapsS");
   u00050 : constant Version_32 := 16#4a6f6ca4#;
   pragma Export (C, u00050, "system__bit_opsB");
   u00051 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00051, "system__bit_opsS");
   u00052 : constant Version_32 := 16#1923ecbb#;
   pragma Export (C, u00052, "system__unsigned_typesS");
   u00053 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00053, "ada__strings__maps__constantsS");
   u00054 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00054, "interfacesS");
   u00055 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00055, "system__address_imageB");
   u00056 : constant Version_32 := 16#8c490d02#;
   pragma Export (C, u00056, "system__address_imageS");
   u00057 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00057, "system__img_unsB");
   u00058 : constant Version_32 := 16#86d7d04c#;
   pragma Export (C, u00058, "system__img_unsS");
   u00059 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00059, "system__ioB");
   u00060 : constant Version_32 := 16#b3e76777#;
   pragma Export (C, u00060, "system__ioS");
   u00061 : constant Version_32 := 16#cf909744#;
   pragma Export (C, u00061, "system__object_readerB");
   u00062 : constant Version_32 := 16#6942daaf#;
   pragma Export (C, u00062, "system__object_readerS");
   u00063 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00063, "interfaces__cB");
   u00064 : constant Version_32 := 16#70be4e8c#;
   pragma Export (C, u00064, "interfaces__cS");
   u00065 : constant Version_32 := 16#1a74a354#;
   pragma Export (C, u00065, "system__val_lliB");
   u00066 : constant Version_32 := 16#b7817698#;
   pragma Export (C, u00066, "system__val_lliS");
   u00067 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00067, "system__val_lluB");
   u00068 : constant Version_32 := 16#63d1bbc9#;
   pragma Export (C, u00068, "system__val_lluS");
   u00069 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00069, "system__val_utilB");
   u00070 : constant Version_32 := 16#810526c6#;
   pragma Export (C, u00070, "system__val_utilS");
   u00071 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00071, "system__case_utilB");
   u00072 : constant Version_32 := 16#09acf9ef#;
   pragma Export (C, u00072, "system__case_utilS");
   u00073 : constant Version_32 := 16#84a27f0d#;
   pragma Export (C, u00073, "interfaces__c_streamsB");
   u00074 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00074, "interfaces__c_streamsS");
   u00075 : constant Version_32 := 16#931ff6be#;
   pragma Export (C, u00075, "system__exception_tracesB");
   u00076 : constant Version_32 := 16#097ab0a2#;
   pragma Export (C, u00076, "system__exception_tracesS");
   u00077 : constant Version_32 := 16#ce7de326#;
   pragma Export (C, u00077, "system__win32S");
   u00078 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00078, "system__wch_conB");
   u00079 : constant Version_32 := 16#36d8b2ea#;
   pragma Export (C, u00079, "system__wch_conS");
   u00080 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00080, "system__wch_stwB");
   u00081 : constant Version_32 := 16#1bc99eeb#;
   pragma Export (C, u00081, "system__wch_stwS");
   u00082 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00082, "system__wch_cnvB");
   u00083 : constant Version_32 := 16#396f0819#;
   pragma Export (C, u00083, "system__wch_cnvS");
   u00084 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00084, "system__wch_jisB");
   u00085 : constant Version_32 := 16#b91f1138#;
   pragma Export (C, u00085, "system__wch_jisS");
   u00086 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00086, "ada__streamsB");
   u00087 : constant Version_32 := 16#2e6701ab#;
   pragma Export (C, u00087, "ada__streamsS");
   u00088 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00088, "ada__io_exceptionsS");
   u00089 : constant Version_32 := 16#920eada5#;
   pragma Export (C, u00089, "ada__tagsB");
   u00090 : constant Version_32 := 16#13ca27f3#;
   pragma Export (C, u00090, "ada__tagsS");
   u00091 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00091, "system__htableB");
   u00092 : constant Version_32 := 16#a96723d2#;
   pragma Export (C, u00092, "system__htableS");
   u00093 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00093, "system__string_hashB");
   u00094 : constant Version_32 := 16#0b3948ac#;
   pragma Export (C, u00094, "system__string_hashS");
   u00095 : constant Version_32 := 16#1d9142a4#;
   pragma Export (C, u00095, "system__val_unsB");
   u00096 : constant Version_32 := 16#098b0180#;
   pragma Export (C, u00096, "system__val_unsS");
   u00097 : constant Version_32 := 16#b29d05bd#;
   pragma Export (C, u00097, "system__file_ioB");
   u00098 : constant Version_32 := 16#8ad4715d#;
   pragma Export (C, u00098, "system__file_ioS");
   u00099 : constant Version_32 := 16#cf417de3#;
   pragma Export (C, u00099, "ada__finalizationS");
   u00100 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00100, "system__finalization_rootB");
   u00101 : constant Version_32 := 16#6257e3a8#;
   pragma Export (C, u00101, "system__finalization_rootS");
   u00102 : constant Version_32 := 16#9dd55695#;
   pragma Export (C, u00102, "system__os_libB");
   u00103 : constant Version_32 := 16#bf5ce13f#;
   pragma Export (C, u00103, "system__os_libS");
   u00104 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00104, "system__stringsB");
   u00105 : constant Version_32 := 16#531a815e#;
   pragma Export (C, u00105, "system__stringsS");
   u00106 : constant Version_32 := 16#d03a0a90#;
   pragma Export (C, u00106, "system__file_control_blockS");
   u00107 : constant Version_32 := 16#22ad1eff#;
   pragma Export (C, u00107, "test_refactoringB");
   u00108 : constant Version_32 := 16#001f911f#;
   pragma Export (C, u00108, "test_refactoringS");
   u00109 : constant Version_32 := 16#b136ea9a#;
   pragma Export (C, u00109, "test_queuesB");
   u00110 : constant Version_32 := 16#01217fd8#;
   pragma Export (C, u00110, "test_queuesS");
   u00111 : constant Version_32 := 16#1916721b#;
   pragma Export (C, u00111, "stream_bufferB");
   u00112 : constant Version_32 := 16#b0179542#;
   pragma Export (C, u00112, "stream_bufferS");
   u00113 : constant Version_32 := 16#6abe5dbe#;
   pragma Export (C, u00113, "system__finalization_mastersB");
   u00114 : constant Version_32 := 16#7659a9f2#;
   pragma Export (C, u00114, "system__finalization_mastersS");
   u00115 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00115, "system__img_boolB");
   u00116 : constant Version_32 := 16#d87ce1d3#;
   pragma Export (C, u00116, "system__img_boolS");
   u00117 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00117, "system__storage_poolsB");
   u00118 : constant Version_32 := 16#0e480e95#;
   pragma Export (C, u00118, "system__storage_poolsS");
   u00119 : constant Version_32 := 16#5a895de2#;
   pragma Export (C, u00119, "system__pool_globalB");
   u00120 : constant Version_32 := 16#7141203e#;
   pragma Export (C, u00120, "system__pool_globalS");
   u00121 : constant Version_32 := 16#ee101ba4#;
   pragma Export (C, u00121, "system__memoryB");
   u00122 : constant Version_32 := 16#74d8f60c#;
   pragma Export (C, u00122, "system__memoryS");
   u00123 : constant Version_32 := 16#dfa03552#;
   pragma Export (C, u00123, "system__strings__stream_opsB");
   u00124 : constant Version_32 := 16#55d4bd57#;
   pragma Export (C, u00124, "system__strings__stream_opsS");
   u00125 : constant Version_32 := 16#c0e7c6c9#;
   pragma Export (C, u00125, "ada__streams__stream_ioB");
   u00126 : constant Version_32 := 16#31fc8e02#;
   pragma Export (C, u00126, "ada__streams__stream_ioS");
   u00127 : constant Version_32 := 16#5de653db#;
   pragma Export (C, u00127, "system__communicationB");
   u00128 : constant Version_32 := 16#34c5c5ea#;
   pragma Export (C, u00128, "system__communicationS");
   u00129 : constant Version_32 := 16#f4e1c091#;
   pragma Export (C, u00129, "system__stream_attributesB");
   u00130 : constant Version_32 := 16#8bc30a4e#;
   pragma Export (C, u00130, "system__stream_attributesS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.handling%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.val_lli%s
   --  system.val_llu%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_llu%b
   --  system.val_lli%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps.constants%s
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  system.communication%s
   --  system.communication%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.file_control_block%s
   --  ada.streams.stream_io%s
   --  system.file_io%s
   --  ada.streams.stream_io%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.win32%s
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.object_reader%s
   --  system.dwarf_lines%s
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.secondary_stack%s
   --  system.finalization_masters%b
   --  system.file_io%b
   --  interfaces.c%b
   --  ada.tags%b
   --  ada.strings.maps%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  ada.characters.handling%b
   --  system.secondary_stack%b
   --  system.dwarf_lines%b
   --  system.object_reader%b
   --  system.address_image%b
   --  ada.exceptions.traceback%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  stream_buffer%s
   --  stream_buffer%b
   --  test069%s
   --  test_queues%s
   --  test_queues%b
   --  test_refactoring%s
   --  test_refactoring%b
   --  test069%b
   --  END ELABORATION ORDER


end ada_main;
