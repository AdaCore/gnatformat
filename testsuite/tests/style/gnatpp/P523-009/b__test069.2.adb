pragma Ada_95;
pragma Warnings (Off);
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test069.2.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test069.2.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E103 : Short_Integer; pragma Import (Ada, E103, "system__os_lib_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "system__soft_links_E");
   E024 : Short_Integer; pragma Import (Ada, E024, "system__exception_table_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "ada__io_exceptions_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "ada__strings_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "ada__strings__maps_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__strings__maps__constants_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "ada__tags_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "ada__streams_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "interfaces__c_E");
   E026 : Short_Integer; pragma Import (Ada, E026, "system__exceptions_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "system__file_control_block_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "ada__streams__stream_io_E");
   E098 : Short_Integer; pragma Import (Ada, E098, "system__file_io_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "system__finalization_root_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "ada__finalization_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "system__storage_pools_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "system__finalization_masters_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "system__object_reader_E");
   E042 : Short_Integer; pragma Import (Ada, E042, "system__dwarf_lines_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "system__pool_global_E");
   E018 : Short_Integer; pragma Import (Ada, E018, "system__secondary_stack_E");
   E037 : Short_Integer; pragma Import (Ada, E037, "system__traceback__symbolic_E");
   E007 : Short_Integer; pragma Import (Ada, E007, "ada__text_io_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "stream_buffer_E");
   E002 : Short_Integer; pragma Import (Ada, E002, "test069_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "test_queues_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "test_refactoring_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E007 := E007 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "ada__text_io__finalize_spec");
      begin
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "system__file_io__finalize_body");
      begin
         E098 := E098 - 1;
         F2;
      end;
      E114 := E114 - 1;
      E120 := E120 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "system__pool_global__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "system__finalization_masters__finalize_spec");
      begin
         F4;
      end;
      E126 := E126 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "ada__streams__stream_io__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := 50176;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E024 := E024 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E088 := E088 + 1;
      Ada.Strings'Elab_Spec;
      E047 := E047 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E053 := E053 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E087 := E087 + 1;
      Interfaces.C'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E026 := E026 + 1;
      System.File_Control_Block'Elab_Spec;
      E106 := E106 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E126 := E126 + 1;
      System.Finalization_Root'Elab_Spec;
      E101 := E101 + 1;
      Ada.Finalization'Elab_Spec;
      E099 := E099 + 1;
      System.Storage_Pools'Elab_Spec;
      E118 := E118 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Object_Reader'Elab_Spec;
      System.Dwarf_Lines'Elab_Spec;
      System.Pool_Global'Elab_Spec;
      E120 := E120 + 1;
      System.Finalization_Masters'Elab_Body;
      E114 := E114 + 1;
      System.File_Io'Elab_Body;
      E098 := E098 + 1;
      E064 := E064 + 1;
      Ada.Tags'Elab_Body;
      E090 := E090 + 1;
      E049 := E049 + 1;
      System.Soft_Links'Elab_Body;
      E014 := E014 + 1;
      System.Os_Lib'Elab_Body;
      E103 := E103 + 1;
      System.Secondary_Stack'Elab_Body;
      E018 := E018 + 1;
      E042 := E042 + 1;
      E062 := E062 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E037 := E037 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E007 := E007 + 1;
      E112 := E112 + 1;
      test_queues'elab_spec;
      E110 := E110 + 1;
      test_refactoring'elab_body;
      E108 := E108 + 1;
      E002 := E002 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test069");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   C:\Projects\gps_test\Test069_refactoring\obj\native\stream_buffer.2.o
   --   C:\Projects\gps_test\Test069_refactoring\obj\native\test_queues.1.o
   --   C:\Projects\gps_test\Test069_refactoring\obj\native\test_refactoring.2.o
   --   C:\Projects\gps_test\Test069_refactoring\obj\native\test069.2.o
   --   -LC:\Projects\gps_test\Test069_refactoring\obj\native\
   --   -LC:\Projects\gps_test\Test069_refactoring\obj\native\
   --   -LC:/gnatpro/7.4.1/lib/gcc/i686-pc-mingw32/4.9.4/adalib/
   --   -static
   --   -lgnat
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
