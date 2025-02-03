PACKAGE test_gnatpp IS

    TYPE development_switch IS
       (none,                                                         --  0

        general_tracing,                                              --  1
        --
        -- Additional output in the GCU trace

        detailed_tracing,                                             --  2
        --
        -- Detailed debug output in every 200 Hz cycle

        serial_write_tracing,                                         --  3
        --
        -- Protocol all outgoing bytes

        serial_read_tracing,                                          --  4
        --
        -- Protocol all incoming bytes

        trigger_booster_overwrite_supply,                             --  5
        --
        -- The power supply for the trigger drive is always set to high.

        select_rof_num                                                --  6
        --
        -- Activate the numeric setting of the rate-of-fire. The numeric
        -- value must be set with the function set_rof_num().

        );

END test_gnatpp;
