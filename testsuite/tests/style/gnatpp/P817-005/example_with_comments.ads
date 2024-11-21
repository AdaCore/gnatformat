package Example_With_Comments is

   type Log_Type is
     (TRACE,   --< for following program flow
      DEBUG,   --< output useful purely for debugging puposes
      INFO,    --< informational output, does not indicate a problem
      WARN,    --< warning: something is not as expected
      ERROR,   --< error: something is wrong
      AWSLOG,  --< AWS message log
      AWSERR   --< AWS error log
     );

   procedure initialize
     (progname : String;  --< Name of the program running
      filename : String   --< Name of the config file to read
     ) with
      pre => progname'Length <= 64 and filename'Length <= 4_096;

end;
