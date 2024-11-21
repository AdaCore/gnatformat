package Examples is

   -- My Functions

   --  First Function
   function First
     (-- minimum value
   Min,
      -- maximum value
      Max : Integer;
      -- Prefix
      Prefix : String)
      --  Is Length of Prefix between Min and Max?
      return Boolean with
      --  Precondition Min <= Max
      Pre => Min <= Max;

end Examples;
