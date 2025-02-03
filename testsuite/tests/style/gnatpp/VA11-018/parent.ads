------------------------------------------------------------------------------
-- @Purpose:
-- This annotation belongs to package Parent.
------------------------------------------------------------------------------
package Parent is
   procedure Initialize (Mode : Mode);
    ---------------------------------------------------------------------------
    -- @Purpose: This annotation belongs to package Child.
    --
    ---------------------------------------------------------------------------
   package Child is
      procedure Do_Something (Var : Integer);
   end Child;
end Parent;
