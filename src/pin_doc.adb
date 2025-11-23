with Ada.Command_Line;
with Ada.Text_IO;
with Cli_Run_File;

procedure Pin_Doc is
begin
   if Ada.Command_Line.Argument_Count = 0
     or else Ada.Command_Line.Argument (1) = "help"
   then
      Ada.Text_IO.Put_Line ("Usage: pin-doc <command> [args]");
      Ada.Text_IO.Put_Line ("Commands:");
      Ada.Text_IO.Put_Line ("    eval [document]");
      Ada.Text_IO.Put_Line ("    => Generates HTML output for ~document~.");
      Ada.Text_IO.Put_Line
        ("    => Omit ~document~ to run immediate mode (REPL)");
   else
      if Ada.Command_Line.Argument (1) = "eval" then
         if Ada.Command_Line.Argument_Count = 1 then
            Cli_Run_File.Evaluate_File;
         elsif Ada.Command_Line.Argument_Count = 2 then
            Cli_Run_File.Evaluate_File (Ada.Command_Line.Argument (2));
         else
            Ada.Text_IO.Put_Line ("Wrong number of parameters. See ~help~.");
         end if;
      end if;
   end if;
end Pin_Doc;
