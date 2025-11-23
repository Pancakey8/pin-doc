with Ada.Characters.Wide_Latin_1;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Text_IO;
with Document_Format;
with HTML_Visitor;

package body Cli_Run_File is
   procedure Output_File (Input : Document_Format.UB_Wide_Str) is
      use Document_Format;
      Doc    : Document;
      Cursor : Positive := 1;
      F      : HTML_Visitor.File_Access :=
        new Ada.Wide_Text_IO.File_Type'(Ada.Wide_Text_IO.Standard_Output);
      procedure Dealloc_File_Type is new
        Ada.Unchecked_Deallocation
          (Object => Ada.Wide_Text_IO.File_Type,
           Name   => HTML_Visitor.File_Access);
   begin
      Doc :=
        Document_Format.Parse_All (UB_Wide.To_Wide_String (Input), Cursor);
      HTML_Visitor.Make_Document (F, Doc);
      Dealloc_File_Type (F);
   end Output_File;

   procedure Evaluate_File is
      use Document_Format;
      Input : Document_Format.UB_Wide_Str;
   begin
      Ada.Text_IO.Put_Line
        ("Immediate mode. Pass filepath as argument to load file instead");
      Ada.Text_IO.Put_Line ("Type \EOF to finalise.");

      loop
         declare
            Line : constant Wide_String := Ada.Wide_Text_IO.Get_Line;
         begin
            if Line = "\EOF" then
               exit;
            end if;
            UB_Wide.Append (Input, Line);
            UB_Wide.Append (Input, Ada.Characters.Wide_Latin_1.LF);
         end;
      end loop;

      Output_File (Input);
   end Evaluate_File;

   procedure Evaluate_File (Path : String) is
      use Document_Format;
      File  : Ada.Wide_Text_IO.File_Type;
      Input : UB_Wide_Str;
   begin
      Ada.Wide_Text_IO.Open (File, Ada.Wide_Text_IO.In_File, Path);
      while not Ada.Wide_Text_IO.End_Of_File (File) loop
         declare
            Line : constant Wide_String := Ada.Wide_Text_IO.Get_Line (File);
         begin
            UB_Wide.Append (Input, Line);
            UB_Wide.Append (Input, Ada.Characters.Wide_Latin_1.LF);
         end;
      end loop;
      Output_File (Input);
   end Evaluate_File;
end Cli_Run_File;
