with Ada.Characters;
with Ada.Characters.Wide_Latin_1;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Wide_Characters;
with Ada.Wide_Text_IO;
with Document_Format; use Document_Format;
with HTML_Visitor;

procedure Pin_Doc is
   use Ada.Wide_Text_IO;
   Input    : UB_Wide_Str;
   Cursor   : Positive := 1;
   Document : Vec_Node.Vector;
   S : HTML_Visitor.File_Access := new File_Type'(Standard_Output);
   V : HTML_Visitor.Visitor := (Output => S, others => <>);
begin
   Ada.Text_IO.Put_Line ("Type \EOF to finalise.");
   loop
      declare
         Line : constant Wide_String := Get_Line;
      begin
         if Line = "\EOF" then
            exit;
         end if;
         UB_Wide.Append (Input, Line);
         UB_Wide.Append (Input, Ada.Characters.Wide_Latin_1.LF);
      end;
   end loop;
   Document :=
     Document_Format.Parse_All (UB_Wide.To_Wide_String (Input), Cursor);
   for Node of Document loop
      declare
         N : Document_Format.Node'Class renames Node;
      begin
         HTML_Visitor.Visit (V, N);
      end;
   end loop;
end Pin_Doc;
