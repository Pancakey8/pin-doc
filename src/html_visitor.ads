with Ada.Wide_Text_IO;
with Document_Format;

package HTML_Visitor is
   pragma Elaborate_Body;
   package WIO renames Ada.Wide_Text_IO;
   type File_Access is access all WIO.File_Type;
   type Visitor is record
      Output : File_Access;
      In_Paragraph : Boolean := False;
   end record;

   procedure Visit (V : in out Visitor; Node : Document_Format.Node'Class);

   procedure Visit_Rich_Text
     (V : in out Visitor; Node : Document_Format.Rich_Text);
   procedure Visit_Header
     (V : in out Visitor; Node : Document_Format.Header_Text);
   procedure Visit_Line_Break
     (V : in out Visitor; Node : Document_Format.Line_Break);
end HTML_Visitor;
