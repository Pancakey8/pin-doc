with Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.Wide_Unbounded;

package Document_Format is
   package UB_Wide renames Ada.Strings.Wide_Unbounded;
   subtype UB_Wide_Str is UB_Wide.Unbounded_Wide_String;

   package UTF renames Ada.Strings.UTF_Encoding;
   subtype U16_Str is UTF.UTF_16_Wide_String;

   type Node is abstract tagged null record;

   procedure Print_Node (Node : Document_Format.Node) is abstract;

   type Rich_Text_Tag is (Plain, Bold, Italic, Underline, Code);
   type Rich_Text_Format is array (Rich_Text_Tag) of Boolean;
   pragma Pack (Rich_Text_Format);

   type Rich_Text is new Node with record
      Format  : Rich_Text_Format;
      Content : UB_Wide_Str;
   end record;

   overriding
   procedure Print_Node (Node : Rich_Text);

   package Vec_Rich_Text is new Ada.Containers.Vectors (Positive, Rich_Text);

   function Parse_Rich_Text
     (Input : U16_Str; Cursor : in out Positive) return Vec_Rich_Text.Vector;

   type Header_Level is (Head_Main, Head_Sub, Head_SSub);
   type Header_Text is new Node with record
      Level   : Header_Level;
      Content : UB_Wide_Str;
   end record;

   overriding
   procedure Print_Node (Node : Header_Text);

   type Line_Break is new Node with null record;

   overriding
   procedure Print_Node (Node : Line_Break);

   function Parse_Header
     (Input : U16_Str; Cursor : in out Positive; Header : out Header_Text)
      return Boolean;

   function Parse_Comment
     (Input : U16_Str; Cursor : in out Positive)
      return Boolean;

   package Vec_Node is new
     Ada.Containers.Indefinite_Vectors (Positive, Node'Class);

   function Parse_All
     (Input : U16_Str; Cursor : in out Positive) return Vec_Node.Vector;
end Document_Format;
