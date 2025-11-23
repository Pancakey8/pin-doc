with Ada.Characters;
with Ada.Text_IO;
with Ada.Wide_Characters;
with Ada.Wide_Characters.Unicode;
with Ada.Wide_Text_IO;

package body Document_Format is
   Null_Char : constant Wide_Character := Wide_Character'First;

   function Peek (Input : U16_Str; Cursor : Positive) return Wide_Character
   is (if Cursor > Input'Length then Null_Char else Input (Cursor));

   --  takes Input for compatibility
   procedure Pop (Input : U16_Str; Cursor : in out Positive) is
   begin
      Cursor := Cursor + 1;
   end Pop;

   function Matches
     (Input : U16_Str; Cursor : Positive; Pattern : U16_Str) return Boolean
   is (Cursor + Pattern'Length - 1 <= Input'Length
       and then Input (Cursor .. Cursor + Pattern'Length - 1) = Pattern);

   procedure Pop_N (Input : U16_Str; Cursor : in out Positive; N : Positive) is
   begin
      Cursor := Cursor + N;
   end Pop_N;

   function Is_Newline (C : Wide_Character) return Boolean
   renames Ada.Wide_Characters.Unicode.Is_Line_Terminator;

   function Is_Special_Char (C : Wide_Character) return Boolean
   is (C = '*'
       or else C = '/'
       or else C = '_'
       or else C = '~'
       or else Is_Newline (C));

   function Parse_Rich_Text
     (Input : U16_Str; Cursor : in out Positive) return Vec_Rich_Text.Vector
   is
      function Tag_Open_Close
        (Pattern  : Wide_String;
         Tag      : Rich_Text_Tag;
         Children : out Vec_Rich_Text.Vector) return Boolean is
      begin
         if Matches (Input, Cursor, Pattern) then
            Pop_N (Input, Cursor, Pattern'Length);
            while Peek (Input, Cursor) /= Null_Char loop
               if Matches (Input, Cursor, Pattern) then
                  Pop_N (Input, Cursor, Pattern'Length);
                  for Child of Children loop
                     Child.Format (Tag) := True;
                  end loop;
                  return True;
               end if;
               Children.Append_Vector (Parse_Rich_Text (Input, Cursor));
            end loop;
            Children.Insert
              (1,
               (Format  => [Plain => True, others => <>],
                Content => UB_Wide.To_Unbounded_Wide_String (Pattern)),
               Count => 1);
            return True;
         end if;
         return False;
      end Tag_Open_Close;

      Children : Vec_Rich_Text.Vector;
   begin
      if Tag_Open_Close ("*", Bold, Children) then
         return Children;
      end if;

      if Tag_Open_Close ("/", Italic, Children) then
         return Children;
      end if;

      if Tag_Open_Close ("_", Underline, Children) then
         return Children;
      end if;

      if Tag_Open_Close ("~", Code, Children) then
         return Children;
      end if;

      declare
         C     : Wide_Character;
         Start : constant Positive := Cursor;
         V     : Vec_Rich_Text.Vector;
      begin
         loop
            C := Peek (Input, Cursor);
            if C = Null_Char or else Is_Special_Char (C) then
               exit;
            end if;
            Pop (Input, Cursor);
         end loop;
         if Cursor /= Start then
            V.Append
              ((Format  => [Plain => True, others => False],
                Content =>
                  UB_Wide.To_Unbounded_Wide_String
                    (Input (Start .. Cursor - 1))),
               Count => 1);
         end if;
         return V;
      end;
   end Parse_Rich_Text;

   function Begin_Newline
     (Input : U16_Str; Cursor : in out Positive) return Boolean is
   begin
      if Cursor /= 1 then
         if not Is_Newline (Peek (Input, Cursor)) then
            return False;
         end if;
         Pop (Input, Cursor);
      end if;
      return True;
   end Begin_Newline;

   function Parse_Header
     (Input : U16_Str; Cursor : in out Positive; Header : out Header_Text)
      return Boolean
   is
      Level : Header_Level;
      Start : constant Positive := Cursor;
   begin
      if not Begin_Newline (Input, Cursor) then
         return False;
      end if;

      if Matches (Input, Cursor, "###") then
         Pop_N (Input, Cursor, 3);
         Level := Head_SSub;
      elsif Matches (Input, Cursor, "##") then
         Pop_N (Input, Cursor, 2);
         Level := Head_Sub;
      elsif Matches (Input, Cursor, "#") then
         Pop (Input, Cursor);
         Level := Head_Main;
      else
         Cursor := Start;
         return False;
      end if;

      declare
         C     : Wide_Character;
         Start : constant Positive := Cursor;
      begin
         loop
            C := Peek (Input, Cursor);
            if C = Null_Char or else Is_Newline (C) then
               exit;
            end if;
            Pop (Input, Cursor);
         end loop;
         Header.Level := Level;
         Header.Content :=
           UB_Wide.To_Unbounded_Wide_String (Input (Start .. Cursor - 1));
         return True;
      end;
   end Parse_Header;

   function Parse_All
     (Input : U16_Str; Cursor : in out Positive) return Vec_Node.Vector
   is
      Nodes : Vec_Node.Vector;
      function Try_Newline return Boolean is
      begin
         declare
            Header : Header_Text;
         begin
            if Parse_Header (Input, Cursor, Header) then
               Nodes.Append (Header);
               return True;
            end if;
         end;
         if Parse_Comment (Input, Cursor) then
            return True;
         end if;
         return False;
      end Try_Newline;
   begin
      while Peek (Input, Cursor) /= Null_Char loop
         if not Try_Newline then
            if Is_Newline (Peek (Input, Cursor)) then
               Pop (Input, Cursor);
               if Is_Newline (Peek (Input, Cursor)) then
                  -- Allow semantic newline
                  Nodes.Append (Line_Break'(null record));
               else
                  Nodes.Append
                    (Rich_Text'
                       (Format  => [Plain => True, others => False],
                        Content => UB_Wide.To_Unbounded_Wide_String (" ")));
               end if;
            end if;
            for Node of Parse_Rich_Text (Input, Cursor) loop
               Nodes.Append (Node);
            end loop;
         end if;
      end loop;
      Nodes.Append (Line_Break'(null record));
      return Nodes;
   end Parse_All;

   overriding
   procedure Print_Node (Node : Rich_Text) is
   begin
      for Flag in Node.Format'Range loop
         if Node.Format (Flag) then
            Ada.Wide_Text_IO.Put (Rich_Text_Tag'Wide_Image (Flag) & " ");
         end if;
      end loop;
      Ada.Text_IO.Put (ASCII.LF);
      Ada.Wide_Text_IO.Put_Line (UB_Wide.To_Wide_String (Node.Content));
      Ada.Text_IO.Put (ASCII.LF);
   end Print_Node;

   overriding
   procedure Print_Node (Node : Header_Text) is
   begin
      Ada.Text_IO.Put_Line ("Header " & Header_Level'Image (Node.Level));
      Ada.Wide_Text_IO.Put_Line (UB_Wide.To_Wide_String (Node.Content));
   end Print_Node;

   overriding
   procedure Print_Node (Node : Line_Break) is
   begin
      Ada.Text_IO.Put_Line ("Line Break");
   end Print_Node;

   function Parse_Comment
     (Input : U16_Str; Cursor : in out Positive) return Boolean
   is
      Start : constant Positive := Cursor;
   begin
      if not Begin_Newline (Input, Cursor) then
         return False;
      end if;
      if not Matches (Input, Cursor, "::") then
         Cursor := Start;
         return False;
      end if;
      loop
         declare
            P : constant Wide_Character := Peek (Input, Cursor);
         begin
            if P = Null_Char then
               exit;
            end if;
            if Is_Newline (P) then
               Pop (Input, Cursor);
               exit;
            end if;
            Pop (Input, Cursor);
         end;
      end loop;
      return True;
   end Parse_Comment;
end Document_Format;
