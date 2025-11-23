with Document_Format; use Document_Format;
with Ada.Tags;        use Ada.Tags;

package body HTML_Visitor is
   procedure Visit (V : in out Visitor; Node : Document_Format.Node'Class) is
      Unimplemented : exception;
   begin
      -- Node.Print_Node;
      if Node'Tag = Rich_Text'Tag then
         Visit_Rich_Text (V, Rich_Text (Node));
      elsif Node'Tag = Header_Text'Tag then
         Visit_Header (V, Header_Text (Node));
      elsif Node'Tag = Line_Break'Tag then
         Visit_Line_Break (V, Line_Break (Node));
      else
         raise Unimplemented;
      end if;
   end Visit;

   type Str_Lit is access constant Wide_String;
   type Format_Map is array (Rich_Text_Tag) of Str_Lit;

   Bold_Html      : aliased constant Wide_String := "b";
   Italic_Html    : aliased constant Wide_String := "i";
   Underline_Html : aliased constant Wide_String := "u";
   Code_Html      : aliased constant Wide_String := "code";

   Format_Pairings : Format_Map :=
     [Plain     => null,
      Bold      => Bold_Html'Access,
      Italic    => Italic_Html'Access,
      Underline => Underline_Html'Access,
      Code      => Code_Html'Access];

   procedure Visit_Rich_Text
     (V : in out Visitor; Node : Document_Format.Rich_Text)
   is
      Plain_Format : Rich_Text_Format := [Plain => True, others => False];
   begin
      if not V.In_Paragraph then
         V.In_Paragraph := True;
         WIO.Put (V.Output.all, "<p>");
      end if;
      for Format in Format_Pairings'Range loop
         if Format /= Plain and then Node.Format (Format) then
            WIO.Put (V.Output.all, "<" & Format_Pairings (Format).all & ">");
         end if;
      end loop;
      WIO.Put
        (V.Output.all, Document_Format.UB_Wide.To_Wide_String (Node.Content));
      for Format in reverse Format_Pairings'Range loop
         if Format /= Plain and then Node.Format (Format) then
            WIO.Put (V.Output.all, "</" & Format_Pairings (Format).all & ">");
         end if;
      end loop;
   end Visit_Rich_Text;

   procedure Visit_Header
     (V : in out Visitor; Node : Document_Format.Header_Text) is
   begin
      if V.In_Paragraph then
         V.In_Paragraph := False;
         WIO.Put (V.Output.all, "</p>");
      end if;
      case Node.Level is
         when Document_Format.Head_Main =>
            WIO.Put (V.Output.all, "<h1>");

         when Document_Format.Head_Sub  =>
            WIO.Put (V.Output.all, "<h2>");

         when Document_Format.Head_SSub =>
            WIO.Put (V.Output.all, "<h3>");
      end case;
      WIO.Put
        (V.Output.all, Document_Format.UB_Wide.To_Wide_String (Node.Content));
      case Node.Level is
         when Document_Format.Head_Main =>
            WIO.Put_Line (V.Output.all, "</h1>");

         when Document_Format.Head_Sub  =>
            WIO.Put_Line (V.Output.all, "</h2>");

         when Document_Format.Head_SSub =>
            WIO.Put_Line (V.Output.all, "</h3>");
      end case;
   end Visit_Header;

   procedure Visit_Line_Break
     (V : in out Visitor; Node : Document_Format.Line_Break) is
   begin
      if V.In_Paragraph then
         V.In_Paragraph := False;
         WIO.Put (V.Output.all, "</p>");
      end if;
      WIO.Put_Line (V.Output.all, "<br>");
   end Visit_Line_Break;
end HTML_Visitor;
