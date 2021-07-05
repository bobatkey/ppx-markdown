open Ppxlib

module Translation = struct
  let list ~loc f l =
    List.fold_right
      (fun x expr -> [%expr [%e f ~loc x] :: [%e expr]])
      l
      [%expr [] ]

  let int i =
    Ast_helper.(Exp.constant (Const.int i))

  let char i =
    Ast_helper.(Exp.constant (Const.char i))

  let string str =
    Ast_helper.(Exp.constant (Const.string str))

  let pair ~loc t1 t2 (a,b) =
    [%expr ([%e t1 a], [%e t2 b])]

  let option ~loc t = function
    | None   -> [%expr None]
    | Some x -> [%expr Some [%e t x]]

  module Omd = struct

    let attributes ~loc =
      list ~loc (pair string string)

    let rec inline ~loc = function
      | Omd.Concat (attr, bits) ->
        [%expr Omd.Concat ([%e attributes ~loc attr], [%e inline_list ~loc bits])]
      | Omd.Text (attr, txt) ->
        [%expr Omd.Text ([%e attributes ~loc attr], [%e string txt])]
      | Omd.Code (attr, txt) ->
        [%expr Omd.Code ([%e attributes ~loc attr], [%e string txt])]
      | Omd.Html (attr, txt) ->
        [%expr Omd.Html ([%e attributes ~loc attr], [%e string txt])]
      | Omd.Emph (attr, bit) ->
        [%expr Omd.Emph ([%e attributes ~loc attr], [%e inline ~loc bit])]
      | Omd.Strong (attr, bit) ->
        [%expr Omd.Strong ([%e attributes ~loc attr], [%e inline ~loc bit])]
      | Omd.Hard_break attr ->
        [%expr Omd.Hard_break [%e attributes ~loc attr]]
      | Omd.Soft_break attr ->
        [%expr Omd.Soft_break [%e attributes ~loc attr]]
      | Omd.Link (attr, link_info) ->
        [%expr Omd.Link ([%e attributes ~loc attr], [%e link ~loc link_info])]
      | Omd.Image (attr, link_info) ->
        [%expr Omd.Image ([%e attributes ~loc attr], [%e link ~loc link_info])]

    and inline_list ~loc =
      list ~loc inline

    and link ~loc { Omd.label; destination; title } =
      [%expr { Omd.label = [%e inline ~loc label]
             ; Omd.destination = [%e string destination]
             ; Omd.title = [%e option ~loc string title ]
             } ]

    let def_elt ~loc { Omd.term; defs } =
      [%expr { Omd.term = [%e inline ~loc term]
             ; Omd.defs = [%e inline_list ~loc defs]
             } ]

    let list_type ~loc = function
      | Omd.Ordered (i, c) -> [%expr Omd.Ordered ([%e int i], [%e char c])]
      | Omd.Bullet c       -> [%expr Omd.Bullet ([%e char c])]

    let list_spacing ~loc = function
      | Omd.Loose -> [%expr Loose]
      | Omd.Tight -> [%expr Tight]

    let rec block ~loc = function
      | Omd.Heading (attrs, d, doc) ->
        [%expr Omd.Heading ([%e attributes ~loc attrs], [%e int d], [%e inline ~loc doc])]

      | Omd.Paragraph (attrs, doc) ->
        [%expr Omd.Paragraph ([%e attributes ~loc attrs], [%e inline ~loc doc])]

      | Omd.Blockquote (attr, doc) ->
        [%expr Omd.Blockquote ([%e attributes ~loc attr], [%e block_list ~loc doc])]

      | Omd.Thematic_break attr ->
        [%expr Omd.Thematic_break [%e attributes ~loc attr]]

      | Omd.Code_block (attr, s1, s2) ->
        [%expr Omd.Code_block ([%e attributes ~loc attr], [%e string s1], [%e string s2])]

      | Omd.Html_block (attr, s) ->
        [%expr Omd.Html_block ([%e attributes ~loc attr], [%e string s])]

      | Omd.Definition_list (attr, defs) ->
        [%expr Omd.Definition_list ([%e attributes ~loc attr], [%e list ~loc def_elt defs])]

      | Omd.List (attr, typ, spac, items) ->
        [%expr Omd.List ([%e attributes ~loc attr],
                         [%e list_type ~loc typ],
                         [%e list_spacing ~loc spac],
                         [%e list ~loc block_list items])]

    and block_list ~loc (blocks : Omd.doc) =
      list ~loc block blocks
  end
end

let expand ~ctxt str =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Translation.Omd.block_list ~loc (Omd.of_string str)

let my_extension =
 Extension.V3.declare
   "markdown"
   Extension.Context.expression
   Ast_pattern.(single_expr_payload (estring __))
   expand

let rule = Ppxlib.Context_free.Rule.extension my_extension

let () =
 Driver.register_transformation
   ~rules:[rule]
   "ppx_markdown"
