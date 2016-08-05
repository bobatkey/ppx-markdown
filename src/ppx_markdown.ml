module Translation = struct
  let list f l =
    List.fold_right
      (fun x expr -> [%expr [%e f x] :: [%e expr]])
      l
      [%expr [] ]

  let string str =
    Ast_helper.(Exp.constant (Const.string str))

  let pair t1 t2 (a,b) =
    [%expr ([%e t1 a], [%e t2 b])]

  let option t = function
    | None   -> [%expr None]
    | Some x -> [%expr Some [%e t x]]

  module Omd = struct
    let rec element = function
      | Omd.H1 doc ->
         [%expr Omd_representation.H1 [%e omd doc]]

      | Omd.H2 doc ->
         [%expr Omd_representation.H2 [%e omd doc]]

      | Omd.H3 doc ->
         [%expr Omd_representation.H3 [%e omd doc]]

      | Omd.H4 doc ->
         [%expr Omd_representation.H4 [%e omd doc]]

      | Omd.H5 doc ->
         [%expr Omd_representation.H5 [%e omd doc]]

      | Omd.H6 doc ->
         [%expr Omd_representation.H6 [%e omd doc]]

      | Omd.Paragraph doc ->
         [%expr Omd_representation.Paragraph [%e omd doc]]

      | Omd.Text str ->
         [%expr Omd_representation.Text [%e string str]]

      | Omd.Emph doc ->
         [%expr Omd_representation.Emph [%e omd doc]]

      | Omd.Bold doc ->
         [%expr Omd_representation.Bold [%e omd doc]]

      | Omd.Ul docs ->
         [%expr Omd_representation.Ul [%e list omd docs]]

      | Omd.Ol docs ->
         [%expr Omd_representation.Ol [%e list omd docs]]

      | Omd.Ulp docs ->
         [%expr Omd_representation.Ulp [%e list omd docs]]

      | Omd.Olp docs ->
         [%expr Omd_representation.Olp [%e list omd docs]]

      | Omd.Code (lang, code) ->
         [%expr Omd_representation.Code
                  ([%e string lang],
                   [%e string code])
         ]

      | Omd.Code_block (lang, code) ->
         [%expr Omd_representation.Code_block
                  ([%e string lang],
                   [%e string code])
         ]

      | Omd.Br ->
         [%expr Omd_representation.Br]

      | Omd.Hr ->
         [%expr Omd_representation.Hr]

      | Omd.NL ->
         [%expr Omd_representation.NL]

      | Omd.Url (href, doc, title) ->
         [%expr Omd_representation.Url
                  ([%e string href],
                   [%e omd doc],
                   [%e string title])
         ]

      | Omd.Ref (references, name, text, fallback) ->
         begin match references#get_ref name with
           | None ->
              [%expr Omd_representation.Text
                       [%e string fallback#to_string]]
           | Some (href, title) ->
              [%expr Omd_representation.Url
                       ([%e string href],
                        [Omd_representation.Text [%e string text]],
                        [%e string title])
              ]
         end
              
      | Omd.Img_ref (references, name, alt, fallback) ->
         begin match references#get_ref name with
           | None ->
              [%expr Omd_representation.Text
                       [%e string fallback#to_string]]
           | Some (href, title) ->
              [%expr Omd_representation.Url
                       ([%e string alt],
                        [%e string href],
                        [%e string title])
              ]
         end
    
      | Omd.Html (name, attrs, doc) ->
         [%expr Omd_representation.Html
                  ([%e string name],
                   [%e list
                         (pair string (option string))
                         attrs],
                   [%e omd doc])
         ]

      | Omd.Html_block (name, attrs, doc) ->
         [%expr Omd_representation.Html_block
                  ([%e string name],
                   [%e list
                         (pair string (option string))
                         attrs],
                   [%e omd doc])
         ]

      | Omd.Html_comment str ->
         [%expr Omd_representation.Html_comment [%e string str]]

      | Omd.Raw str ->
         [%expr Omd_representation.Raw [%e string str]]

      | Omd.Raw_block str ->
         [%expr Omd_representation.Raw_block [%e string str]]

      | Omd.Blockquote doc ->
         [%expr Omd_representation.Blockquote [%e omd doc]]

      | Omd.Img (alt, src, title) ->
         [%expr Omd_representation.Img
                  ([%e string alt],
                   [%e string src],
                   [%e string title])]

      | Omd.X _ ->
         failwith "Markdown extensions not handled"

    and omd elems =
      list element elems
  end
end

let map_expr mapper expr =
  match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_constant (Parsetree.Pconst_string (str, Some "markdown")) ->
       str |> Omd.of_string |> Translation.Omd.omd

    | _ ->
       Ast_mapper.default_mapper.Ast_mapper.expr mapper expr

let _ =
  Ast_mapper.run_main
    (fun _ -> {Ast_mapper.default_mapper with Ast_mapper.expr = map_expr})
