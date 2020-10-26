open OUnit

let tests =
  "ppx_markdown" >:::
  [ "text" >:: begin fun () ->
      assert_equal ~printer:Omd_backend.sexpr_of_md
        [Omd.Paragraph [Omd.Text "some text"]]
        [%markdown {|some text|}]
    end

  ; "styled text" >:: begin fun () ->
      assert_equal ~printer:Omd_backend.sexpr_of_md
        [Omd.Paragraph [ Omd.Text "some text "
                       ; Omd.Bold [Omd.Text "bold"]
                       ; Omd.Text " "
                       ; Omd.Emph [Omd.Text "emphasized"]]]
        [%markdown {|some text **bold** *emphasized*|}]
    end

  ; "newlines" >:: begin fun () ->
      assert_equal ~printer:Omd_backend.sexpr_of_md
        [Omd.Paragraph [ Omd.Text "some text"
                       ; Omd.NL
                       ; Omd.Bold [Omd.Text "bold"]
                       ; Omd.NL
                       ; Omd.Emph [Omd.Text "emphasized"]]]
        [%markdown {|some text
**bold**
*emphasized*|}]
    end

  ; "inline code" >:: begin fun () ->
      assert_equal ~printer:Omd_backend.sexpr_of_md
        [Omd.Paragraph [ Omd.Text "some text"
                       ; Omd.NL
                       ; Omd.Text "with "
                       ; Omd.Code ("", "inline code")
                       ; Omd.Text " "
                       ; Omd.Emph [Omd.Text "emphasized"]]]
        [%markdown {|some text
with `inline code` *emphasized*|}]
    end

  ; "heading" >:: begin fun () ->
        assert_equal ~printer:Omd_backend.sexpr_of_md
          [Omd.H1 [Omd.Text "Heading"]]
          [%markdown {|# Heading|}]
      end

  ; "code block" >:: begin fun () ->
      assert_equal ~printer:Omd_backend.sexpr_of_md
        [Omd.Code_block ("", "this is a code block")]
        [%markdown {|
```
this is a code block
```
|}]
    end

    ; "code block" >:: begin fun () ->
      assert_equal ~printer:Omd_backend.sexpr_of_md
        [Omd.Code_block ("type", "this is a code block")]
        [%markdown {|
```type
this is a code block
```
|}]
      end
    ]

let _ =
  run_test_tt_main tests
