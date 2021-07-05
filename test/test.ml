open OUnit

let tests =
  "ppx_markdown" >:::
  [ "text" >:: begin fun () ->
      assert_equal ~printer:Omd.to_sexp
        [Omd.Paragraph ([], Omd.Text ([], "some text"))]
        [%markdown {|some text|}]
    end

  ; "styled text" >:: begin fun () ->
      assert_equal ~printer:Omd.to_sexp
        Omd.[Paragraph ([],
                        Concat ([],
                                [ Text ([], "some text ")
                                ; Strong ([], Text ([], "bold"))
                                ; Omd.Text ([], " ")
                                ; Omd.Emph ([], Omd.Text ([], "emphasized"))
                                ]))]
        [%markdown {|some text **bold** *emphasized*|}]
    end

  ; "newlines" >:: begin fun () ->
      assert_equal ~printer:Omd.to_sexp
        Omd.[Paragraph ([],
                        Concat ([],
                                [ Text ([], "some text")
                                ; Soft_break []
                                ; Strong ([], Text ([], "bold"))
                                ; Soft_break []
                                ; Emph ([], Text ([], "emphasized"))
                                ]))]
        [%markdown {|some text
**bold**
*emphasized*|}]
    end

  ; "inline code" >:: begin fun () ->
      assert_equal ~printer:Omd.to_sexp
        Omd.[Paragraph ([],
                        Concat ([],
                                [ Text ([], "some text")
                                ; Soft_break []
                                ; Text ([], "with ")
                                ; Code ([], "inline code")
                                ; Text ([], " ")
                                ; Emph ([], Text ([], "emphasized"))
                                ]))]
        [%markdown {|some text
with `inline code` *emphasized*|}]
    end

  ; "heading" >:: begin fun () ->
        assert_equal ~printer:Omd.to_sexp
          [Omd.Heading ([], 1, Text ([], "Heading"))]
          [%markdown {|# Heading|}]
      end

  ; "code block" >:: begin fun () ->
      assert_equal ~printer:Omd.to_sexp
        [Omd.Code_block ([], "", "this is a code block\n")]
        [%markdown {|
```
this is a code block
```
|}]
    end

    ; "code block" >:: begin fun () ->
      assert_equal ~printer:Omd.to_sexp
        [Omd.Code_block ([], "type", "this is a code block\n")]
        [%markdown {|
```type
this is a code block
```
|}]
      end
    ]

let _ =
  run_test_tt_main tests
