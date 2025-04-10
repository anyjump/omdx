let show_sexp x = Omdx.to_sexp x |> print_string
let show_html x = Omdx.to_html x |> print_string

let%expect_test "construct inline elements" =
  show_sexp
    Omdx.Ctor.
      [ p
          [ em "emphasized"
          ; br
          ; strong ~attrs:[ "class", "my-class" ] "strong"
          ; nl
          ; code "some code"
          ; nl
          ; a "label" ~url:"my/page/url"
          ; nl
          ; a "other label" ~url:"my/other/page" ~title:"title text"
          ; nl
          ; img "my/img/src" ~alt:"Some alt text"
          ; nl
          ; img "my/img/src" ~alt:"Some alt text" ~title:"some title"
          ; nl
          ; html "<em>inline html <!-- with a comment! --> here</em>"
          ]
      ];
  [%expect
    {|
    ((paragraph
      (concat (emph emphasized) hard-break (strong strong) soft-break code
       soft-break (url (link label my/page/url)) soft-break
       (url (link "other label" my/other/page "title text")) soft-break img
       soft-break img soft-break
       (html "<em>inline html <!-- with a comment! --> here</em>"))))
    |}]
;;

let%expect_test "construct headings" =
  show_sexp Omdx.Ctor.[ h 1 ~attrs:[ "class", "my-class" ] [ txt "Heading 1" ] ];
  [%expect {| ((heading 1 (concat "Heading 1"))) |}];
  show_sexp Omdx.Ctor.[ h 6 [ txt "Heading 6"; em "with emphasis!" ] ];
  [%expect {| ((heading 6 (concat "Heading 6" (emph "with emphasis!")))) |}]
;;

let%expect_test "construct lists" =
  show_sexp
    Omdx.Ctor.
      [ ul
          ~spacing:Tight
          [ [ p [ txt "Item 1" ] ]
          ; [ p [ txt "Item 2" ] ]
          ; [ p [ txt "Item 3"; strong "with strength!" ] ]
          ]
      ];
  [%expect
    {|
    ((list (list-item (paragraph (concat "Item 1")))
      (list-item (paragraph (concat "Item 2")))
      (list-item (paragraph (concat "Item 3" (strong "with strength!"))))))
    |}];
  show_sexp
    Omdx.Ctor.
      [ ol [ [ p [ txt "Item 1" ] ]; [ p [ txt "Item 2" ] ]; [ p [ txt "Item 3" ] ] ] ];
  [%expect
    {|
    ((list (list-item (paragraph (concat "Item 1")))
      (list-item (paragraph (concat "Item 2")))
      (list-item (paragraph (concat "Item 3")))))
    |}]
;;

let%expect_test "construct paragraphs and blockquotes with hrs" =
  let para =
    Omdx.Ctor.(
      p ~attrs:[ "class", "my-para" ] [ txt "Contet of"; em "this"; txt "paragraph" ])
  in
  show_sexp Omdx.Ctor.[ blockquote [ para; hr; p [ txt "Content of second paragraph" ] ] ];
  [%expect
    {|
    ((blockquote (paragraph (concat "Contet of" (emph this) paragraph))
      thematic-break (paragraph (concat "Content of second paragraph"))))
    |}]
;;

let%expect_test "construct code blocks" =
  show_sexp
    Omdx.Ctor.
      [ code_bl ~attrs:[ "class", "my-code" ] ~lang:"ocaml" "let foo = bar + bing" ];
  [%expect {| ((code-block ocaml)) |}]
;;

(* let%expect_test "construct html blocks" = *)
(*   show *)
(*     Omdx.Ctor. *)
(*       [ html_bl "<p><em>Some</em> inline HTML <!-- With a comment --> here</p>" *)
(*       ]; *)
(*   [%expect {| ((html "<p><em>Some</em> inline HTML <!-- With a comment --> here</p>")) |}] *)

let%expect_test "construct definition list" =
  show_sexp
    Omdx.Ctor.
      [ dl
          [ { term = [ txt "def term 1" ]
            ; defs =
                [ [ txt "definition 1.1" ]
                ; [ txt "definition 1.2" ]
                ; [ txt "definition 1.3" ]
                ]
            }
          ; { term = [ txt "def term 2" ]
            ; defs =
                [ [ txt "definition 2.1" ]
                ; [ txt "definition 2.2" ]
                ; [ txt "definition 2.3" ]
                ]
            }
          ]
      ];
  [%expect
    {|
    ((def-list
      (((concat "def term 1")
        ((concat "definition 1.1") (concat "definition 1.2")
         (concat "definition 1.3")))
       ((concat "def term 2")
        ((concat "definition 2.1") (concat "definition 2.2")
         (concat "definition 2.3"))))))
    |}]
;;

let%expect_test "basic markdown document" =
  let h1 =
    {|
  # Heading 1

  This is a nice and basic markdown document.
  With a paragraph on 2 lines.

  And then another paragraph.
|}
  in
  Omdx.of_string h1 |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((heading 1 "Heading 1")
     (paragraph
      (concat "This is a nice and basic markdown document." soft-break
       "With a paragraph on 2 lines."))
     (paragraph "And then another paragraph."))
    |}]
;;

let%expect_test "unordered list" =
  let h1 =
    {|
  - ul 1
  - ul 2
  - ul 3
  - ul 4
|}
  in
  Omdx.of_string h1 |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((list (list-item (paragraph "ul 1")) (list-item (paragraph "ul 2"))
      (list-item (paragraph "ul 3")) (list-item (paragraph "ul 4"))))
    |}]
;;

let%expect_test "ordered list" =
  let h1 =
    {|
  - ul 1
  - ul 2
  - ul 3
    1. ol 1
    2. ol 2
    3. ol 3
  - ul 4
|}
  in
  Omdx.of_string h1 |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((list (list-item (paragraph "ul 1")) (list-item (paragraph "ul 2"))
      (list-item (paragraph "ul 3")
       (list (list-item (paragraph "ol 1")) (list-item (paragraph "ol 2"))
        (list-item (paragraph "ol 3"))))
      (list-item (paragraph "ul 4"))))
    |}]
;;

let%expect_test "nested mixed lists" =
  let h1 =
    {|
  1. ol 1
  2. ol 2
  3. ol 3
  4. ol 4
|}
  in
  Omdx.of_string h1 |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((list (list-item (paragraph "ol 1")) (list-item (paragraph "ol 2"))
      (list-item (paragraph "ol 3")) (list-item (paragraph "ol 4"))))
    |}]
;;

let%expect_test "html in markdown" =
  let h1 =
    {|
  # Heading 1

  <div>
  This is a nice and basic markdown document.
  With a paragraph on 2 lines.
  </div>

|}
  in
  Omdx.of_string h1 |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((heading 1 "Heading 1")
     (html-block div ()
      (paragraph
       (concat "This is a nice and basic markdown document." soft-break
        "With a paragraph on 2 lines."))))
    |}]
;;

let%expect_test "html with attributes" =
  let h1 =
    {|
  # Heading 1

  <div className="text-white-500 bg-gray-900" tight testing="true">
  This is a nice and basic markdown document.
  With a paragraph on 2 lines.
  </div>

|}
  in
  Omdx.of_string h1 |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((heading 1 "Heading 1")
     (html-block div
      ((className "text-white-500 bg-gray-900") (tight ) (testing true))
      (paragraph
       (concat "This is a nice and basic markdown document." soft-break
        "With a paragraph on 2 lines."))))
    |}]
;;

let%expect_test "jsx" =
  let h1 =
    {|
  # Heading 1

  <div>
    <CustomComponent className="text-white-500 bg-gray-900">
      <p>Some text</p>
    </CustomComponent>
    This is a nice and basic markdown document.
    With a paragraph on 2 lines.
  </div>

|}
  in
  Omdx.of_string h1 |> Omdx.to_html |> print_string;
  [%expect
    {|
    <h1 id="heading-1">
      Heading 1
    </h1>
    <div>
      <CustomComponent className="text-white-500 bg-gray-900">
      <p>Some text</p>
      </CustomComponent>
      This is a nice and basic markdown document.
      With a paragraph on 2 lines.
    </div>
    |}]
;;

let%expect_test "markdown in jsx" =
  let h1 =
    {|
# Heading 1

<div>
<CustomComponent className="text-white-500 bg-gray-900">
<p>Some text</p>
  - ul 1
  - ul 2
  - ul 3
</CustomComponent>
</div>

|}
  in
  Omdx.of_string h1 |> Omdx.to_html |> print_string;
  [%expect
    {|
    <h1 id="heading-1">
      Heading 1
    </h1>
    <div>
      <CustomComponent className="text-white-500 bg-gray-900">
        <p>Some text</p><ul>
          <li>
            ul 1
          </li>
          <li>
            ul 2
          </li>
          <li>
            ul 3
          </li>
        </ul>
      </CustomComponent>
    </div>
    |}]
;;

let%expect_test "jsx custom component block" =
  let h1 =
    {|
# Heading 1

<CustomComponent className="text-white-500 bg-gray-900">
  Some text
</CustomComponent>
This is a nice and basic markdown document.
With a paragraph on 2 lines.

|}
  in
  Omdx.of_string h1 |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((heading 1 "Heading 1")
     (html-block CustomComponent ((className "text-white-500 bg-gray-900"))
      (paragraph "Some text"))
     (paragraph
      (concat "This is a nice and basic markdown document." soft-break
       "With a paragraph on 2 lines.")))
    |}]
;;

let%expect_test "html of custom component block" =
  let h1 =
    {|
# Heading 1

<CustomComponent className="text-white-500 bg-gray-900">
  Some text
</CustomComponent>
This is a nice and basic markdown document.
With a paragraph on 2 lines.

|}
  in
  Omdx.of_string h1 |> Omdx.to_html |> print_string;
  [%expect
    {|
    <h1 id="heading-1">
      Heading 1
    </h1>
    <CustomComponent className="text-white-500 bg-gray-900">
      Some text
    </CustomComponent>
    <p>
      This is a nice and basic markdown document.
      With a paragraph on 2 lines.
    </p>
    |}]
;;

let%expect_test "jsx custom component block" =
  let h1 =
    {|
# Heading 1

<CustomComponent className="text-white-500 bg-gray-900">
  <p>
    Some text
  </p>
</CustomComponent>
This is a nice and basic markdown document.
With a paragraph on 2 lines.

|}
  in
  Omdx.of_string h1 |> Omdx.to_html |> print_string;
  [%expect
    {|
    <h1 id="heading-1">
      Heading 1
    </h1>
    <CustomComponent className="text-white-500 bg-gray-900">
      <p>
        Some text
      </p>
    </CustomComponent>
    <p>
      This is a nice and basic markdown document.
      With a paragraph on 2 lines.
    </p>
    |}]
;;

let%expect_test "codeblocks" =
  let md =
    {|
# This is the documentation for the example markdown file

---

## It's a basic test but hey it works!

- ul 1
- ul 2
- ul 3
  1. ol 1
  2. ol 2
  3. ol 3
- ul 4

<CodeTabs>

```ocaml
we might even have something fun in here like this!
```

</CodeTabs>
|}
  in
  Omdx.of_string md |> Omdx.to_html |> print_string;
  [%expect
    {|
    <h1 id="this-is-the-documentation-for-the-example-markdown-file">
      This is the documentation for the example markdown file
    </h1>
    <hr />
    <h2 id="its-a-basic-test-but-hey-it-works">
      It's a basic test but hey it works!
    </h2>
    <ul>
      <li>
        ul 1
      </li>
      <li>
        ul 2
      </li>
      <li>
        ul 3<ol>
          <li>
            ol 1
          </li>
          <li>
            ol 2
          </li>
          <li>
            ol 3
          </li>
        </ol>
      </li>
      <li>
        ul 4
      </li>
    </ul>
    <CodeTabs>
      <pre>
        <code class="language-ocaml">we might even have something fun in here like this!
    </code>
      </pre>
    </CodeTabs>
    |}]
;;
