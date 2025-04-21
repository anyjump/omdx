let show_sexp x = Omdx.to_sexp x |> print_string
let show_html x = Omdx.to_html x |> print_string

let%expect_test "unordered list" =
  let s =
    {|
  - ul 1
  - ul 2
  - ul 3
  - ul 4
|}
  in
  Omdx.of_string s |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((metadata none)
     ((list () (bullet -) tight (list-item (paragraph () "ul 1"))
       (list-item (paragraph () "ul 2")) (list-item (paragraph () "ul 3"))
       (list-item (paragraph () "ul 4")))))
    |}]
;;

let%expect_test "ordered list" =
  let s =
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
  Omdx.of_string s |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((metadata none)
     ((list () (bullet -) tight (list-item (paragraph () "ul 1"))
       (list-item (paragraph () "ul 2"))
       (list-item (paragraph () "ul 3")
        (list () (ordered 1 .) tight (list-item (paragraph () "ol 1"))
         (list-item (paragraph () "ol 2")) (list-item (paragraph () "ol 3"))))
       (list-item (paragraph () "ul 4")))))
    |}]
;;

let%expect_test "nested mixed lists" =
  let s =
    {|
  1. ol 1
  2. ol 2
  3. ol 3
  4. ol 4
|}
  in
  Omdx.of_string s |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((metadata none)
     ((list () (ordered 1 .) tight (list-item (paragraph () "ol 1"))
       (list-item (paragraph () "ol 2")) (list-item (paragraph () "ol 3"))
       (list-item (paragraph () "ol 4")))))
    |}]
;;

let%expect_test "html in markdown" =
  let s =
    {|
  # Heading 1

  <div>
  This is a nice and basic markdown document.
  With a paragraph on 2 lines.
  </div>

|}
  in
  Omdx.of_string s |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((metadata none)
     ((heading () 1 "Heading 1")
      (html-block div ()
       (paragraph ()
        (concat "This is a nice and basic markdown document." soft-break
         "With a paragraph on 2 lines.")))))
    |}]
;;

let%expect_test "html with attributes" =
  let s =
    {|
  # Heading 1

  <div className="text-white-500 bg-gray-900" tight testing="true">
  This is a nice and basic markdown document.
  With a paragraph on 2 lines.
  </div>

|}
  in
  Omdx.of_string s |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((metadata none)
     ((heading () 1 "Heading 1")
      (html-block div
       ((className "text-white-500 bg-gray-900") (tight "") (testing true))
       (paragraph ()
        (concat "This is a nice and basic markdown document." soft-break
         "With a paragraph on 2 lines.")))))
    |}]
;;

let%expect_test "jsx" =
  let s =
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
  Omdx.of_string s |> Omdx.content |> Omdx.to_html |> print_string;
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
  let s =
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
  Omdx.of_string s |> Omdx.content |> Omdx.to_html |> print_string;
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
  let s =
    {|
# Heading 1

<CustomComponent className="text-white-500 bg-gray-900">
  Some text
</CustomComponent>
This is a nice and basic markdown document.
With a paragraph on 2 lines.

|}
  in
  Omdx.of_string s |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((metadata none)
     ((heading () 1 "Heading 1")
      (html-block CustomComponent ((className "text-white-500 bg-gray-900"))
       (paragraph () "Some text"))
      (paragraph ()
       (concat "This is a nice and basic markdown document." soft-break
        "With a paragraph on 2 lines."))))
    |}]
;;

let%expect_test "html of custom component block" =
  let s =
    {|
# Heading 1

<CustomComponent className="text-white-500 bg-gray-900">
  Some text
</CustomComponent>
This is a nice and basic markdown document.
With a paragraph on 2 lines.

|}
  in
  Omdx.of_string s |> Omdx.content |> Omdx.to_html |> print_string;
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
  let s =
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
  Omdx.of_string s |> Omdx.content |> Omdx.to_html |> print_string;
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
  let s =
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
  Omdx.of_string s |> Omdx.content |> Omdx.to_html |> print_string;
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

let%expect_test "document with metadata" =
  let s =
    {|---
title: Example Markdown Title
other_metadata: Some other metadata
data: true
---

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
  let doc = Omdx.of_string s in
  doc |> Omdx.content |> Omdx.to_html |> print_string;
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
  |}];
  doc |> Omdx.to_sexp |> print_string;
  [%expect
    {|
    ((metadata
      ((title "Example Markdown Title") (other_metadata "Some other metadata")
       (data true)))
     ((heading () 1 "This is the documentation for the example markdown file")
      (thematic-break ()) (heading () 2 "It's a basic test but hey it works!")
      (list () (bullet -) tight (list-item (paragraph () "ul 1"))
       (list-item (paragraph () "ul 2"))
       (list-item (paragraph () "ul 3")
        (list () (ordered 1 .) tight (list-item (paragraph () "ol 1"))
         (list-item (paragraph () "ol 2")) (list-item (paragraph () "ol 3"))))
       (list-item (paragraph () "ul 4")))
      (html-block CodeTabs ()
       (code-block () ocaml
        "we might even have something fun in here like this!\n"))))
    |}]
;;
