# Conditionals

You can use the `if` tag for text that depends on certain conditions.

There are two options: `if` can test if the "condition" attribute ("True" or
"False") is "True", or it can test if the "exists" attribute contains non-empty
Text.

If the conditions provided are true, the `then` block will be filled in. If the
conditions are not true, then the `else` block will be filled in.

```
<if condition="True">
  <then>It's true!</then>
  <else>It's false!</else>
</if>
```

This code will always print "It's true!"

```
<if exists="some text">
   <then>It exists!</then>
   <else>It doesn't exist!</else>
</if>
```

This code will always print "It exists!"

You can also use exists to see if a list or nested tag is empty, in
combination with `bind`.

```
<bind tag="renderedList"><list><listItem /></list></bind>
<if exists="${renderedList}">
  <then>This list is not empty.</then>
  <else>This list is empty!</else>
</if>
```

This one will print "The list is not empty" if the `list` has items called
`listItem` with text content.

If `list` doesn't exist, or if `list` is empty, or if `list` uses some other
name for the items of the list, then it will print "This listis empty!"

