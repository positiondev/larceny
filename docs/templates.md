# Using Larceny: Writing templates

Larceny templates look a lot like regular HTML, but you can imagine
that they have blank spaces that can be filled in with other
data.

Larceny also adds three special tags, `bind`, `apply`, and
`apply-content`.

## Template files

Template files are plain text files that end with the suffix `.tpl`.

## Blanks

"Blanks" are the spaces in your template that get filled in with text supplied
by your application backend.

Here is an example of a template with a blank:

```
  <div id="header"><myPageTitle /></div>
```

`<myPageTitle />` is a blank labeled `myPageTitle`.

You can also put blanks in attributes:

```
  <a href="profile/${userID}"><userName /></a>
```

In the above example, `${userID}` and `<userName />` are both blanks. The
`${blankName}` form is only used within attributes.

You can fill in blanks by writing [fills](fills) in Haskell.

If Larceny can't find a fill for some blank, it will just leave an empty space.
You'll see a warning message in your logs as well.

### Nested blanks

Just like regular HTML tags, blanks can contain other blanks.

Here is a blank that represents a user. It contains blanks that 
are filled in with the user's name and other information.

```
<user>
  Name: <userName />
  Age: <userAge />
</user>
```

### List blanks

Blanks can also represent lists of Fills.

Here's a blank for a list of users. Everything inside the 
`<users>` blank is repeated for each user.

```
<users>
  <user>
    Name: <userName />
    Age: <userAge />
  </user>
</users>
```

## Bind

You can use `<bind>` to create new fills directly in your
template. For example:

```
  <bind tag="pageTitle">This is the title of the page!</bind>

  <h1><pageTitle /></h1>
```

This may not seem super useful on it's own, but in combination with
`<apply>`, it's very handy!

## Apply

`<apply>` is how you can combine modular templates. For example, often
you'll want your site to have a header that's nearly the same on every
page.

```
   <div id="header">
     <h1>My website</h1>
   </div>
```

Call this "_header.tpl". The underscore indicates that you're not
going to serve this template directly, it's only going to be included
by other templates.

Here is how you use `<apply>` to include this template:

```
  <html>
    <body>
      <apply template="_header"></apply>

      <p>This is the rest of the page.</p>
    </body>
  </html>
```

The "template" attribute tells what template to include. You can also
use a path like "fragments/_header". You shouldn't add the ".tpl"
extension.

## Apply-Content

There's one more special tag in Larceny, `<apply-content>`. You use it
along with `<apply>`.

Let's look at again at the `<apply>` example. Except this time, we
want headers to be different on different pages. This means passing
information from your root template (the one your app wanted to
render) to the template fragments that the root template needs.
`<apply-content>` is one way to do that.

```
  <html>
    <body>
      <apply template="_header">The title of my page!</apply>

      <p>This is the rest of the page.</p>
    </body>
  </body>
```

The only difference between this template and the earlier one is that
in this one, the `<apply>` tag has some text inside it.

The "\_header" template looks different too:

```
  <div id="header">
    <apply-content />
  </div>
```

`<apply-content>` will put whatever text (or HTML) that was inside
the `<apply>` tag into the rendered page.

The rendered HTML will look something like this:

```
  <html>
    <body>
      <div id="header">
        The title of my page!
      </div>

      <p>This is the rest of the page.</p>
    </body>
  </body>
```

## Using `<bind>` and `<apply>` for a base template

Often web developers want to make a "base" template. The base template
includes the main page content, a header, and a footer (and any other
components you want). Here is how you do that in Larceny.

We'll use the "\_header" template from before, and a similar one called
"\_footer".

```
  <div id="footer">
    This is a footer.
  </div>
```


But now we'll also have a "\_base" template:

```
  <html>
    <body>
      <apply template="_header"><pageTitle /></apply>

      <apply-content />

      <apply template="_footer"></apply>
    </body>
  </body>
```

This looks a lot like the root template in our `<apply>` example
above, but it's got a couple key differences. The page title could be
different for every page. Since the base template doesn't know what
the title is going to be, it can't give that to the header template
directly. Inside of the `<apply>` tag for the header, it has another
Blank for the page title instead.

The root template looks like this:

```
<bind tag="pageTitle">The title of the page</bind>

<apply template="_base">

  This is the body of the page.

</apply>
```

This binds the `<pageTitle />` Blank to the title of the page. So,
when the base template is rendered, it has a Fill for that Blank.

The rendered result is this:

```
<html>
  <body>
    <div id="header">
      The title of the page
    </div>

    This is the body of the page.

    <div id="footer">
      This is a footer.
    </div>
  </body>
</html>
```
