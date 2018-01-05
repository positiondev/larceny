## When is Larceny the right choice?

Larceny is different from a lot of templating frameworks in that it keeps
programming out of the templates. In a lot of languages you can "drop down" into
the host language -- for example, in ERB templates, you can write Ruby in the
templates. Other templating engines like Jinja have their own embedded language
for conditionals and loops.

In Larceny, your ability to write code in the templates is very restricted. You
can only use what the backend provides, in terms of data as well as a structure.
What does this mean?

In another language, to display some content to only some users, you might write:

```
<div>
  % if @user.admin? %
    <p>Welcome, admin user!</p>
  % else %
    <p>Welcome, user!</p>
  % end %
</div>
```

But in Larceny, that logic has to be written in the backend and provided to the
frontend in the form of a "Substitution":

```
<div>
  <if condition="${isAdmin}">
    <then>
      <p>Wecome, admin user!</p>
    </then>

    <else>
      <p>Welcome user!</p>
    </else>
  </if>
</div>
```

Here, "isLoggedIn" is substituted by the backend for the word "True" or
"False", and the "if" Fill chooses which branch to display. So, the result is
the same as the previous example, but the logic is partly hidden in the backend.

This is a core restriction of how Larceny works. And in some ways, it's a good
thing. The lack of ability to program in the templates keeps the templates a
little more accessible to non-programmers.

Still, Larceny works best when:
  * one person or team is writing both templates and backened, or
  * there's a high degree of collaboration between backend and template writers

However, if you are working with a frontend developer who doesn't know Haskell
and who is used to writing conditionals and loops in the templates themselves,
they may find Larceny very frustrating. Consider using a different language,
such as [Ginger](https://hackage.haskell.org/package/ginger).
