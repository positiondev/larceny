<bind tag="page-title">
  Fancy tricks
</bind>

<apply template="base">

  <section>
    <h2>Reverse</h2>
    <p>This is some text: <reverse>This is some text</reverse></p>

    <p>This is a buggy reverse: <buggyReverse>This is a <em>buggy</em> reverse</buggyReverse></p>

    <p>This is much better: <reverse>This is <strong><em>much</em> better</strong></reverse></p>
  </section>
</apply>
