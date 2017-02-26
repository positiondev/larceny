<bind tag="page-title">
   My favorite roller derby teams
</bind>

<apply template="base">
  <ul>
    <teams>
      <li>
        <h2><name/></h2>
        <p>Founded in: <foundedIn/></p>
        <p><shorten length="80" text="${longDesc}"/></p>
        <p><a href="team/${id}">Read more!</a></p>
      </li>
    </teams>
  </ul>
</apply>
