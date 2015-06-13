<apply template="base">

  <ifLoggedIn>
    <apply template="_home"/>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
