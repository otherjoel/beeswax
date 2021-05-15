#lang beeswax/template

<html>
  <title>[BEESWAX was here]</title>
  <h1>◊(hash-ref metas 'title)</h1>
  ◊(->html doc)
</html>
