#lang beeswax/template

[BEESWAX]
◊->html[doc]
◊when/splice[#t]{True!}
Nothing:◊when/splice[#f]{False!}
◊here
◊current-pagetree[]
◊current-metas[]
◊(select-from-metas 'here-path metas)
◊(previous here)
◊(next here)
