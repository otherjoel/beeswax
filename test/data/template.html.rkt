#lang beeswax/template

◊(require pollen/setup)

[BEESWAX]
◊->html[doc]
◊when/splice[#t]{True!}
Nothing:◊when/splice[#f]{False!}
◊here
◊current-pagetree[]
◊current-metas[]
◊current-project-root[]
◊(select-from-metas 'title metas)
◊(previous here)
◊(next here)
