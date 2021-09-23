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
◊(define prev (previous here))
◊(define meta-here-path (select-from-metas 'here-path (current-metas)))
◊when/splice[prev]{Define with pagetree function works}
◊when/splice[meta-here-path]{Define with current-metas works}