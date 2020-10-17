itemtexts <- c("A","B","C")

ui <- do.call(tagList,
              lapply(1:length(itemtexts), function(i){
                shiny::tags$div(shiny::tags$b(itemtexts[i]),
                                shiny::radioButtons(paste0("item",i),"",choices = c(1,2,3,4,5,6), inline = TRUE, selected = 0))
                }
                )
              )

ui_rendered <- htmltools::renderTags(ui)

ui_final <- shiny::HTML(ui_rendered$html)


?lapply
?htmltools::renderTags()
?htmltools::doRenderTags()
