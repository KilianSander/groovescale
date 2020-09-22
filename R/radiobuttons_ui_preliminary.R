above_ui <- # dieser Ansatz hat keinen guten Umbruch; Radiobuttons liegen hinter Text auf kleinen Screens
shiny::tags$div(
shiny::tags$head(
  htmltools::tags$style(htmltools::HTML("
            .shiny-input-radiogroup label {
                display: inline-block;
                text-align: center;
            }
            .shiny-input-radiogroup label input[type='radio'] {
                display: block;
                margin: 2em auto;
            }

        "))
  ),
shiny::radioButtons("item1",
                    "Diese Musik weckt in mir den Wunsch, mich zu bewegen.",
                    choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                    choiceValues = 1:7,
                    selected = 0,
                    inline = TRUE),
shiny::radioButtons("item2",
                    "Diese Musik ist gut zum Tanzen",
                    choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                    choiceValues = 1:7,
                    selected = 0,
                    inline = TRUE),
shiny::radioButtons("item3",
                    "Ich kann nicht stillsitzen, während ich diese Musik höre.",
                    choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                    choiceValues = 1:7,
                    selected = 0,
                    inline = TRUE),
shiny::radioButtons("item4",
                    "Diese Musik zu hören, macht mir Vergnügen.",
                    choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                    choiceValues = 1:7,
                    selected = 0,
                    inline = TRUE),
shiny::radioButtons("item5",
                    "Ich höre diese Musik gern.",
                    choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                    choiceValues = 1:7,
                    selected = 0,
                    inline = TRUE),
shiny::radioButtons("item6",
                    "Diese Musik gibt mir ein gutes Gefühl.",
                    choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                    choiceValues = 1:7,
                    selected = 0,
                    inline = TRUE)
)

side_ui <- # dieser Ansatz könnte gut gehen; man wird womöglich nach unten scrollen müssen, um mit Audio alle sechs Items sehen zu können
shiny::div(
  shiny::radioButtons("item1",
                      "Diese Musik weckt in mir den Wunsch, mich zu bewegen.",
                      choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                      choiceValues = 1:7,
                      selected = 0,
                      inline = TRUE),
  shiny::radioButtons("item2",
                      "Diese Musik ist gut zum Tanzen",
                      choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                      choiceValues = 1:7,
                      selected = 0,
                      inline = TRUE),
  shiny::radioButtons("item3",
                      "Ich kann nicht stillsitzen, während ich diese Musik höre.",
                      choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                      choiceValues = 1:7,
                      selected = 0,
                      inline = TRUE),
  shiny::radioButtons("item4",
                      "Diese Musik zu hören, macht mir Vergnügen.",
                      choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                      choiceValues = 1:7,
                      selected = 0,
                      inline = TRUE),
  shiny::radioButtons("item5",
                      "Ich höre diese Musik gern.",
                      choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                      choiceValues = 1:7,
                      selected = 0,
                      inline = TRUE),
  shiny::radioButtons("item6",
                      "Diese Musik gibt mir ein gutes Gefühl.",
                      choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                      choiceValues = 1:7,
                      selected = 0,
                      inline = TRUE)
)
