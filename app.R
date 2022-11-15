# Load the necessary packages----
library(shiny)
library(shinyjs)
library(shinyalert)
library(speechcollectr)

# User Interface code----
ui <- fluidPage(
  useShinyjs(),

  # Add an invisible clock that will run continuously
  # and keep the experiment from timing out and disconnecting.
  # Make this element fully transparent
  div(style = "color:rgba(0,0,0,0);",

      # It will need to be reactive (updated throughout the experiment)
      # so we'll make it an output value printed with the server code.
      textOutput("clock")),

  # Some general formatting to make sure everything is centered and at a reasonable width----
  fluidRow(

    ## Note: Width and offset are defined in columns, and there are 12 columns in a row----
    column(width = 8,
           offset = 2,

           # Each portion of the experiment is contained in a separate tab, but the tab titles are invisible to participants----
           tabsetPanel(id = "tabs", selected = "entry",

                       ## Make the first tab the title page, clearly stating what the experiment is----
                       tabPanelBody(value = "entry",
                                    div(id = "title", style = "text-align:center",
                                        HTML("<h2><em>Welcome to...</em><h2>"),
                                        h1("Emotional Voices:"),
                                        h3("A Study of Emotional Ways of Speaking"),
                                        br(),
                                        br(),
                                        actionButton(inputId = "enter",
                                                     label = "Enter Study"))
                       ),

                       ## Tab 2: Consent----
                       tabPanelBody(value = "consent",
                                    consentUI(title = "Do you consent to participate?")),

                       # Tab 3: PIN----
                       tabPanelBody(value = "show_pin",
                                    div(id = "pinDiv", style = "text-align:center;",
                                        h3("If you had previously begun the experiment and were already assigned an 4-digit ID number, please enter that number here; then click 'Resume'. You will need to complete the practice recording & equipment check again, then you may continue from the recording you made last."),
                                        textInput(inputId = "resume_pin",
                                                  label = "Your ID Number:"),
                                        actionButton("check_pin", "Resume"),
                                        br(),
                                        h3("Otherwise, click 'Begin' to receive a new ID number and begin the experiment."),
                                        actionButton("new_pin", "Begin")),
                                    uiOutput("pin_ui")
                       ),

                       ## Tab 4: Practice recording----
                       tabPanelBody(value = "practice",
                                    div(id = "practiceDiv", style = "text-align:center",
                                        h2("Let's check to see if your equipment will work for this experiment."),

                                        # The addition of the HTML tag "br()" makes the script look messy,
                                        # but results in a nicer UI by putting empty lines between elements.
                                        br(),
                                        h5("Make a practice recording by clicking the 'START' button below."),
                                        h5("After you click 'START', read the word that appears on the screen."),
                                        br(),

                                        # The tag "hr()" will add a thin horizontal line between elements.
                                        hr(),

                                        br(),

                                        # Make a placeholder for the recording interface----
                                        recordUI(id = "practice_rec"),
                                        br(),
                                        br(),
                                        h5("You may re-do this recording as many times as you want. When you are satisfied with your recording, click 'SUBMIT RECORDING' below."),
                                        br(),
                                        disabled(actionButton("submit_pract", "SUBMIT RECORDING")))
                       ),

                       ## Tab 5: Demographic survey----
                       # Remember to use a separate script to prepare 'www/survey.csv' and put it in the www folder
                       tabPanelBody(value = "background",
                                    surveyUI(id = "survey",
                                             questionFile = "www/survey.csv",
                                             title = "Background Information",
                                             notListedLab = "Not listed:")
                       ),

                       ## Tab 6: Experiment instructions----
                       tabPanelBody(value = "instructions",
                                    div(id = "instructDiv", style = "text-align:center",
                                        br(),
                                        h2("Let's Play a Game!"),
                                        br(),
                                        h4("First, some background: Let's say your local radio station
                      is looking for some fresh talent. Specifically, they need
                      some voice actors, like you, who can speak convincingly with
                      a variety of different emotions."),
                                        h4("Today, you'll be recording some auditions for the radio
                      station. You'll have a chance to record a set of 24 words in
                      4 different emotional tones of voice. "),
                                        h4("BUT...To earn your time in the recording study, you'll need
                      to use your memory and concentration skills (great for
                      memorizing lines in a script!) to find pairs matching buttons
                      on a grid. Each pair of matching buttons will earn you a
                      chance to record a word."),
                                        br(),
                                        h4("Click BEGIN when you are ready to begin the matching
                      game."),
                                        actionButton("begin", "BEGIN"))
                       ),

                       ## Tab 6: The matching game interface----
                       tabPanelBody(value = "game",
                                    matchUI(instructions = "Click a button in the grid to see the image it
                     is hiding. Keep clicking buttons until you find the image that
                     matches the one above. Each match will earn you a chance to
                     record a word. Once you arrive at the recording studio, pay
                     careful attention to emotion you're supposed to use to read
                     the word that appears when you click 'START'."),
                                    hidden(actionButton("rec_trig", ""))
                       ),

                       ## Tab 7: Experimental trial recording interface----
                       tabPanelBody("recording",
                                    br(),
                                    br(),

                                    # We'll use the server code to tell the participant which emotion they should use
                                    hidden(div(id = "emo_div", style = "text-align:center;",
                                               uiOutput("emo_ui"))),
                                    br(),
                                    hr(),
                                    br(),

                                    # Note that the "startId", "stopId", and "id" argument values
                                    # MUST all match those given in the corresponding "recordServer()" call.
                                    div(id = "recDiv",
                                        style = "text-align:center; background-color:#D3D3D3",
                                        recordUI(id = "rec"),
                                        br(),

                                        # We'll show the submit button in the server code.
                                        hidden(actionButton("submit_rec", "SUBMIT RECORDING")))
                       )
           )
    )
  )
)

# Server program----
server <- function(input, output, session) {
  # Create a reactive values object that holds the trial number
  # Eventually, the participant's PIN and their set of randomized stimuli
  # will also be stored in this object.
  # All these values can be created/accessed by prefixing the value name with "counter$"
  counter <- reactiveValues(n = 1)

  # Set up the matching game. This function must always be used at the top level of the server code.
  # (i.e., NOT within a call to observe or observeEvent)
  # We'll use a grid of 20 icons, one for each of the 20 items.
  # This is also useful for observing an increase in speed
  # if the participant is paying attention as they repeat matches.
  found <- matchServer(triggerInit = reactive(input$begin),
                       triggerReturn = reactive(input$submit_rec),
                       n2find = 20,
                       n_items = 20,
                       n_cols = 4,
                       randomGrid = TRUE,
                       lab_type = "icon",
                       result = "hide")

  # When a participant clicks "enter"...
  observeEvent(input$enter, {

    ## Send them to the consent form----
    updateTabsetPanel(session, "tabs", selected = "consent")
    consent <- consentServer(delayResponse = 2000,
                             cons2rec = TRUE,
                             result = "hide",
                             disagreeLab = "Disagree")

    ## When consent is obtained, move to the practice recording----
    ## Note: the consent server function will return the reactive value "consent"
    ## Reactive values are like events or functions, and must be followed by "()"
    ## unless contained in an object created with "shiny::reactiveValues()"
    observeEvent(consent(), {
      updateTabsetPanel(session, "tabs", selected = "show_pin")
    })
  })

  # If the participant is new...
  observeEvent(input$new_pin, {
    hide("pinDiv")

    # Now we create a new pin and store it in the reactive "counter" object
    # so that it will be available outside of this "observeEvent()" command
    counter$pin <- pinGen(reactive = FALSE)

    # And save a randomized stimuli set with this pin number.
    trials <- randomStim(dataFile = "www/stimuli_short.csv",
                         blockCol = "block",
                         blocksSameOrd = FALSE,
                         outFile = paste0("www/outputs/stimuli", counter$pin, ".rds"))
    # Then put the randomized stimuli in the reactive value object
    counter$stimuli <- trials

    # Show the PIN to the participant
    output$pin_ui <- renderUI({
      div(id = "pinDiv2", style = "text-align:center;",
          br(),
          h3("Welcome! Your ID number is: "),
          br(),
          h1(counter$pin),
          br(),
          h3("Please make a note of this number. You may use it to resume the experiment if necessary."),
          h3(em("NOTE: We strongly recommend completing the experiment in one sitting.")),
          actionButton("pin_done", "Click here to Continue.")
      )
    })
  })

  # If the participant is returning...
  observeEvent(input$check_pin, {

    # check to see whether the PIN they entered matches a demographic survey we have already saved
    if (file.exists(paste0("www/outputs/demographics", input$resume_pin, ".rds"))) {

      hide("pinDiv")

      # If the PIN they entered does match data we have stored, put it in the reactive "counter" object
      # so that it will be accessible throughout the experiment
      counter$pin <- as.numeric(input$resume_pin)

      if (file.exists(paste0("www/outputs/completed", counter$pin, ".rds"))) {
        # Change the trial number to the number of the last completed trial
        # if and only if they made it past the first trial
        counter$n <- readRDS(paste0("www/outputs/completed", counter$pin, ".rds")) + 1
      }

      # Read in the randomized set of stimuli from the participant's session

      # And store the stimuli in the reactive object
      counter$stimuli <- readRDS(paste0("www/outputs/stimuli", counter$pin, ".rds"))

      # They will have to check their equipment again, as they might have changed devices.
      # After the practice recording, the participant will resume the experiment from the last completed trial.
      output$pin_ui <- renderUI({
        div(id = "pinDiv2", style = "text-align:center;",
            br(),
            h1("Welcome back!"),
            br(),
            h3("You'll need to complete the microphone check again before resuming the experiment. After you complete the check, you'll go straight to the last trial you completed and proceed from there."),
            actionButton("pin_done", "Click here to Continue.")
        )
      })
    } else {
      # If no data exists for the PIN the participant has entered, send them back to get a new PIN.
      shinyalert(title = "Invalid ID Number",
                 text = "We do not have any data associated with the ID number you
               entered. Please begin the experiment again by requesting a new ID number.",
                 type = "warning",
                 confirmButtonText = "OKAY")
    }
  })

  # Once a new participant has a PIN, move them to the practice recording.
  observeEvent(input$pin_done, {
    updateTabsetPanel(session, "tabs", selected = "practice")

    counter$practice <- recordServer(id = "practice_rec",
                                     trigger = NULL,
                                     writtenStim = c("pineapple"),
                                     writtenDelay = 500,
                                     outPrefix = paste0("www/outputs/practice", counter$pin))
  })

  observeEvent(input[["practice_rec-stop"]], {

    # When the file has been saved, allow the participant to click "submit" to move to the next step
    # Though the recording has already been submitted, a click on this button
    # signals the participant's readiness to move to the next step.
    enable("submit_pract")
  })

  # Once a participant submits a recording, evaluate the SNR, clipping, and sample rate.
  # Return the result of the evaluation as a reactive value "feedback()"
  observeEvent(input$submit_pract, {
    feedback <- evalWavServer(wave = counter$practice()$file,
                              counter = counter$practice()$n,
                              min_sf = 44100,
                              snr_best = 15, snr_good = 10,
                              max_clip = .01, tries = 3,
                              onFail = "stop")
    observe({
      if (req(feedback()) == "pass") {
        if (!file.exists(paste0("www/outputs/demographics", counter$pin, ".rds"))) {
          updateTabsetPanel(session, "tabs", selected = "background")

          surveyServer(id = "survey",
                       questionFile = "www/survey.csv",
                       result = "hide",
                       outFile = paste0("www/outputs/demographics", counter$pin, ".rds"),
                       notListedLab = "Not listed:")
        } else {
          updateTabsetPanel(session, "tabs", selected = "game")
          click("submit_rec")
        }
      }
    })
  })


  # Once the participant completes the demographic survey,
  # send them to the instructions for the experiment.
  # Note that for all speechcollectr module functions, the submit button ID will be "submit"
  # prefixed by the value of the "id" argument of the module + a hyphen.
  # To access these buttons, we'll need to treat the submit button id as a character string name/index
  # of an item in the list of inputs.
  # (This is standard practice for accessing inputs created inside other shiny modules,
  # besides those included in speechcollectr)
  observeEvent(input[["survey-submit"]], {
    updateTabsetPanel(session, "tabs", selected = "instructions")
  })

  # Move participants to the matching game after they read the instructions
  observeEvent(input$begin, {
    updateTabsetPanel(session, "tabs", selected = "game")
  })

  observe({
    # Any time the number of matches found matches the trial number,
    # move to the recording tab after a brief delay.
    # Automatically click the invisible button with the ID "rec_trig"
    if (req(found()$correct) == 1) {
      delay(1000, updateTabsetPanel(session, "tabs", selected = "recording"))
      delay(1000, click("rec_trig"))
      showElement("emo_div")
    }
  })
  # Actions for the recording tab.
  # Make the instructions appear based on the click of the invisible button...
  observeEvent(input$rec_trig, {

    # By adding the UI elements that tell participants which emotion to use when reading
    # to the server code, we ensure they can be updated in response to the trial number.
    # This wouldn't have been possible if we had put these elements inside the main UI object.
    output$emo_ui <- renderUI({
      tagList(
        HTML(paste0("<h4> Read the word that appears below in a(n)
                  <span style='font-size:32px;'><em>",
                    counter$stimuli$emotion[counter$n],
                    "</em></span> tone of voice.</h4>")),
        br(),

        # make the class of the icon Font Awesome (fa-) at 5x normal text size.
        span(
          icon(counter$stimuli$icon[counter$n], class = "fa-5x"),
          style="color:#AAAAAA")
      )
    })

    # No need to place "recordServer()" inside an observeEvent,
    # when using a non-null trigger argument
    recordServer(id = "rec",
                 #trigger = reactive(input$rec_trig),
                 writtenStim = paste0(counter$stimuli$word[counter$n]),
                 writtenDelay = 500,
                 outPrefix = (paste0("www/outputs/", counter$stimuli$word[counter$n],
                                     counter$pin, "_", counter$stimuli$block[counter$n], "_",
                                     counter$stimuli$trial[counter$n])))
  })



  # Note that the word that will appear for the participant to read
  # and the prefix of the returned wav file are named dynamically
  # based on reactive values, so they will be different for each participant/trial

  # Actions for when a participant completes a recording.
  # The ID for the stop button from the recording module is accessed by appending "-stop" to the module's id.
  observeEvent(input[["rec-stop"]], {
    showElement("submit_rec")
    hide("emo_div")
    hide("rec")
  })

  # Actions for when a participant submits the recording.
  observeEvent(input$submit_rec, {
    hide("submit_rec")

    # save a record of the most recent trial completed
    saveRDS(counter$n-1, paste0("www/outputs/completed", counter$pin, ".rds"))

    # This portion of the "if" statement defines what happens if the participant has NOT completed the last trial
    if (counter$n-1 < nrow(counter$stimuli)) {
      #...when they complete a block, give an alert to allow the participant time for a break
      if (counter$stimuli$block[counter$n-1] != counter$stimuli$block[counter$n]) {
        delay(1500, showElement("rec"))
        hide("emo_ui")
        delay(1500, showElement("emo_ui"))

        shinyalert(title = "Block Complete!",
                   text = paste0("Great job! You've completed ",
                                 counter$stimuli$block[counter$n-1],
                                 "  out of ", max(counter$stimuli$block),
                                 " blocks of 24 trials each. When you are ready,
                               click the button below to move on to the next
                               block. Be careful! You'll be reading these next
                               items with a different emotion."),
                   confirmButtonText = "Begin Next Block")
      } else {
        # If they haven't reached the end of the block, just go to the next trial
        delay(1500, showElement("rec"))
      }

      # Return to the game tab
      updateTabsetPanel(session, "tabs", selected = "game")

    } else {
      # When they finish the last trial, show an alert that tells the participant to close the browser window.
      shinyalert(title = "Experiment Complete!",
                 text = "You have completed the experiment. All recordings have been
               saved. Thank you for your participation. Please close this browser
               window now.",
                 showConfirmButton = FALSE)
    }
  })
}
shinyApp(ui = ui, server = server)
