tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         tags$div(
           h3("2020 Landings Tracker"),
           h4(em("Weekly summary of West Coast commercial fisheries landings by species group and state")),
           p("This app was designed to provide about fisheries landings as data are reported."),
           p("Data are obtained from PacFIN and are summarized by week and species group and 
             presented as vessel-level mean/median, fleet-wide totals or as cumulative totals."),
           h4("Data details"),
           p("The states (California, Oregon, and Washington) provide landings data from paper fish tickets 
             and e-tickets to PacFIN. The timing when data becomes available varies by state as each 
             state has different regulations and procedures regarding fish tickets."),
           p("Based on our understanding of data completeness, we identify the most recent landing date 
             where data are complete. All data presented after this date are still shown, but with
             a higher level of uncertainty visualized with a dotted line. In general, we expect there
             to be a two-week lag (i.e., March landings would be complete by April 15)."),
           p("In order to protect confidentiality, we suppress data with too few observations. We use a 
             thin lined to indicate that the data point is suppressed, and may be larger or smaller
             than predicted with the dashed line."),
           p("More information about the fish ticket databases can be found here: https://pacfin.psmfc.org/.")
         )
)
