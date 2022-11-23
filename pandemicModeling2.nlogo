breed [civilians civilian]
breed [cars car]
breed [taxis taxi]
breed [busses bus]

; -------------------------------------------------------------------
; VARIABLES (INSTANCE AND GLOBAL)
; -------------------------------------------------------------------

globals
[
  ;agent_colors ; list of possible agent colors
  unemployed ; count of the number of unemployed civilians
  max_aptgroup ;
  max_workgroup ;
]

patches-own
[
  state ; undeveloped, house, street, or workplace

  ; building variables
  stories ; number of stories on a building or house patch
  residents ; array containing the number of residents on each floor
  building_no ; identifier for the building
  res_perFloor ; number of allowed residents per building floor

  building_group ; used for grouping buildings for bus visiting
  visited? ; used for making building groups
]

civilians-own
[
  infection_status ; the infection status of the civilian
  ; 0 = safe, 1 = exposed, 2 = infected, , 3 = positive
  ; 4 = post-infection immune, 5 = dead
  day_of_exposure ; day of exposure

  apt_no ; apartment building id for where the civilian lives
  apt_floor ; which floor the civilian lives on
  apt_spec ; which room the civilian lives in

  workplace_no ; building id for where the civilian works
  workplace_floor ; which floor the civilian works on
  workplace_spec ; which office the civilian works in

  homexy ; xy coordinates for where the civilian goes when at home
  workxy ; xy coordinates for where the civilian goes when at work

  current_location ; home or work
  tm ; transportation method: car, taxi, bus, (train?)
]

taxis-own
[
  infection_status ; infection status of the taxi - can be safe, exposed, or infected: once positive the driver is replaced by a safe driver
  day_of_exposure ; the day the taxi driver was exposed to the virus
  passenger ; stores the passenger of the taxi
]

busses-own
[
  passengers ; list of current passengers
  infection_status ; infection status of the bus driver
]

; -------------------------------------------------------------------
; SETUP
; -------------------------------------------------------------------

; import a saved world
to import
  ; need to save some variables so they arent overriden by the import (necessary for using behavior space simulation)
  let saved list (public_transportation_usage) (taxi>bus) ; for the time being these are the only variables we are changing in the experiments
  import-world (word "model" mnum ".csv")
  set public_transportation_usage (item 0 saved)
  set taxi>bus (item 1 saved)

  ; override
  if taxi>bus < 100 and bus_count = 0
  [
    set bus_count 3
  ]
  if taxi>bus > 0 and taxi_count = 0
  [
    set bus_count 10
  ]

end

; setup function: make a new world and set all patches to undeveloped
to setup
  ; clear all
  ca
  reset-ticks

  ; make a new world according to the specified dimensions
  resize-world (-1 * (world_size / 2)) (world_size / 2) (-1 * (world_size / 2)) (world_size / 2)
  set-patch-size patch_size
  ask patches [
    set state "undev"
    set pcolor resetcolor
    set stories -1 ; no stories on grass
  ]
end

; visual aesthetics for development (highlighting patches the mouse is hovering over)
to develop-mode
  ifelse mouse-inside?
  [
    ask patch mouse-xcor mouse-ycor
    [
      ; highlight the patch the mouse is hovering over
      ask other patches [set pcolor resetcolor]
      set pcolor (resetcolor + 1)
    ]
  ]
  [
    ask patches [
      ; reset the color to the base color of the patch if the mouse isnt hovering over it
      set pcolor resetcolor
    ]
  ]
end

; get the standard color for each patch-type
to-report resetcolor
  if state = "workplace" [report 3 - (stories / 3)]
  if state = "home" [report 86 - (stories / 2)]
  if state = "street" [report 5]
  if state = "undev" [report 68]
end

; make a patch a new type (as specified by str)
to make [str]
  develop-mode; highlight the patche the mouse is over
  while [mouse-down?][
    update-plots ; update the graphs
    ; change the state of the selected patch
    ask patch mouse-xcor mouse-ycor
    [
      set state str ; set the patch state accordingly
      ifelse str = "home" or str = "workplace"
      [
        set building_no count patches with [state = str] ; set the building identifier
        set stories building-height ; set the number of stories
        set plabel building_no ; set the label
        set plabel-color white ; color the label
        set res_perFloor per-floor ; set the number of residents per floor
      ]
      [ ; if its a street patch
        set stories -1 ; no stories
        set plabel "" ; and no labels
      ]
      develop-mode ; update patch colors according to mouse highlight
    ]
  ]
end

; generate: create the agents (people, taxis) and assign their properties
to generate
  print "-----------------" ; provide a buffer in the console output
  set day 1 ; reset the day counter
  set time "morning" ; set the time to morning

  print word "public transportation_usage " public_transportation_usage
  print word "taxi>bus " taxi>bus

  clear_prev ; clear the previous model
  setup-buildings ; setup function for buildings
  generate-civilians ; make all the civilians
  generate-vehicles ; make the vehicles
  initial-exposure ; expose a certain number of civilians
end

; clear the previous model
to clear_prev
  ask turtles [die] ; ask all the agents to die
  clear-all-plots ; clear all the plots
end

; create an empty list of the number of residents per floor for each building
to setup-buildings
  set max_aptgroup 0
  set max_workgroup 0
  ask patches with [state = "home" or state = "workplace"]
  [
    set residents list 0 0
    repeat 2 [set residents remove-item 0 residents]
    let i 0
    while [i < stories]
    [
      set residents lput 0 residents
      set i i + 1
    ]
  ]
end

; create the cars and taxis for the world
to generate-vehicles
  ; create the taxis
  create-taxis taxi_count
  [
    set shape "van top"
    set color white
    set infection_status "safe"
    set size .5
    set heading 0
    set passenger nobody
    carefully[ move-to one-of patches with [state = "street"] ]
    [ print "ERROR: no streets created..." ]
  ]
  ; create the busses
  create-busses bus_count
  [
    set shape "bus"
    set color white
    set infection_status "safe"
    set size .5
    set heading 0
    set passengers list nobody nobody
    repeat 2 [ set passengers remove-item 0 passengers ]
  ]
  ; if there are busses, group the buildings for pickup and dropoff
  if bus_count > 0
  [
    ask patches with [state = "home" or state = "workplace"]
    [ set visited? false ]
    group_buildings
  ]
end

; group the buildings for bus pickup spots
to group_buildings
  ; group the home buildings
  while [any? patches with [state = "home" and visited? = false]]
  [
    ask one-of patches with [state = "home" and visited? = false]
    [
      group_buildings_helper self "home"
      set max_aptgroup max_aptgroup + 1
    ]
  ]
  ; group the work buildings
  while [any? patches with [state = "workplace" and visited? = false]]
  [
    ask one-of patches with [state = "workplace" and visited? = false]
    [
      group_buildings_helper self "work"
      set max_workgroup max_workgroup + 1
    ]
  ]
end

; helper function recursively assigns building group numbers
to group_buildings_helper [buildingP HoW] ; pass in the building patch and the state
  ask buildingP
  [
    set visited? true
    ifelse hoW = "home"
    [
      set building_group max_aptgroup
    ]
    [
      set building_group max_workgroup
    ]
  ]
  if any? neighbors with [state = HoW and visited? = false]
  [
    ask neighbors with [state = HoW and visited? = false]
    [
      group_buildings_helper self HoW
    ]
  ]
end

; create the people
to generate-civilians
  let civilian_count 0 ; reset the civilian count
  set unemployed 0 ; reset the unemployed count
  ; set number of civilians each apartment building houses
  carefully
  [
    ask patches with [state = "home"]
    [ set civilian_count (civilian_count + stories * res_perFloor) ]
  ]
  [ print "ERROR: no homes created..." ]
  ; create the civilians
  create-civilians civilian_count
  [
    set size patch-size / 100
    set shape "person"
    set color white
    set infection_status "safe"
    assign-home ; assign a home
    assign-work ; assign a place of work
  ]
  print word "total civilian count: " civilian_count
  print (word unemployed " citizens are unemployed")

  ; set the home and work locations of civilians
  ask civilians
  [
    set homexy place-civilian xcor ycor (apt_spec) ((item apt_floor [residents] of patch-here)) (apt_floor) (length [residents] of patch-here) ; set the xy coordinate for when the civilian is home
    let wrkn workplace_no ; temp varaible
    let workplace (one-of patches with [state = "workplace" and building_no = wrkn]) ; temp variable
    set workxy place-civilian ([pxcor] of workplace) ([pycor] of workplace) (workplace_spec) (item workplace_floor [residents] of workplace) (workplace_floor) (length [residents] of workplace) ; set the xy coordinate for when the civilian is at work

    ; put the civilian at home
    setxy (item 0 homexy) (item 1 homexy)
    set current_location "home"
  ]
  choose-transportation
end

; assign an open apartment space to the civilian
to assign-home
  ask one-of patches with [state = "home" and hasSpace res_perFloor] ;
    [
      let floorSel find-floor res_perFloor
      ask myself [
        set apt_no [building_no] of myself
        set apt_floor item 0 floorSel
        set apt_spec item 1 floorSel
      ]
  ]
  move-to one-of patches with [state = "home" and building_no = [apt_no] of myself]
end

; assign an open work space to the civilian
to assign-work
  carefully
  [
    ask one-of patches with [state = "workplace" and hasSpace res_perFloor]
    [
      let floorSel find-floor res_perFloor
      ask myself [
        set workplace_no [building_no] of myself
        set workplace_floor item 0 floorSel
        set workplace_spec item 1 floorSel
      ]
    ]
  ]
  [ ; there are no available work spaces
    set workplace_no -1
    set unemployed unemployed + 1
  ]

end

; return a position for where a civilian should be places in a building- basexy = xycor of the building patch, xpos & xtot are the horizontal space the civilian is on on the floor and the total spaces on that floor,
; ypos & ytot correspond with the floor number and total number of floors in a building
to-report place-civilian [ basex basey xpos xtot ypos ytot ]
  report list ((basex - .5 + (1 / (2 * xtot)) + (xpos / xtot))) (basey - .5 + (1 / (2 * ytot)) + (ypos / ytot))
end

; find a floor to place the civilian on
to-report find-floor [perFloor]
  let i 0
  while [i < stories]
  [
    if item i residents < perFloor
    [
      set residents replace-item i residents (item i residents + 1)
      report list i (item i residents - 1)
    ]
    set i i + 1
  ]
end

; reports whether the building has space for another resident
to-report hasSpace [perFloor]
  let i 0
  while [i < stories]
  [
    if item i residents < perFloor
    [ report true ]
    set i i + 1
  ]
  report false
end

; initially expose civilians
to initial-exposure
  let num_exposed round(initially_expose / 100 * count civilians)
  print word num_exposed " civilians were initially exposed"
  ask n-of num_exposed civilians
  [
    set infection_status "exposed"
    set day_of_exposure day
  ]
  update-infection
end

; -------------------------------------------------------------------
; ACTIVE SIMULATION
; -------------------------------------------------------------------

; simulations
to simulate
  ; change time if all civilians have been moved
  if time = "morning" and (not any? civilians with [current_location = "home" and not (infection_status = "positive") and not (infection_status = "dead")])
  [
    set time "at work"
    spread-infection
    choose-transportation
    wait sim_delay
  ]
  if time = "evening" and not any? civilians with [current_location = "work"]
  [
    set time "morning"
    set day day + 1
    tick ; update the graphs
    if (day - 1) mod 10 = 0
    [
      print word (day - 1) " day statistics: "
      print word "deaths: " count civilians with [infection_status = "dead"]
    ]
    choose-transportation
    spread-infection
    wait sim_delay
  ]

  wait sim_delay
  car-behavior ; simualte private car behavior
  taxi-behavior ; simulate taxi behavior
  bus-behavior ; simulate bus behavior
end

to choose-transportation
  ; set the transportation method of civilians
  ask civilians
  [
    ifelse 1 + random 99 < public_transportation_usage ; use public transportation
    [
      ifelse 1 + random 99 < taxi>bus
      [ set tm "taxi" ]
      [ set tm "bus" ]
    ]
    [ set tm "car" ]
  ]
end

; spread the infection from infected civilians to nearby civilians
to spread-infection
  ifelse time = "morning" ; in the morning: update the infection and transmit the virus to people living on the same floor
  [
    update-infection
    ; spread to nearby home
    ask civilians with [infection_status = "infected"]
    [
      transmit_to (civilians with [apt_no = [apt_no] of myself and apt_floor = [apt_floor] of myself])
    ]
  ]
  [
    if time = "at work" ; at work: transmit the virus to people working on the same floor
    [
      ask civilians with [infection_status = "infected"]
      [
        transmit_to (civilians with [workplace_no = [workplace_no] of myself and workplace_floor = [workplace_floor] of myself])
      ]
      set time "evening" ; set time to evening
    ]
  ]
end

; transmit the virus to a person
to transmit_to [dest]
  ask dest
  [
    if (infection_status = "safe" or infection_status = "exposed") and 1 + random 99 < exposure_rate
    [
      set infection_status "exposed"
      set day_of_exposure day
    ]
  ]
end

; update the infection for each agent
to update-infection
  ask civilians
  [ update-infection-helper ]
  ask taxis
  [ update-infection-helper ]
end

; update the infection in each civilian or taxi
to update-infection-helper
  if infection_status = "safe"
  [ set color white ]
  ifelse infection_status = "exposed"
  [
    set color 25
    ; determine whether the civilian is actually infected: they can be positive after a minimum 2 days of exposure
    if day >= day_of_exposure + 2
    [
      if 1 + random 99 <= transmission_rate
      [
        set infection_status "infected"
        set color 15
      ]
    ]

    if day > day_of_exposure + 7 ; the civilian was exposed but not infected
    [ set infection_status "safe" ]
  ]
  [
    if infection_status = "infected"
    [
      if 1 + random 99 < testing_rate ; if the agent takes an accurate test
      [
        set infection_status "positive"
        set color green
      ]

      if 1 + random-float 99 <= death_rate ; chance the person dies
      [
        set infection_status "dead"
        set color black
      ]
      if day >= day_of_exposure + 5 ; recovery is possible 5-14 days after exposure
      [
        if day > day_of_exposure + 14 ; the civilian has recovered
        [
          set infection_status "immune"
          set color yellow
        ]
        if 1 + random 99 <= recovery_rate ; chance the civilian will recover
        [
          set infection_status "immune"
          set color yellow
        ]
      ]
    ]
    if infection_status = "positive"
    [
      if 1 + random-float 99 <= death_rate ; chance the person dies
      [
        set infection_status "dead"
        set color black
      ]
      if day >= day_of_exposure + 5 ; 5 - 14 days of positive testing
      [
        if day > day_of_exposure + 14 ; the civilian has recovered
        [
          set infection_status "immune"
          set color yellow
        ]
        if 1 + random 99 <= recovery_rate ; chance the civilian will recover
        [
          set infection_status "immune"
          set color yellow
        ]
      ]
    ]
  ]
end

; ask a civilian to enter a vehicle
to get-in
  ask myself [set passenger self]
end

; ask the civilian to get out of a vehicle
to get-out
  ask passenger
  [
    ifelse current_location = "home"; if at home, move to work
    [
      let wpn workplace_no
      ;move-to one-of patches with [state = "workplace" and building_no = wpn]
      setxy (item 0 workxy) (item 1 workxy)
      set current_location "work"
    ]
    [ ; if at work, move to home
      let an apt_no
      ;move-to one-of patches with [state = "home" and building_no = an]
      setxy (item 0 homexy) (item 1 homexy)
      set current_location "home"
    ]
  ]
  set passenger nobody
end

; direct the cars
to car-behavior

  ifelse time = "morning"
  [
    ask civilians with [current_location = "home" and not (infection_status = "positive") and not (infection_status = "dead") and tm = "car"]
    [
      ; civilians hatch their own cars which take them to their destinations
      let civ self
      hatch-cars 1
      [
        set shape "car"
        set color blue
        move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "home" and building_no = [apt_no] of civ]]
        ask civ [ ht ]
        move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "workplace" and building_no = [workplace_no] of civ]]
        ask civ [ st ]
        die
      ]
      setxy (item 0 workxy) (item 1 workxy)
      set current_location "work"
    ]
  ]
  ; evening
  [
    ask civilians with [current_location = "work" and not (infection_status = "positive") and not (infection_status = "dead") and tm = "car"]
    [
      ; civilians hatch their own cars to take them to their destinations
      let civ self
      hatch-cars 1
      [
        set shape "car"
        set color blue
        move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "workplace" and building_no = [workplace_no] of civ]]
        ask civ [ ht ]
        move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "home" and building_no = [apt_no] of civ]]
        ask civ [ st ]
        die
      ]
      setxy (item 0 homexy) (item 1 homexy)
      set current_location "home"
    ]
  ]

end

; direct taxis
to taxi-behavior
  ask taxis
  [
    if infection_status = "positive" or infection_status = "dead"; if the taxidriver is known to be compromised, replace him with another
    [
      set infection_status "safe"
      set color white
    ]
    ifelse time = "morning" ; in the morning take people to work
    [
      if any? civilians with [current_location = "home" and not (infection_status = "positive") and not (infection_status = "dead") and tm = "taxi"] ; if there are still civilians to be moved
      [
        ifelse passenger = nobody ; pick up a new passenger
        [
          let civilian_sel (one-of civilians with [current_location = "home" and workplace_no != -1 and not (infection_status = "positive") and not (infection_status = "dead") and tm = "taxi"])
          move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "home" and building_no = [apt_no] of civilian_sel]]
          ask civilian_sel [ get-in ht ]
          set passenger civilian_sel
        ]
        [ ; spread the virus and take the passenger to their new destination
          if [infection_status] of ([passenger] of self) = "infected" ; spread the virus from passenger to taxi driver
          [ transmit_to self ]
          if infection_status = "infected" ; spread the virus from the taxi driver to the passenger
          [ transmit_to passenger ]

          ; take the passenger to work
          let p [passenger] of self
          ask p [ st ]
          move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "workplace" and building_no = [workplace_no] of p]]
          get-out
        ]
      ]
    ]
    [
      if time = "evening" ; in the evening, take people home
      [
        if any? civilians with [current_location = "work" and tm = "taxi"] ; if there are any civilians to be moved
        [
          ifelse passenger = nobody ; pick up a new passenger
          [
            let civilian_sel (one-of civilians with [current_location = "work" and tm = "taxi"])
            move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "workplace" and building_no = [workplace_no] of civilian_sel]]
            ask civilian_sel [ get-in ht ]
            set passenger civilian_sel
          ]
          [ ; spread the virus and take the passenger to their new destination
            if [infection_status] of ([passenger] of self) = "infected" ; spread the virus from the passenger to the taxi driver
            [ transmit_to self ]
            if infection_status = "infected" ; spread the virus from the taxi driver to the passenger
            [ transmit_to passenger]

            ; take the passenger home
            let p [passenger] of self
            ask p [ st ]
            move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "home" and building_no = [apt_no] of p]]
            get-out
          ]
        ]
      ]
    ]
  ]
end

; determine whether there are any more passengers on a given block of houses or workplaces that match the selected civilian who need to be picked up by the bus
to-report more_passengers? [civilian_sel HoW]

  report (any? civilians with [current_location = HoW and workplace_no != -1 and not (infection_status = "positive") and not (infection_status = "dead") and tm = "bus" and [building_group] of (one-of patches with [building_no = [apt_no] of myself]) = [building_group] of (one-of patches with [building_no = [apt_no] of civilian_sel])])

end

; direct busses
to bus-behavior
  ask busses
  [
    ifelse time = "morning" ; in the morning take people to work
    [
      ifelse length passengers = 0 and any? civilians with [current_location = "home" and not (infection_status = "positive") and not (infection_status = "dead") and tm = "bus"] ; if there are still any civilians to be moved by the bus
      [ ; pickup
        ; select a civilian to pick up and then pick up all civilians on the same block who need transport
        let civilian_sel (one-of civilians with [current_location = "home" and workplace_no != -1 and not (infection_status = "positive") and not (infection_status = "dead") and tm = "bus"])
        ask civilian_sel [geton_bus myself]
        move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "home" and building_no = [apt_no] of civilian_sel]]

        ; while any passengers in the building group still need to be picked up and there is room on the bus
        while [length passengers < 40 and more_passengers? civilian_sel "home"]
        [
          let civ_toadd (one-of civilians with [current_location = "home" and workplace_no != -1 and not (infection_status = "positive") and not (infection_status = "dead") and tm = "bus" and [building_group] of (one-of patches with [building_no = [apt_no] of myself]) = [building_group] of (one-of patches with [building_no = [apt_no] of civilian_sel])])
          if civ_toadd != nobody
          [ ask civ_toadd [geton_bus myself] ]
        ]
      ]
      [ ; dropoff
        bus_transmit
        ; ask all the passengers to get off and go to work
        while [length passengers > 0]
        [
          let civ_torem one-of passengers
          move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "workplace" and building_no = [workplace_no] of civ_torem]]

          ; ask the passengers whose workplace's building group is the same as the workplace building group of civ_torem
          foreach passengers
          [
            x -> ask x
            [
              if [building_group] of (one-of patches with [building_no = [workplace_no] of myself]) = [building_group] of (one-of patches with [building_no = [workplace_no] of civ_torem])
              [
                setxy (item 0 workxy) (item 1 workxy)
                set current_location "work"
                st
                ask myself [ set passengers remove myself passengers ]
              ]
            ]
          ]
        ]
      ]
    ]
    [
      if time = "evening" ; in the evening, take people home
      [
        ifelse length passengers = 0 and any? civilians with [current_location = "work" and tm = "bus"] ; if there are any civilians to be moved
        [ ; pickup

          ; select a civilian to pick up and then pick up all civilians on the same block who need transport
          let civilian_sel (one-of civilians with [current_location = "work" and tm = "bus"])
          ask civilian_sel [geton_bus myself]
          ; visually move the bus
          move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "workplace" and building_no = [workplace_no] of civilian_sel]]

          ; while any passengers in the building group still need to be picked up and there is room on the bus
          while [length passengers < 40 and more_passengers? civilian_sel "work"]
          [
            let civ_toadd (one-of civilians with [tm = "bus" and [building_group] of (one-of patches with [building_no = [workplace_no] of myself]) = [building_group] of (one-of patches with [building_no = [workplace_no] of civilian_sel])])
            if civ_toadd != nobody
            [ ask civ_toadd [geton_bus myself] ]
          ]
        ]
        [ ; dropoff
          bus_transmit
          ; ask the passengers to get off and go home
          while [length passengers > 0]
          [
            let civ_torem one-of passengers
            move-to min-one-of patches with [state = "street"] [distance one-of patches with [state = "home" and building_no = [apt_no] of civ_torem]]

            ; ask the passengers whose workplace's buulding group is the same as the workplace building group of civ_torem
            foreach passengers
            [
              x -> ask x
              [
                if [building_group] of (one-of patches with [building_no = [apt_no] of myself]) = [building_group] of (one-of patches with [building_no = [apt_no] of civ_torem])
                [
                  ;let apn apt_no
                  setxy (item 0 homexy) (item 1 homexy)
                  set current_location "home"
                  st
                  ask myself [ set passengers remove myself passengers ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
end

; put a civilian on a bus
to geton_bus [ bus_ ]

  set current_location "on bus"
  ht
  ask bus_
  [ set passengers lput myself passengers ]

end

; transmit the virus to others on the bus
to bus_transmit

  let bus_ self
  foreach passengers
  [
    x -> ask x
    [
      if infection_status = "infected"
      [
        foreach ([passengers] of bus_)
        [
          y -> ask y
          [
            transmit_to self
          ]

        ]
      ]
    ]
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
240
25
668
454
-1
-1
20.0
1
10
1
1
1
0
1
1
1
-10
10
-10
10
0
0
1
ticks
30.0

BUTTON
64
93
127
126
NIL
setup
NIL
1
T
OBSERVER
NIL
R
NIL
NIL
1

TEXTBOX
47
135
197
153
Development Options
11
0.0
1

BUTTON
36
302
156
335
make workplace
make \"workplace\"
T
1
T
OBSERVER
NIL
W
NIL
NIL
1

BUTTON
49
230
144
263
make home
make \"home\"
T
1
T
OBSERVER
NIL
H
NIL
NIL
1

BUTTON
48
266
146
299
make street
make \"street\"
T
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
13
557
137
590
generate
generate
NIL
1
T
OBSERVER
NIL
G
NIL
NIL
1

BUTTON
719
140
796
173
NIL
simulate
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
51
337
140
370
undevelop
make \"undev\"
T
1
T
OBSERVER
NIL
U
NIL
NIL
1

SLIDER
19
187
191
220
building-height
building-height
1
9
3.0
1
1
stories
HORIZONTAL

SLIDER
18
151
190
184
per-floor
per-floor
1
9
3.0
1
1
residents
HORIZONTAL

CHOOSER
718
410
856
455
time
time
"morning" "at work" "evening"
0

SLIDER
718
372
890
405
day
day
1
100
32.0
1
1
NIL
HORIZONTAL

BUTTON
442
501
546
534
export model
export-world (word \"model\" mnum \".csv\")
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
296
472
434
517
mnum
mnum
1 2 3 4 5
3

BUTTON
443
465
545
498
import model
import
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
11
376
183
409
taxi_count
taxi_count
0
100
10.0
1
1
taxis
HORIZONTAL

SLIDER
12
448
184
481
initially_expose
initially_expose
0
25
2.0
1
1
%
HORIZONTAL

SLIDER
719
49
891
82
transmission_rate
transmission_rate
0
100
5.0
1
1
%
HORIZONTAL

SLIDER
719
89
891
122
testing_rate
testing_rate
0
100
20.0
1
1
%
HORIZONTAL

PLOT
898
131
1191
417
Civilians
days
health status
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"safe" 1.0 0 -4539718 true "" "plot count civilians with [infection_status = \"safe\"]"
"exposed" 1.0 0 -955883 true "" "plot count civilians with [infection_status = \"exposed\"]"
"infected" 1.0 0 -2674135 true "" "plot count civilians with [infection_status = \"infected\"]"
"positive" 1.0 0 -13840069 true "" "plot count civilians with [infection_status = \"positive\"]"
"immune" 1.0 0 -1184463 true "" "plot count civilians with [infection_status = \"immune\"]"
"dead" 1.0 0 -16777216 true "" "plot count civilians with [infection_status = \"dead\"]"

SLIDER
23
15
195
48
patch_size
patch_size
1
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
15
53
202
86
world_size
world_size
2
100
20.0
2
1
patches (l&w)
HORIZONTAL

SLIDER
895
88
1067
121
death_rate
death_rate
0
100
10.0
.1
1
%
HORIZONTAL

SLIDER
895
49
1067
82
recovery_rate
recovery_rate
0
100
10.0
.1
1
%
HORIZONTAL

SLIDER
11
411
183
444
bus_count
bus_count
0
10
3.0
1
1
busses
HORIZONTAL

SLIDER
12
484
236
517
public_transportation_usage
public_transportation_usage
0
100
60.0
1
1
%
HORIZONTAL

SLIDER
13
520
185
553
taxi>bus
taxi>bus
0
100
60.0
1
1
%
HORIZONTAL

SLIDER
720
181
892
214
sim_delay
sim_delay
0
.25
0.0
.01
1
s
HORIZONTAL

SLIDER
718
10
890
43
exposure_rate
exposure_rate
0
100
50.0
1
1
%
HORIZONTAL

@#$#@#$#@
# Modeling Details

## Goal

Model the effect of public transportation use on the spread of covid or similar viruses

Allow the user to custom design any worldspace and determine scale

## Types of Transportation

### Private Cars

Private cars take their passengers to and from work with no risk of catching covid from the driver. Covid-status is not tracked for private cars because there is no driver other than the civilian going to work or home. For the purposes of this model, we can also equivocate car usage to walking or biking to/from work, as from a viral exposure perspective they have the same effect.

### Taxis

Taxis take their passengers to and from work, and are at risk for catching covid. The taxi drivers are able to spread the virus to their passengers if they're infected, but once they test positive they are replaced by a new uncompromised taxi.

### Busses

Busses take their passengers to and from work in larger groups, which carries with it a larger risk of infection from other passengers. Bus drivers cannot catch and spread the virus but passengers can pass it to one another. For the purposes of this model, we can equivocate subway usage to bus usage as they would model the spread of covid in a near identical way and can be represented by the same agent (a bus).

Busses pick up passengers on home and work building groups.

# USAGE GUIDE

## City Design and Initialization

**patch_size**: select the desired patch size for the world. 
**world_size**: select the number of patches for the length and width of the world
**setup**: make a new world and set all patches to undeveloped
**per-floor**: for building creation- specifies the number of people per floor
**building-height**: for building creation- specifies the number of floors
**make home (H)**: make new apartment buildings on patches you select
**make street (S)**: turn patches you select into street
**make workplace (W)**: make new work buildings on patches you select
**undevelop (U)**: destroy a building or street

## Agent Generation

**taxi_count**: choose the number of taxis in the simulation
**bus_count**: choose the number of busses in the simulation
**initially_expose**: select a proportion of the population you want to initially expose to the virus
**generate**: create the agents (people, taxis) and assign their properties

## Public Transportation Usage Score

The **public_transportation_usage** slider determines the chance that a person will choose to use public transport over a car or other private method
The **taxi>bus** slider determines the chance that a person using public transportation will choose a taxi over a bus for a given trip

## Active Simulation

**transmission_rate**: chance that an exposed person will turn out to be positive for the virus
**exposure_rate**: chance that an infected person will expose the people around them
**testing_rate**: chance that an infected person will take a test on a given day
**death_rate**: chance that an infected person will die on a given day 
**recovery_rate**: chance that an infected person will recover on a given day (days 5-14 of infection)
**simulate**: run the simulation ~ transportation moves people to work and back home allowing them to spread the virus
**time**: designates the time (morning or evening)
**day**: the day of the simulation

## Import / Export Model

**mnum**: allows the user to select a model number to save or read
**export model**: allows the user to save the model
**import model**: allows the user to read in a saved model

# Research and Useful References

Data for setting death rate:
https://ourworldindata.org/covid-deaths-by-vaccination
https://ourworldindata.org/covid-vaccinations

Data for setting testing rate:
https://ourworldindata.org/coronavirus-testing

Data for setting transmission and exposure rates based on a variety of circumstances:
https://www.yalemedicine.org/news/covid-19-variants-of-concern-omicron
https://mycovidrisk.app/
https://covid19risk.biosci.gatech.edu/
https://19andme.covid19.mathematica.org/
https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/2783099

# Behaviorspace Experiment Report

## Varying Public Transportation Usage (PTU) and Taxi>Bus (T>B):

### PTU 40%, T>B 40% Results:

day 10: 0 deaths
day 20: 25 deaths
day 30: 82 deaths

### PTU 40% T>B 60%
day 10: 3 deaths
day 20: 45 deaths
day 30 92 deaths

### PTU 60% T>B 40%

day 10: 0 deaths
day 20: 36 deaths 
day 30: 88 deaths

### PTU 60% T>B 60%

day 10: 2 deaths
day 20 34 deaths
day 30: 100 deaths

### Conclusions:

These simulation results show significant correlation between higher public transportation usage and more deaths. However, it also shows that a higher taxi>bus value, which implies people are more likely to use taxis, also correlates with more deaths. This is interesting as it implies that virally compromised drivers can be more dangerous than larger groups of people traveling together.

NOTE: This is for a system of 648 civilians, 10 taxis, and 3 busses.

# Some Related Models

#### High-Resolution Agent-Based Modeling of COVID-19 Spreading in a Small Town
https://onlinelibrary.wiley.com/doi/10.1002/adts.202000277
#### Covasim: An agent-based model of COVID-19 dynamics and interventions
https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1009149
#### A stochastic agent-based model of the SARS-CoV-2 epidemic in France
https://www.nature.com/articles/s41591-020-1001-6
#### Agent-based Simulation Model and Deep Learning Techniques to Evaluate and Predict Transportation Trends around COVID-19
https://arxiv.org/ftp/arxiv/papers/2010/2010.09648.pdf
#### An Agent-Based Covid-19 Simulator: Extending Covasim to the Polish Context
https://www.sciencedirect.com/science/article/pii/S1877050921018731?via%3Dihub
#### COVID-19 and the value of safe transport in the United States
https://www.nature.com/articles/s41598-021-01202-9
#### Risk of COVID-19 Infection in Public Transportation: The Development of a Model
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8657409/


# Simplifications

- the model is relatively small-scale in relation to an actual city and is spatially limited
- the model assumes that the only time people interact with others is at home, on the way to work and at work
	- no children / relationships / activities / restaurants / etc.
- the model assumes that individuals do not change jobs or move
- the model assumes the spatial limitation that no people leave the space or interact with other people outside the ones in the simulation
- the model limits transportation methods to 3 categories
- the model does not account for specificities of real civilian interactions or the fluxuation of covid contaigiousness over time
- the model also assumes there is little anti-viral infrastructure to reduce the spread of covid in taxis or busses


@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

bus
false
0
Polygon -7500403 true true 15 206 15 150 15 120 30 105 270 105 285 120 285 135 285 206 270 210 30 210
Rectangle -16777216 true false 36 126 231 159
Line -7500403 false 60 135 60 165
Line -7500403 false 60 120 60 165
Line -7500403 false 90 120 90 165
Line -7500403 false 120 120 120 165
Line -7500403 false 150 120 150 165
Line -7500403 false 180 120 180 165
Line -7500403 false 210 120 210 165
Line -7500403 false 240 135 240 165
Rectangle -16777216 true false 15 174 285 182
Circle -16777216 true false 48 187 42
Rectangle -16777216 true false 240 127 276 205
Circle -16777216 true false 195 187 42
Line -7500403 false 257 120 257 207

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

car side
false
0
Polygon -7500403 true true 19 147 11 125 16 105 63 105 99 79 155 79 180 105 243 111 266 129 253 149
Circle -16777216 true false 43 123 42
Circle -16777216 true false 194 124 42
Polygon -16777216 true false 101 87 73 108 171 108 151 87
Line -8630108 false 121 82 120 108
Polygon -1 true false 242 121 248 128 266 129 247 115
Rectangle -16777216 true false 12 131 28 143

car top
true
0
Polygon -7500403 true true 151 8 119 10 98 25 86 48 82 225 90 270 105 289 150 294 195 291 210 270 219 225 214 47 201 24 181 11
Polygon -16777216 true false 210 195 195 210 195 135 210 105
Polygon -16777216 true false 105 255 120 270 180 270 195 255 195 225 105 225
Polygon -16777216 true false 90 195 105 210 105 135 90 105
Polygon -1 true false 205 29 180 30 181 11
Line -7500403 false 210 165 195 165
Line -7500403 false 90 165 105 165
Polygon -16777216 true false 121 135 180 134 204 97 182 89 153 85 120 89 98 97
Line -16777216 false 210 90 195 30
Line -16777216 false 90 90 105 30
Polygon -1 true false 95 29 120 30 119 11

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

van top
true
0
Polygon -7500403 true true 90 117 71 134 228 133 210 117
Polygon -7500403 true true 150 8 118 10 96 17 85 30 84 264 89 282 105 293 149 294 192 293 209 282 215 265 214 31 201 17 179 10
Polygon -16777216 true false 94 129 105 120 195 120 204 128 180 150 120 150
Polygon -16777216 true false 90 270 105 255 105 150 90 135
Polygon -16777216 true false 101 279 120 286 180 286 198 281 195 270 105 270
Polygon -16777216 true false 210 270 195 255 195 150 210 135
Polygon -1 true false 201 16 201 26 179 20 179 10
Polygon -1 true false 99 16 99 26 121 20 121 10
Line -16777216 false 130 14 168 14
Line -16777216 false 130 18 168 18
Line -16777216 false 130 11 168 11
Line -16777216 false 185 29 194 112
Line -16777216 false 115 29 106 112
Line -7500403 false 210 180 195 180
Line -7500403 false 195 225 210 240
Line -7500403 false 105 225 90 240
Line -7500403 false 90 180 105 180

virus
true
15
Circle -1 true true 75 75 150
Circle -1 true true 75 30 30
Circle -1 true true 150 0 60
Circle -1 true true 43 88 32
Circle -1 true true 0 180 60
Circle -1 true true 90 225 30
Circle -1 true true 225 90 30
Circle -1 true true 159 249 42
Circle -1 true true 225 165 58
Polygon -1 true true 75 60
Polygon -1 true true 90 60 120 105 135 105 90 45 90 45
Polygon -1 true true 165 45 150 90 180 90 180 45 165 45
Polygon -1 true true 240 105 195 120 210 135 240 105
Polygon -1 true true 60 105 105 120 90 135 60 105
Polygon -1 true true 45 195 105 165 105 195 45 210
Polygon -1 true true 105 225 105 195 120 210 105 240
Polygon -1 true true 180 255 180 210 165 210 150 210 165 255
Polygon -1 true true 210 165 240 180 240 195 210 180

warning
false
0
Polygon -7500403 true true 0 240 15 270 285 270 300 240 165 15 135 15
Polygon -16777216 true false 180 75 120 75 135 180 165 180
Circle -16777216 true false 129 204 42

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>import
generate</setup>
    <go>simulate</go>
    <final>print "run finished"</final>
    <exitCondition>day &gt; 30</exitCondition>
    <metric>count civilians with [infection_status = "dead"]</metric>
    <enumeratedValueSet variable="building-height">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="day">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transmission_rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time">
      <value value="&quot;morning&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mnum">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="taxi_count">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="public_transportation_usage" first="40" step="20" last="60"/>
    <enumeratedValueSet variable="per-floor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initially_expose">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="taxi&gt;bus" first="40" step="20" last="60"/>
    <enumeratedValueSet variable="sim_delay">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exposure_rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch_size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death_rate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery_rate">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing_rate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world_size">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bus_count">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
