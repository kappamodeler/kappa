(**EDIT THIS PART AT WILL**)

let browser_command = "open" (* use "open" under MacOs, "xdg-open" under unix*)
let dot_command = "/opt/homebrew/bin/dot"
let neato_command = "/opt/homebrew/bin/neato"
let gnuplot_image_terminal = "png"
let gnuplot_window_terminal = "X11"
let gnuplot_activity_style = "points"
let gnuplot_concentration_style = "lines"
let dot_image_format = "png"
let css_file = "/home/krivine/usr/lib/style.css"


let additional_buttons =  (*put any number of additional button here with format (html_address,label)*)
  [(*(address,label) ;*)
    ("mailto:jk@plectix.com","Contact") ;
    ("http://www.rcsb.org/pdb/","Protein database")
  ]

let auto_launch_browser = true (*set to false if you find it annoying*)

(*COMMAND LINE DEFAULT VALUES*)
(*DO NOT EDIT*)
let auto_plot = ref false
let build_rules = ref true
