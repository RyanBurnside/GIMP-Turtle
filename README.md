GIMP-Turtle
=============

A simple set of functions designed to be used in the Script-Fu REPL to add precise placement and drawing commands to GIMP using turtle graphics.

This allows the user to define drawing routines in real time.
Precision lines and shapes can be made by defining a Turtle and telling it to move and face various directions.

=============
README

These commands are used in real time, start a Script-Fu console to begin.
The Script-Fu console is actually a TinyScheme language environment.
This environment is traditionally called a Read Evaluate Print Loop or REPL.
You will give TinyScheme instructions here using my Turtle library.

Turtles?:

To start you need to define a Turtle which is another name for a movable position that you will be drawing with. A Turtle has a pen attached to his tail, you can raise or lower the pen with the pu and pd commands.

These stand for pen up and pen down. By default a turtle has his pen down.

A Turtle keeps track of his position, color, direction and pen.

Let's try an example type in each line and hit ENTER key.
First, we need to make a new Turtle and call him "t".

(define t (make-Turtle 0 0 0))

Now, we want to move him to the center of the screen, tell him to go "home"

(home t)

Now, let's tell him to look at 0 degrees (points left).

(lk 0 t)

Good, lets tell him to move forward 100 pixels.

(fd 100 t)

A line appears! Lets tell him to turn right 90 degrees.

(rt 90 t)

Repeat the last two steps until you form a 100 x 100 pixel square!

Now please quickly look over the source code Turtle.scm. This contains all the functions you may use. You might take a week or so to learn TinyScheme. Gimp and this Turtle library are NOT part of TinyScheme but extend the language to be used in Gimp.

I've included a help function that may give you additional information on the command parameters if you get stuck. For help on the mv command you could type the following into the REPL.

(help mv)

You will be given the document string for mv command.

***Note***
Currently there is a bug in Gimp.
You will need to make a brush with a 1 pixel image to draw a thin line.
Otherwise you will be drawing in an unscaled version of the current brush.
This may be fixed in the future.

Also, if you wish to execute many commands without flooding your undo stack you might wrap your calls in the (with-undo-group ...) macro. See complex examples in source code.

I'm not a professional programmer but encourage you to learn a dialect of LISP
It may not be trendy but there are a lot of unique aspects to both Scheme and Common Lisp.

Have fun! Happy programming!

-Ryan Burnside
