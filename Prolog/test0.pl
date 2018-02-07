:- use_module(library(hyper)).

:- pce_begin_class(link_demo, picture).
initialise(P) :->
	send_super(P, initialise, 'Link Demo'),
	send(P, recogniser,
		click_gesture(left, ’’, single,
			message(P, add_box, @event?position))).

add_box(P, At:point) :->
	send(P, display, new(link_box), At).

:- pce_end_class(link_demo).

:- pce_begin_class(link_box, box).

handle(w/2, 0, link, north).
handle(w/2, h, link, south).
handle(0, h/2, link, west).
handle(w, h/2, link, east).

initialise(B) :->
	send_super(B, initialise, 100, 50),
	send_list(B, recogniser,
		[ click_gesture(left, ’’, double,
				message(B, edit)),
			new(connect_gesture),
			new(move_gesture)
		]).

colour(red).
colour(green).
colour(blue).
colour(yellow).

edit(B) :->
		new(D, dialog('Select colour')),
		send(D, append, new(M, menu(colour, choice,
				message(?(D, hypered, box),
					fill_pattern,
					@arg1)))),
		( colour(Colour),
			send(M, append,
				menu_item(colour(Colour),
					@default,
					pixmap(@nil,
						background := Colour,
						width := 32,
						height := 16))),
			fail
		; true
		),
		send(D, append, button(done, message(D, destroy))),
		new(_, partof_hyper(B, D, dialog, box)),
		get(B, display_position, PosB),
		get(PosB, plus, point(20,100), PosD),
		send(D, open, PosD).

:- pce_end_class(link_box).