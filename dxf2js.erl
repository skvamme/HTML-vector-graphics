-module(dxf2js).
-author(skvamme).
-compile(export_all).
-import(lists, [reverse/1]).
-import(ets,[lookup/2,insert/2]).
-import(math,[pi/0,pow/2,sqrt/1,atan2/2]).
-define(DOUBLE, 64/little-float).
-define(DWORD, 32/unsigned-little-integer).
-define(WORD, 16/unsigned-little-integer).
-define(BYTE, 8/unsigned-little-integer).

%% ToDo: More entity types eg POINT
%% ctx.shadowOffsetX = 2;
%% ctx.shadowOffsetY = 2;
%% ctx.shadowBlur = 2;
%% ctx.shadowColor = "rgba(0,0,0,0.5)";
%% Done:
%% 080612	First version													S Kvamme
%% 081213	Bug in setting of Color fixed								S Kvamme
%% 081213	Bug in sorting fixed. Changed to sort Thicknes (was Elevation).	S Kvamme
%% 090909	Added support for ASCII DXF								S Kvamme
%% 100117	Bug in entity color fixed (QCAD)							S Kvamme
%% 100619	Changed output format from erlang to Javascript		S Kvamme


%****************************************************************************
% Read dxf file and otput corresponding javascript source for <CANVAS> graphics
%****************************************************************************
start(Args) ->
	DXF = hd(Args),
	Mode = svg, % svg or canvas are supported
	Layerarray = tl(Args),
	Etable = ets:new(entity,[duplicate_bag,private]), % Store all the entities in this table
	Ttable = ets:new(tmp,[set,private]), % Store temporary group values here
	lists:foreach( fun(Layer) -> 
		case read_dxf_tag(DXF) of
			error -> io:format("//Cannot read file: ~p~n",[DXF]);
			ascii ->
				{ok, F} = file:open(DXF, read),
				print_header(),
				Mode:print_body(),
				find_entities_ascii(F),
				io:get_line(F, ''), % get rid of "  0"
				entities_ascii(F,Etable,Layer,trim(io:get_line(F, '')));
			bin -> 
				{ok, B} = file:read_file(DXF),
				{_,B1} = split_binary(B, 22),
				B2 = find_header(B1),
				print_header(),
				Mode:print_body(),
				limits(B2),
				B3 = find_entities(B2),
				entities(Etable,B3,Layer)
		end,
		prints_entities(Etable,Ttable,Mode),
		Mode:print_endbody()
	end,Layerarray),
	print_trailer().


%****************************************************************************************
% Function print_header()
%****************************************************************************************
print_header() ->
	io:format("<html>~n",[]),
	io:format("<head>~n",[]),
	io:format("<meta name=creator, value=dxf2js>~n",[]),
	io:format("<title>dxf2js</title>~n",[]),
	io:format("</head>~n",[]),
	io:format("<body>~n",[]).


%****************************************************************************************
% Function print_trailer()
%****************************************************************************************
print_trailer() ->
	io:format("</body>~n"),
	io:format("</html>~n").


%****************************************************************************************
% Function find_entities_ascii(F1), 
%****************************************************************************************
find_entities_ascii(F) -> fea(F,"").

fea(_F,"ENTITIES") -> ok;
fea(_F,eof) -> io:format("Didn't find entities section~n",[]),erlang:halt();
fea(F,_) -> fea(F,trim(io:get_line(F, ''))).
	

%****************************************************************************************
% Function entities_ascii(Etable,F1)
%****************************************************************************************
entities_ascii(_F,_Etable,_Layer,"ENDSEC") -> ok; 
entities_ascii(_F,_Etable,_Layer,eof) -> ok;
entities_ascii(F,Etable,Layer,E) ->
	entity_ascii(F,Etable,Layer,E),
	entities_ascii(F,Etable,Layer,trim(io:get_line(F, ''))).


%****************************************************************************************
% Function entity_ascii(F,Etable,Entity);
%****************************************************************************************
entity_ascii(F,Etable,Layer,E) ->
	Gtable = ets:new(group,[duplicate_bag,private]),
	reset_all(Gtable),
	e_a(F,Etable,Gtable,Layer,E,trim(io:get_line(F, ''))).

e_a(_F,_Etable,_Gtable,_Layer,_E,eof) -> ok;
e_a(_F,Etable,Gtable,Layer,E,"0") -> % end of this entity
	params(Gtable,Etable,Layer,E,'end',0);
e_a(F,Etable,Gtable,Layer,E,G) ->
	G1 = list_to_integer(G),
	V = format_value(G1,trim(io:get_line(F, ''))), 
	params(Gtable,Etable,Layer,E,V,G1),
	e_a(F,Etable,Gtable,Layer,E,trim(io:get_line(F, ''))).


%****************************************************************************************
% Function trim(String) 
%****************************************************************************************
trim(String) ->
    reverse(strip(reverse(strip(String)))).

strip([$   | Cs]) -> strip(Cs);
strip([$\t | Cs]) -> strip(Cs);
strip([$\r | Cs]) -> strip(Cs);
strip([$\n | Cs]) -> strip(Cs);
strip(Cs) -> Cs.


%****************************************************************************
% Check that this is a binary DXF
%****************************************************************************
read_dxf_tag(File) -> 
	case file:open(File, [read,binary,raw]) of
		{ok, S} ->
			{ok, B2} = file:pread(S, 0, 18),
			Result = parse_tag(B2),
			file:close(S);
		 _ -> 	
			Result = error
	end,		 
	Result.

	parse_tag(<<$A,$u,$t,$o,$C,$A,$D,$\s,$B,$i,$n,$a,$r,$y,$\s,$D,$X,$F>>) -> bin; 	
	parse_tag(_) -> ascii. 


%****************************************************************************
% prints_entities(Etable,Ttable,canvas or svg)
% Print all the entities, lowest thickness first
%****************************************************************************
prints_entities(Etable,Ttable,Mode) ->
	Elevlist = elevations(Etable), % print the entities lowest elevation first
	prints_entities1(Elevlist,Etable,Ttable,Mode).
	
prints_entities1([],_Etable,_Ttable,_Mode) -> true;
prints_entities1([E|Tail],Etable,Ttable,Mode) ->
	Entitytuplelist = ets:lookup(Etable,E),
	% io:format("Entities ~p~n",[Entitytuplelist]),
	lists:foreach(fun (Entity) -> Mode:print_entity(Entity,Ttable) end, Entitytuplelist),
	prints_entities1(Tail,Etable,Ttable,Mode).


%****************************************************************************
% Create a sorted list with all unique Thicknesses
%****************************************************************************
elevations(Etable) -> 
	Key = ets:first(Etable),
	Elevlist = elevation1(Etable,[],Key),
	lists:usort(Elevlist).

elevation1(_,Elevlist,'$end_of_table') -> Elevlist;
elevation1(Etable,Elevlist,Key) ->
	Key1 = ets:next(Etable,Key),
	elevation1(Etable,[Key|Elevlist],Key1).


%****************************************************************************
% Find the header section in the dxf file
%****************************************************************************
find_header(B) -> find_header1({B,"",0}).

find_header1({B,"HEADER",2}) -> B;
find_header1({B,_,_}) -> find_header1(parse_dxf(B)).

	
%****************************************************************************
% Write the size of the drawing
%****************************************************************************
limits(B) -> limits1({B,"",0}).
limits1({B,"$EXTMIN",9}) -> 
	{B1,X1,_G1} = parse_dxf(B), 
	{B2,Y1,_G2} = parse_dxf(B1),
    io:format("var bbox = [~B,~B,",[round(X1),round(Y1)]),
	limits1({B2,"",0});
limits1({B,"$EXTMAX",9}) -> 
	{B1,X2,_G1} = parse_dxf(B), 
	{_B2,Y2,_G2} = parse_dxf(B1),
	io:format("~B,~B];~n",[round(X2),round(Y2)]),
	io:format("//EndSetup~n",[]);
limits1({B,_,_}) -> limits1(parse_dxf(B)).


%****************************************************************************
% Insert a new entity table Gtable in the Etable. Thickness is the key, Etype is the entity type
%****************************************************************************
doInsert(Gtable,Etable,Etype,Layer)  -> 
	[{8,Layer1}|_] = reverse(lookup(Gtable, 8)), % Check it's on the right layer
	case Layer == Layer1 of
		true ->
				[{39,Elev}|_] = reverse(lookup(Gtable, 39)),
				case Etype == "POLYLINE" of  % A hack for polylines when vertices and
					true -> 	put(elevation,Elev); % seqend don't have a thickness
					_ -> 		ok
				end,
				case ((Etype == "VERTEX") or (Etype == "SEQEND")) of
					true -> 	insert(Etable,{get(elevation),Etype,Gtable});
					_ -> 		insert(Etable,{Elev,Etype,Gtable})
				end;
		_ -> ignore
	end.


%****************************************************************************
% Erase the previous entity from the Gtable and set some default values
%****************************************************************************
reset_all(Gtable) ->
	ets:delete_all_objects(Gtable),
	insert(Gtable, {6,"CONTINUOUS"}),
	insert(Gtable, {7,"STANDARD"}),
	insert(Gtable, {8,"0"}),
	insert(Gtable, {38,0}),
	insert(Gtable, {39,0}),
	insert(Gtable, {44,0}),
	insert(Gtable, {45,0}),
	insert(Gtable, {62,0}),
	insert(Gtable, {71,0}),
	insert(Gtable, {72,0}).
	
	
%****************************************************************************
% Find the entities section in the dxf file
%****************************************************************************
find_entities(B) -> find_entities1({B,"",0}).
find_entities1({B,"ENTITIES",2}) -> B;
find_entities1({B,_,_}) -> find_entities1(parse_dxf(B)).


%****************************************************************************
% Step thru each entity in the entities section
%****************************************************************************
entities(_,<<>>,_Layer) -> true;
entities(Etable,<<0:?WORD,Rest/binary>>,Layer) -> 
	Gtable = ets:new(group,[duplicate_bag,private]),
	reset_all(Gtable), 
	{B,T} = ac_text(Rest), 
	B1 = entity(Gtable,Etable,Layer,T,B), 
	entities(Etable,B1,Layer);
entities(Etable,B,Layer) -> 
	{B1,_T1,_G1} = parse_dxf(B), 
	entities(Etable,B1,Layer).


%****************************************************************************
% Step thru each Group in an entity
%****************************************************************************
entity(_,_,_,_,<<>>) -> <<>>;
entity(Gtable,Etable,Layer,E,<<0:?WORD,Rest/binary>>) -> 
	params(Gtable,Etable,Layer,E,'end',0), 
	<<0:?WORD,Rest/binary>>; %% Reached the end of current entity
entity(Gtable,Etable,Layer,E,B) ->
	{B1,V1,G1} = parse_dxf(B), 
	params(Gtable,Etable,Layer,E,V1,G1), 
	entity(Gtable,Etable,Layer,E,B1).


%****************************************************************************
% Insert each entity into an ets table Gtable
%****************************************************************************
params(Gtable,Etable,Layer,Etype, 'end', 0) -> doInsert(Gtable,Etable,Etype,Layer);
params(Gtable,_,_,_, V, 42) -> 
	insertBulge(Gtable, length(lookup(Gtable,10)),length(lookup(Gtable,42)), {42,V});
params(Gtable,_,_,_, V, G) -> insert(Gtable, {G,V}).

% When inserting a G42, first make sure the G42 list is equally long (-1) as the G10 list 
insertBulge(Gtable,G10,G42,G) when G10 == G42+1 -> insert(Gtable, G);
insertBulge(Gtable,G10,G42,G) -> insert(Gtable, {42,0}),insertBulge(Gtable,G10,G42+1,G).


%****************************************************************************
% Parse the binary dxf file and create erlang data types
%****************************************************************************
ac_text(Thefile) -> T = parse_text(Thefile, []), {_,B1} = split_binary(Thefile, length(T)+1), {B1,T}.

parse_text(<<0:?BYTE,_R/binary>>,Text) -> reverse(Text) ; %% Terminating zero string composer	
parse_text(<<C:?BYTE,R/binary>>,R1) -> parse_text(R, [C|R1]).

ac_double(Thefile) -> <<Y:?DOUBLE,B/binary>> = Thefile, {B,Y}.

% ac_byte(Thefile) -> <<F:?BYTE,B/binary>> = Thefile, {B,F}.

ac_word(Thefile) -> <<F:?WORD,B/binary>> = Thefile, {B,F}.

ac_dword(Thefile) -> <<F:?DWORD,B/binary>> = Thefile, {B,F}.

parse_dxf(<<G0:?WORD,Rest/binary>>) when G0 >= 0, G0 =< 9 -> {B,T} = ac_text(Rest), {B,T,G0};
parse_dxf(<<G10:?WORD,Rest/binary>>) when G10 >= 10, G10 =< 59 -> {B,F} = ac_double(Rest),{B,F,G10};
parse_dxf(<<G60:?WORD,Rest/binary>>) when G60 >= 60, G60 =< 79 -> {B,W} = ac_word(Rest),{B,W,G60};
parse_dxf(<<G90:?WORD,Rest/binary>>) when G90 >= 90, G90 =< 99 -> {B,W} = ac_dword(Rest),{B,W,G90};
parse_dxf(<<G100:?WORD,Rest/binary>>) when G100 >= 100, G100 =< 105 -> {B,T} = ac_text(Rest),{B,T,G100};
parse_dxf(<<G140:?WORD,Rest/binary>>) when G140 >= 140, G140 =< 147 -> {B,F} = ac_double(Rest),{B,F,G140};
parse_dxf(<<G170:?WORD,Rest/binary>>) when G170 >= 170, G170 =< 178 -> {B,W} = ac_word(Rest),{B,W,G170};%% 176-178 not in spec
parse_dxf(<<G210:?WORD,Rest/binary>>) when G210 >= 210, G210 =< 239 -> {B,F} = ac_double(Rest),{B,F,G210};
parse_dxf(<<255:?WORD,Rest/binary>>) -> {B,W} = ac_word(Rest),{B,W,255};
parse_dxf(<<G270:?WORD,Rest/binary>>) when G270 >= 270, G270 =< 275 -> {B,W} = ac_word(Rest),{B,W,G270};%% not in spec
parse_dxf(<<G280:?WORD,Rest/binary>>) when G280 >= 280, G280 =< 289 -> {B,W} = ac_word(Rest),{B,W,G280}; %% ac_byte in spec
parse_dxf(<<G300:?WORD,Rest/binary>>) when G300 >= 300, G300 =< 369 -> {B,T} = ac_text(Rest),{B,T,G300};
parse_dxf(<<G1000:?WORD,Rest/binary>>) when G1000 >= 1000, G1000 =< 1009 -> {B,T} = ac_text(Rest),{B,T,G1000};
parse_dxf(<<G1010:?WORD,Rest/binary>>) when G1010 >= 1010, G1010 =< 1059 -> {B,F} = ac_double(Rest),{B,F,G1010};
parse_dxf(<<G1060:?WORD,Rest/binary>>) when G1060 >= 1060, G1060 =< 1069 -> {B,F} = ac_word(Rest),{B,F,G1060};
parse_dxf(<<G1071:?WORD,Rest/binary>>) when G1071 >= 1071, G1071 =< 1079 -> {B,F} = ac_dword(Rest),{B,F,G1071}.
%parse_dxf(<<G:?WORD,_Rest/binary>>) -> io:format("Missing group: ~w~n",[G]), erlang:halt().

%****************************************************************************************
% Parse the ASCII dxf file and create erlang data types
%****************************************************************************************
format_value(G,S) when G >= 0, G =< 9 -> S; %String
format_value(G,S) when G >= 10, G =< 59 -> {V,_} = string:to_float(S),V;
format_value(G,S) when G >= 60, G =< 79 -> list_to_integer(S);
format_value(G,S) when G >= 90, G =< 99 -> list_to_integer(S);
format_value(100,S) -> S; %String
format_value(102,S) -> S; %String
format_value(105,S) -> S; %String
format_value(G,S) when G >= 140, G =< 147 -> {V,_} = string:to_float(S),V;
format_value(G,S) when G >= 170, G =< 175 -> list_to_integer(S);
format_value(G,S) when G >= 210, G =< 230 -> {V,_} = string:to_float(S),V; % ellipse, ej i standard
format_value(G,S) when G >= 280, G =< 289 -> list_to_integer(S);
format_value(G,S) when G >= 300, G =< 369 -> S; %String
format_value(G,S) when G >= 370, G =< 389 -> list_to_integer(S);
format_value(G,S) when G >= 390, G =< 399 -> S; %String
format_value(G,S) when G >= 400, G =< 409 -> list_to_integer(S);
format_value(G,S) when G >= 410, G =< 419 -> S; %String
format_value(420,S) -> list_to_integer(S); % QCAD: some value if entity color is other than default
format_value(999,S) -> S; %String
format_value(G,S) when G >= 1000, G =< 1009 -> S; %String
format_value(G,S) when G >= 1010, G =< 1059 -> {V,_} = string:to_float(S),V;
format_value(G,S) when G >= 1060, G =< 1071 -> list_to_integer(S).
%format_value(G,S) -> io:format("Group: ~p, Value:  ~p~n",[G,S]).

