-module(canvas).
-author(skvamme).
-compile(export_all).
-import(lists, [reverse/1]).
-import(ets,[lookup/2,insert/2]).
-import(math,[pi/0,pow/2,sqrt/1,atan2/2]).

%% ToDo: 
%% Done:
%% 120123	First version													S Kvamme


%****************************************************************************
% Print the <canvas>
%****************************************************************************
print_body({X1,Y1,X2,Y2}) ->
	io:format("<canvas id=dxf height=\"~.3f\" width=\"~.3f\" style=\"position:absolute; left:0; top:0; z-index:0;\">~n",[Y2-Y1,X2-X1]),
	io:format("<style type='text/css'> canvas { border: solid 1px red; } </style>~n",[]),
	io:format("<script type='text/javascript'>~n",[]),
	io:format("colors = new Array(257);~n",[]),
	io:format("for(var i=0;i<257;i++) {~n",[]),
	io:format("	colors[i]='rgb('+i+','+i+','+i+')';~n",[]),
	io:format("}; colors[1] = 'red'; colors[3] = 'green';~n",[]),
	io:format("var canvas = document.getElementById('dxf');~n",[]),
	io:format("var ctx = canvas.getContext('2d');~n",[]),
	io:format("canvas.setAttribute('miterLimit','2.0');~n",[]),
%	io:format("ctx.scale(1,-1);~n",[]),
%	io:format("ctx.translate(0,\"-~.3f\");~n",[Y2-Y1]),
	io:format("ctx.lineWidth=1;~n",[]).


%****************************************************************************************
% Function print_endbody()
%****************************************************************************************
print_endbody() ->
	io:format("</script>~n").
	

%****************************************************************************
% Print an entity in <canvas> notation
%****************************************************************************
print_entity({_,"TRACE",Entity},_) -> print_entity({0,"SOLID",Entity},0);
print_entity({_,"SOLID",Entity},_) ->
	[{_,X1}|_] = lookup(Entity, 10),[{_,Y1}|_] = lookup(Entity, 20),
	[{_,X2}|_] = lookup(Entity, 12),[{_,Y2}|_] = lookup(Entity, 22),
	[{_,X3}|_] = lookup(Entity, 13),[{_,Y3}|_] = lookup(Entity, 23),
	[{_,X4}|_] = lookup(Entity, 11),[{_,Y4}|_] = lookup(Entity, 21),
	[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	setColor(Pen),
	io:format("ctx.beginPath(); ctx.moveTo(~.3f,~.3f); ctx.lineTo(~.3f,~.3f); ctx.lineTo(~.3f,~.3f); ctx.lineTo(~.3f,~.3f); ctx.stroke();~n",
	  [X1,Y1,X2,Y2,X3,Y3,X4,Y4]);
	
print_entity({_,"LINE",Entity},_) ->
	[{_,X1}|_] = lookup(Entity, 10),
	[{_,Y1}|_] = lookup(Entity, 20),
	[{_,X2}|_] = lookup(Entity, 11),
	[{_,Y2}|_] = lookup(Entity, 21),
	[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	setColor(Pen),
	io:format("ctx.beginPath(); ctx.moveTo(~.3f,~.3f); ctx.lineTo(~.3f,~.3f); ctx.stroke();~n",[X1,Y1,X2,Y2]);
	
print_entity({_,"ARC",Entity},_) ->
	[{_,X1}|_] = lookup(Entity, 10),
	[{_,Y1}|_] = lookup(Entity, 20),
	[{_,Radius}|_] = lookup(Entity, 40),
	[{_,Startangle}|_] = lookup(Entity, 50),
	[{_,Endangle}|_] = lookup(Entity, 51),
	[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	setColor(Pen),
	io:format("ctx.beginPath(); ctx.arc(~.3f,~.3f,~.3f,~.3f,~.3f,false); ctx.stroke();~n",[X1,Y1,Radius,dtor(Startangle),dtor(Endangle)]);

print_entity({_,"ELLIPSE",Entity},_) -> 
	[{_,X1}|_] = lookup(Entity, 10),
	[{_,Y1}|_] = lookup(Entity, 20),
	[{_,RadiusX}|_] = lookup(Entity, 11),
	[{_,RadiusY}|_] = lookup(Entity, 21),
	[{_,Startangle}|_] = lookup(Entity, 41),
	[{_,Endangle}|_] = lookup(Entity, 42),
	[{_,Ratio}|_] = lookup(Entity, 40),
	[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	Rot = ang(RadiusX,RadiusY,0,0),
	R = radius(RadiusX,RadiusY,0,0),
	setColor(Pen),
	io:format("ctx.save();~n"),
	io:format("ctx.translate(~.3f,~.3f);~n",[X1,Y1]),
	io:format("ctx.rotate(~.3f);~n",[Rot]),
	io:format("ctx.scale(1,~.3f);~n",[Ratio]),
	io:format("ctx.beginPath(); ctx.arc(0,0,~.3f,~.3f,~.3f,false); ctx.stroke();~n",[R,dtor(Startangle),dtor(Endangle)]),
	io:format("ctx.restore();~n");

print_entity({_,"CIRCLE",Entity},_) ->
	[{_,X1}|_] = lookup(Entity, 10),
   [{_,Y1}|_] = lookup(Entity, 20),
	[{_,Radius}|_] = lookup(Entity, 40),
   [{_,Pen}|_] = reverse(lookup(Entity, 62)),
	setColor(Pen),
	io:format("ctx.beginPath(); ctx.arc(~.3f,~.3f,~.3f,~.3f,false); ctx.stroke();~n",[X1,Y1,Radius,2*pi()]);

print_entity({_,"TEXT",Entity},_) ->
	[{_,X1}|_] = lookup(Entity, 10),
   [{_,Y1}|_] = lookup(Entity, 20),
	[{_,String}|_] = lookup(Entity, 1),
	[{_,Size}|_] = lookup(Entity, 40),
   [{_,Pen}|_] = reverse(lookup(Entity, 62)),
	setColor(Pen),
%	io:format("ctx.save();~n",[]),
%	io:format("ctx.scale(1,-1);~n",[]),
%	io:format("ctx.translate(0,~.3f);~n",[-1 * (2*Y1)]),
   io:format("ctx.font = '~.3fpt Helvetica'; ctx.fillText('~s', ~.3f, ~.3f);~n",[Size,String,X1,Y1]);
%	io:format("ctx.restore();~n",[]);

print_entity({_,"POLYLINE",Entity},Ttable) -> 
	[{_,Pen}|_] = reverse(lookup(Entity, 62)), 
	setColor(Pen),
	Closed = lookup_safe(Entity, 70), 
	insert(Ttable,{firstvertex,1}),
	insert(Ttable,{flags,Closed});
	
print_entity({_,"VERTEX",Entity},Ttable) -> 
	[{_,X1}|_] = lookup(Entity, 10),
	[{_,Y1}|_] = lookup(Entity, 20),
	[{_,FV}|_] = lookup(Ttable, firstvertex),
	Bulge = lookup_safe(Entity, 42),
	case FV of	
		1 ->  	
			io:format("ctx.beginPath(); ctx.moveTo(~.3f,~.3f);~n",[X1,Y1]),
			insert(Ttable,{firstvertex,0}),
			insert(Ttable,{startx,X1}),
			insert(Ttable,{starty,Y1});
		_ -> 	
			[{_,X2}|_] = lookup(Ttable, 10), 
			[{_,Y2}|_] = lookup(Ttable, 20),
			Bulge1 = lookup_safe(Ttable,42), % Bulge group on previous vertex
			drawSegment(Bulge1,X2,Y2,X1,Y1)
	end,
	insert(Ttable,{10,X1}),    % Save point
	insert(Ttable,{20,Y1}),
	insert(Ttable,{42,Bulge}); % Save bulge
	

print_entity({_,"SEQEND",_Entity},Ttable) ->
	[{_,Closed}|_] = lookup(Ttable, flags),
	case Closed of
		0 -> 	io:format("ctx.stroke();~n",[]);
		1 -> 	[{_,X1}|_] = lookup(Ttable, startx), % Draw the closing segment
		 		[{_,Y1}|_] = lookup(Ttable, starty),
				[{_,X2}|_] = lookup(Ttable, 10), 
				[{_,Y2}|_] = lookup(Ttable, 20),
				Bulge1 = lookup_safe(Ttable,42),
				drawSegment(Bulge1,X2,Y2,X1,Y1),
				io:format("ctx.fill();~n",[])
	end,
	ets:delete_all_objects(Ttable);

print_entity({_,"LWPOLYLINE",Entity},_) ->
	[{_,Pen}|_] = reverse(lookup(Entity, 62)), 
	setColor(Pen),
	G10list = lookup(Entity, 10),
	G20list = lookup(Entity, 20),
	fixBulgelist(Entity,length(lookup(Entity, 10)),length(lookup(Entity, 42))),
	Bulgelist = lookup(Entity, 42),
	[{70,Closed}] = lookup(Entity, 70),
	case Closed of
		1 -> 	G10list1 = append_startpoint(G10list),
		  		G20list1 = append_startpoint(G20list),
		  		Bulgelist1 = append_startpoint(Bulgelist);
		_ -> 	G10list1 = G10list,
	   		G20list1 = G20list,
	   		Bulgelist1 = Bulgelist
	end,
   doLWPoly(Closed,1,Bulgelist1,G10list1,G20list1);

print_entity({_,_Name,_Entity},_) -> ok.
%		List = ets:tab2list(_Entity),
%		io:format("~p ~p~n", [_Name,List]).

%****************************************************************************
% Set current drawing color if it is new
%****************************************************************************
setColor(Pen) ->
	case get(color) /= Pen of
   true -> put(color,Pen),
			io:format("ctx.fillStyle = colors[~p];~n",[Pen]),
   		io:format("ctx.strokeStyle = colors[~p];~n",[Pen]);
	_ -> ok
end.

%****************************************************************************
% Math functions
%****************************************************************************
rtod(R) -> R * 180 / pi().

dtor(D) -> D * pi() / 180.

rtodfix(R)-> R1 = R * 180 / pi(),
	 case R1 > 360 of
		true -> R1 - 360;
		_ -> R1
		end.

cotbce(B) -> ((1 / B) - B) / 2.

xcenter(X1,X2,Y1,Y2,Cbce) -> (((Y2 - Y1) * Cbce * -1) + X1 + X2) / 2.

ycenter(X1,X2,Y1,Y2,Cbce) -> (((X2 - X1) * Cbce) + Y1 + Y2) / 2.	

radius(X1,Y1,Xcen,Ycen) -> T = pow((Xcen - X1),2) + pow((Ycen - Y1),2),
	sqrt(T).

ang(X1,Y1,Xcen,Ycen) -> atan2((Y1 - Ycen),(X1 - Xcen)).

fixang(Ang) when Ang < 0.0 -> Ang + (2 * pi());
fixang(Ang) -> Ang.

%****************************************************************************************
% Function lookup_safe(Entity,G) -> 
% Some group codes are optional. Return a zero if not found.
%****************************************************************************************
lookup_safe(Entity,G) -> 
	case  lookup(Entity, G) of
		[{_,Bulge}|_] -> Bulge;
		_ -> 0
	end.

%****************************************************************************
% drawSegment([{10,X1}|[{10,X2}|_]], [{20,Y1}|[{20,Y2}|_]], [{42,B1}|_]) 
% Draw a LWPOLYLINE segment
%****************************************************************************
drawSegment([{10,X1}|[{10,X2}|_]],[{20,Y1}|[{20,Y2}|_]],[{42,B1}|_]) when 
		(B1 > 0.000063) or (B1 < -0.000063) -> % Arc ahead
	Cbce = cotbce(B1), 
	Ycen = ycenter(X1,X2,Y1,Y2,Cbce),
	Xcen = xcenter(X1,X2,Y1,Y2,Cbce),
	Rad = radius(X1,Y1,Xcen,Ycen),
	St_ang = fixang(ang(X1,Y1,Xcen,Ycen)), 
	End_ang = fixang(ang(X2,Y2,Xcen,Ycen)),
	io:format("ctx.arc(~.3f,~.3f,~.3f,~.3f,~.3f,~p);~n",[Xcen,Ycen,Rad,St_ang,End_ang,B1<0]); 
drawSegment([{10,X1}|_],[{20,Y1}|_],_) -> 
	io:format("ctx.lineTo(~.3f,~.3f);~n",[X1,Y1]).

%****************************************************************************************
% Function drawSegment(B2,X1,Y1,X2,Y2)
% Draw a polyline segment
%****************************************************************************************
drawSegment(B2,X1,Y1,X2,Y2) when
			(B2 > 0.000063) or (B2 < -0.000063) -> 
	Cbce = cotbce(B2), 
	Ycen = ycenter(X1,X2,Y1,Y2,Cbce),
	Xcen = xcenter(X1,X2,Y1,Y2,Cbce), 
	Rad = radius(X1,Y1,Xcen,Ycen),
	St_ang = fixang(ang(X1,Y1,Xcen,Ycen)), 
	End_ang = fixang(ang(X2,Y2,Xcen,Ycen)),
	io:format("ctx.arc(~.3f,~.3f,~.3f,~.3f,~.3f,~p);~n",[Xcen,Ycen,Rad,St_ang,End_ang,B2<0]); 
drawSegment(_,_,_,X2,Y2) -> % This is a line segment
	io:format("ctx.lineTo(~.3f,~.3f);~n",[X2,Y2]).

%****************************************************************************************
% Function fixBulgelist(_Entity,X,X)
% Fill up with G42's until list length is same as G10's
%****************************************************************************************
fixBulgelist(_Entity,X,X) -> ok;
fixBulgelist(Entity,X,Y) -> insert(Entity, {42,0}), fixBulgelist(Entity,X,Y+1).

%****************************************************************************************
% Function append_startpoint(List) 
% If an Lwpline is closed we need to append the last path segment
%****************************************************************************************
append_startpoint(List) ->
	L = hd(List),
	L1 = reverse(List),
	L2 = [L|L1],
	reverse(L2).

%****************************************************************************
% Fill or stroke the lwpolyline
%****************************************************************************
doLWPoly(Closed,_FirstVertex,[],[],[]) ->
	case Closed of 
		1 -> io:format("ctx.fill();~n");
		_ -> io:format("ctx.stroke();~n")
	end;
doLWPoly(Closed,FirstVertex,G42list,G10list,G20list) ->
	[_|G42tail] = G42list,
	[{10,_X1}|G10tail] = G10list,
	[{20,_Y1}|G20tail] = G20list,
	case FirstVertex of
		1 -> 	io:format("ctx.beginPath(); ~n"),
				doLWPoly(Closed,0,G42list,G10list,G20list);
		_ ->  drawSegment(G10list,G20list,G42list),
				doLWPoly(Closed,0,G42tail,G10tail,G20tail)
	end.

