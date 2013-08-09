-module(svg).
-author(skvamme).
-compile(export_all).
-import(lists, [reverse/1]).
-import(ets,[lookup/2,insert/2]).
-import(math,[pi/0,pow/2,sqrt/1,atan2/2,sin/1,cos/1]).

%% ToDo: 
%% Done:
%% 120123	First version													S Kvamme

%****************************************************************************
% Print the header section in the html file
%****************************************************************************
print_body({X1,Y1,X2,Y2}) ->
%	io:format("<svg  id=\"svg\" viewBox=\"0 0 220 146\" shape-rendering=\"auto\" xmlns=\"http://www.w3.org/2000/svg\">~n",[]),
	io:format("<svg  id=\"svg\" width=\"~.3f\" height=\"~.3f\" shape-rendering=\"auto\" xmlns=\"http://www.w3.org/2000/svg\"~n",[X2-X1,Y2-Y1]),
	io:format("style=\"position:absolute; top:0; left:0; z-index:1\">~n",[]),
	io:format("<g  transform=\"scale(1 -1) translate(0,-~.3f)\">~n",[Y2-Y1]),
	io:format("<title>Layer 1</title>~n",[]).


%****************************************************************************************
% Function print_endbody()
%****************************************************************************************
print_endbody() ->
	io:format("</g>~n"),
	io:format("</svg>~n").


%****************************************************************************************
% Function print_entity({_,Entity name,Entity},_) 
%****************************************************************************************
print_entity({_,"POINT",Entity},_) ->
[{_,X1}|_] = lookup(Entity, 10),
[{_,Y1}|_] = lookup(Entity, 20),
io:format("~.3f, ~.3f~n",[X1,Y1]);

print_entity({_,"LINE",Entity},_) ->
	[{_,X1}|_] = lookup(Entity, 10),
	[{_,Y1}|_] = lookup(Entity, 20),
	[{_,X2}|_] = lookup(Entity, 11),
	[{_,Y2}|_] = lookup(Entity, 21),
	[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	io:format("<line x1=\"~.3f\" y1=\"~.3f\" x2=\"~.3f\" y2=\"~.3f\" fill=\"none\" stroke-width=\"1\" stroke=\"rgb(~B,~B,~B)\" />~n",[X1,Y1,X2,Y2,Pen,Pen,Pen]);

print_entity({_,"ARC",Entity},_) ->
	[{_,X1}|_] = lookup(Entity, 10),
	[{_,Y1}|_] = lookup(Entity, 20),
	[{_,Radius}|_] = lookup(Entity, 40),
	[{_,Startangle}|_] = lookup(Entity, 50),
	[{_,Endangle}|_] = lookup(Entity, 51),
	[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	{Xs,Ys} = polarToRect(X1, Y1, Radius, Startangle),
	{Xe,Ye} = polarToRect(X1, Y1, Radius, Endangle),
	io:format("<path d=\"M ~.3f ~.3f A ~.3f ~.3f 0 0 1 ~.3f ~.3f\" fill=\"none\" stroke-width=\"1\" stroke=\"rgb(~B,~B,~B)\" />~n",[Xs,Ys,Radius,Radius,Xe,Ye,Pen,Pen,Pen]);

print_entity({_,"CIRCLE",Entity},_) ->
	[{_,X1}|_] = lookup(Entity, 10),
   [{_,Y1}|_] = lookup(Entity, 20),
	[{_,Radius}|_] = lookup(Entity, 40),
   [{_,Pen}|_] = reverse(lookup(Entity, 62)),
	io:format("<circle cx=\"~.3f\" cy=\"~.3f\" r=\"~.3f\" stroke-width=\"1\" fill='none' stroke=\"rgb(~B,~B,~B)\" />~n",[X1,Y1,Radius,Pen,Pen,Pen]);

print_entity({_,"TEXT",Entity},_) ->
	[{_,X1}|_] = lookup(Entity, 10),
   [{_,Y1}|_] = lookup(Entity, 20),
	[{_,String}|_] = lookup(Entity, 1),
	[{_,Size}|_] = lookup(Entity, 40),
   [{_,Pen}|_] = reverse(lookup(Entity, 62)),
	Translate = -1 * ((2 * Y1)),
	io:format("<g transform='scale(1 -1) translate(0,~.3f)')>~n",[Translate]),
	io:format("<text x='~.3f' y='~.3f' font-family='Verdana' font-size='~.3f' fill='rgb(~B,~B,~B)' >~s</text>~n",[X1,Y1,Size,Pen,Pen,Pen,String]),
	io:format("</g>~n",[]);
	
print_entity({_,"LWPOLYLINE",Entity},_) ->
	[{_,Pen}|_] = reverse(lookup(Entity, 62)), 
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
   doLWPoly(Pen,Closed,1,Bulgelist1,G10list1,G20list1);

print_entity({_,_Name,_Entity},_) -> ok,
		List = ets:tab2list(_Entity),
		io:format("~p ~p~n", [_Name,List]).

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
doLWPoly(Pen,Closed,_FirstVertex,[],[],[]) ->
	case Closed of 
		1 -> io:format("z\" fill=\"rgb(~B,~B,~B)\" />~n",[Pen,Pen,Pen]);
		_ -> io:format("\" fill=\"none\" stroke-width=\"1\" stroke=\"rgb(~B,~B,~B)\" />~n",[Pen,Pen,Pen])
	end;
doLWPoly(Pen,Closed,FirstVertex,G42list,G10list,G20list) ->
	[_|G42tail] = G42list,
	[{10,X1}|G10tail] = G10list,
	[{20,Y1}|G20tail] = G20list,
	case FirstVertex of
		1 -> 	io:format("<path d=\"M ~.3f ~.3f ",[X1,Y1]),
				doLWPoly(Pen,Closed,0,G42list,G10list,G20list);
		_ ->  drawSegment(G10list,G20list,G42list),
				doLWPoly(Pen,Closed,0,G42tail,G10tail,G20tail)
	end.


%****************************************************************************
% drawSegment([{10,X1}|[{10,X2}|_]], [{20,Y1}|[{20,Y2}|_]], [{42,B1}|_]) 
% Draw an LWPOLYLINE segment
%****************************************************************************
drawSegment([{10,X1}|[{10,X2}|_]],[{20,Y1}|[{20,Y2}|_]],[{42,B1}|_]) when 
		(B1 > 0.000063) or (B1 < -0.000063) -> % Arc ahead
	Cbce = cotbce(B1), 
	Ycen = ycenter(X1,X2,Y1,Y2,Cbce),
	Xcen = xcenter(X1,X2,Y1,Y2,Cbce),
	Rad = radius(X1,Y1,Xcen,Ycen),
	Clockwise = case B1<0 of
							true -> 0;
							_ -> 1
						end,
	io:format("L ~.3f ~.3f A ~.3f ~.3f 0 0 ~B ~.3f ~.3f~n",[X1,Y1,Rad,Rad,Clockwise,X2,Y2]);
drawSegment([{10,X1}|_],[{20,Y1}|_],_) -> 
	io:format("L ~.3f ~.3f ",[X1,Y1]).

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
	io:format("A ~.3f ~.3f 0 0 0 ~.3f ~.3f~n",[Rad,Rad,X2,Y2]);
drawSegment(_,_,_,X2,Y2) -> % This is a line segment
	io:format("L ~.3f ~.3f~n",[X2,Y2]).


%****************************************************************************************
% Math Functions 
%****************************************************************************************
rtod(R) -> R * 180 / pi().

dtor(D) -> D * pi() / 180.

polarToRect(Xc, Yc, R, Adeg) ->
  Arad = dtor(Adeg),
  X = Xc + R * cos(Arad),
  Y = Yc + R * sin(Arad),
  {X,Y}.
  
cotbce(B) -> ((1 / B) - B) / 2.

xcenter(X1,X2,Y1,Y2,Cbce) -> (((Y2 - Y1) * Cbce * -1) + X1 + X2) / 2.

ycenter(X1,X2,Y1,Y2,Cbce) -> (((X2 - X1) * Cbce) + Y1 + Y2) / 2.	

radius(X1,Y1,Xcen,Ycen) -> T = pow((Xcen - X1),2) + pow((Ycen - Y1),2),
	sqrt(T).

ang(X1,Y1,Xcen,Ycen) -> atan2((Y1 - Ycen),(X1 - Xcen)).

fixang(Ang) when Ang < 0.0 -> Ang + (2 * pi());
fixang(Ang) -> Ang.

