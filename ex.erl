-module(ex).
-author(skvamme).
-compile(export_all).
-import(lists, [reverse/1]).
-import(ets,[lookup/2,insert/2]).
-import(math,[pi/0,pow/2,sqrt/1,atan2/2,sin/1,cos/1]).
-define(absolute,ok).  %% Define 'absolute' to place graphic at an absolute position in the window coordinate system
% If absolute is undefined, each X-coordinate and Y-coordinate will be paired together with an X+ and an Y+ to make
% it simple to relocate (translate) the drawing in the window.
-ifdef(absolute).
	-define(DX, "").
	-define(DY, "").
-else.
	-define(DX, "X+").
	-define(DY, "Y+").
-endif.

%% ToDo: 
%% Done:
%% 120123	First version													S Kvamme

%****************************************************************************
% Fill or stroke a polyline
%****************************************************************************
doPoly(Ttable,1,_Flags,Bulge,_Bulge1,Pen,X1,Y1) when Bulge /= 0 -> % This is FirstVertex of an Arc
	insert(Ttable,{10,X1}),    % Save point
	insert(Ttable,{20,Y1}),
	insert(Ttable,{42,Bulge}), % Save bulge
	insert(Ttable,{firstvertex,0}),
	io:format("xDo(Display,ePolyFillArc(Win, Pen~p, [",[Pen]);
doPoly(Ttable,1,1,0,_,Pen,X1,Y1) -> % This is FirstVertex of a closed pline
	io:format("xDo(Display,eFillPoly(Win, Pen~B, complex, origin, [mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++"~B),~n",[Pen,round(X1),round(Y1)]),
	insert(Ttable,{firstvertex,0});
doPoly(Ttable,1,_,0,_,Pen,X1,Y1) -> % This is FirstVertex of an open pline
	io:format("xDo(Display,ePolyLine(Win, Pen~B, origin, [mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++ "~B),~n",[Pen,round(X1),round(Y1)]),	
	insert(Ttable,{firstvertex,0});
doPoly(Ttable,0,_,_,B2,Pen,X1,Y1) when B2 > 0 -> % This is SecondVertex of an Arc
	[{_,X2}|_] = lookup(Ttable, 10), 
	[{_,Y2}|_] = lookup(Ttable, 20), 
	insert(Ttable,{10,X1}),    % Save point
	insert(Ttable,{20,Y1}),
	insert(Ttable,{42,B2}), % Save bulge
	Cbce = cotbce(B2), 
	Ycen = ycenter(X1,X2,Y1,Y2,Cbce),
	Xcen = xcenter(X1,X2,Y1,Y2,Cbce), 
	Rad = radius(X1,Y1,Xcen,Ycen),
	St_ang1 = ang(X1,Y1,Xcen,Ycen), 
	End_ang1 = ang(X2,Y2,Xcen,Ycen),
	St_ang = fixang(St_ang1), 
	End_ang = fixang(End_ang1),	
	StAng = rtod(St_ang), 
	EnAng = rtod(End_ang),
	% Now we have a start and an end angle in degrees	
	EndAng1 = fixang2(StAng,EnAng), % fix end angle if start is more than end
	StAng1 = fixang1(StAng), % Do the angles negative
	Ang = fixang1(EndAng1 - StAng),
	Ang1 = angle(B2,Ang), % same as PostScript arcn if bulge is negative
	Xbbox = Xcen - Rad, Ybbox = Ycen - Rad,
	Method = closeArc(0),	
	io:format("xDo(Display,~s(Win, Pen~B, [mkArc(" ++ ?DX ++ "~B," ++ ?DY ++ "~B,~B,~B,~B,~B)])),~n",
	  [Method,Pen,round(Xbbox),round(Ybbox),round(Rad*2),round(Rad*2),round(64*StAng1),round(64*Ang1)]); 
doPoly(_Ttable,0,_,_,_,_Pen,X1,Y1) -> % This is a vertex of a pline
	io:format("mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++ "~B),~n",[round(X1),round(Y1)]).


%****************************************************************************
% Write an arc
%****************************************************************************
tryArc(Pen,Closed,[{10,X1}|[{10,X2}|_]],[{20,Y1}|[{20,Y2}|_]],[{42,B1}|_],Arctable) when B1 /= 0 ->
	io:format("mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++ "~B),~n",[round(X1),round(Y1)]),
	Cbce = cotbce(B1), 
	Ycen = ycenter(X1,X2,Y1,Y2,Cbce),
	Xcen = xcenter(X1,X2,Y1,Y2,Cbce),
	Rad = radius(X1,Y1,Xcen,Ycen),
	St_ang1 = ang(X1,Y1,Xcen,Ycen), 
	End_ang1 = ang(X2,Y2,Xcen,Ycen),
	St_ang = fixang(St_ang1), 
	End_ang = fixang(End_ang1),	
	StAng = rtod(St_ang), 
	EnAng = rtod(End_ang),
	% Now we have a start and an end angle in degrees
	EndAng1 = fixang2(StAng,EnAng), % fix end angle if start is more than end
	StAng1 = fixang1(StAng), % Do the start ang negative
	Ang = fixang1(EndAng1 - StAng),
	Ang1 = angle(B1,Ang), % same as PostScript arcn if bulge is negative
	Xbbox = Xcen - Rad, Ybbox = Ycen - Rad,
	insert(Arctable,{arc, io_lib:format("xDo(Display,~s(Win, Pen~B, [mkArc(" ++ ?DX ++ "~B," ++ ?DY++ "~B,~B,~B,~B,~B)])),~n",
	  [closeArc(Closed),Pen,round(Xbbox),round(Ybbox),round(Rad*2),round(Rad*2),round(64*StAng1),round(64*Ang1)])}); 
tryArc(_Pen,_Closed,[{10,X1}|_],[{20,Y1}|_],_,_) -> io:format("mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++ "~B),~n",[round(X1),round(Y1)]).

firstVertex(1,1,Pen) ->
	io:format("xDo(Display,eFillPoly(Win, Pen~B, complex, origin, [",[Pen]);
firstVertex(1,0,Pen) ->
	io:format("xDo(Display,ePolyLine(Win, Pen~B, origin, [",[Pen]);	
firstVertex(0,_,_) -> ok.

closeArc(1) -> "ePolyFillArc";
closeArc(_) -> "ePolyArc".

%****************************************************************************
% Fill or stroke an LWpolyline
%****************************************************************************
doLWPoly(_Pen,_Closed,_FirstVertex,[],[],[],_) -> io:format("])),~n",[]);
doLWPoly(Pen,Closed,FirstVertex,G42list,G10list,G20list,Arctable) ->	
	firstVertex(FirstVertex,Closed,Pen),
	tryArc(Pen,Closed,G10list,G20list,G42list,Arctable),
	[_|G42tail] = G42list,[_|G10tail] = G10list,[_|G20tail] = G20list,
	doLWPoly(Pen,Closed,0,G42tail,G10tail,G20tail,Arctable).



isClosed([]) -> 0;
isClosed(_) -> 1.
	
%****************************************************************************
% Print an entity
%****************************************************************************
%print_entity({_,"POINT",Entity},_,_) ->
%	[{_,X1}|_] = lookup(Entity, 10),[{_,Y1}|_] = lookup(Entity, 20),
%	io:format("xDo(Display,ePolyLine(Win, Pen, origin, [mkPoint(~B,~B),mkPoint(~B,~B)])),~n",
%	  [round(X1),round(Y1),round(X1),round(Y1)]);
print_entity({_,"TRACE",Entity},_,_) -> print_entity({0,"SOLID",Entity},0,0);
print_entity({_,"SOLID",Entity},_,_) ->
	[{_,X1}|_] = lookup(Entity, 10),[{_,Y1}|_] = lookup(Entity, 20),
	[{_,X2}|_] = lookup(Entity, 12),[{_,Y2}|_] = lookup(Entity, 22),
	[{_,X3}|_] = lookup(Entity, 13),[{_,Y3}|_] = lookup(Entity, 23),
	[{_,X4}|_] = lookup(Entity, 11),[{_,Y4}|_] = lookup(Entity, 21),
	[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	io:format("xDo(Display,eFillPoly(Win, Pen~B, convex, origin, [mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++ "~B),mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++ "~B),
		mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++ "~B),mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++ "~B)])),~n",
	  [Pen,round(X1),round(Y1),round(X2),round(Y2),round(X3),round(Y3),round(X4),round(Y4)]);
print_entity({_,"LINE",Entity},_,_) ->
	[{_,X1}|_] = lookup(Entity, 10),[{_,Y1}|_] = lookup(Entity, 20),
	[{_,X2}|_] = lookup(Entity, 11),[{_,Y2}|_] = lookup(Entity, 21),
	[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	io:format("xDo(Display,ePolyLine(Win, Pen~B, origin, [mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++ "~B),mkPoint(" ++ ?DX ++ "~B," ++ ?DY ++ "~B)])),~n",
	  [Pen,round(X1),round(Y1),round(X2),round(Y2)]);
print_entity({_,"ARC",Entity},_,_) ->
	[{_,X1}|_] = lookup(Entity, 10),[{_,Y1}|_] = lookup(Entity, 20),
	[{_,Radius}|_] = lookup(Entity, 40),
	[{_,Startangle}|_] = lookup(Entity, 50),
	[{_,Endangle}|_] = lookup(Entity, 51),
	[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	Endangle1 = fixang2(Startangle,Endangle),
	Endangle2 = Endangle1 - Startangle,
	io:format("xDo(Display,ePolyArc(Win, Pen~B, [mkArc(" ++ ?DX ++ "~B," ++ ?DY ++ "~B,~B,~B,~B,~B)])),~n",
	  [Pen,round(X1 - Radius),round(Y1 - Radius),round(Radius*2),round(Radius*2),
	  	round(fixang1(Startangle)*64),round(fixang1(Endangle2)*64)]);
print_entity({_,"ELLIPSE",Entity},_,_) -> %% dxf2erl:start(dialer_slider_menu.dxf).
	[{_,X1}|_] = lookup(Entity, 10),[{_,Y1}|_] = lookup(Entity, 20),
	[{_,RadiusX}|_] = lookup(Entity, 11),
	[{_,RadiusY}|_] = lookup(Entity, 21),
	[{_,Startangle}|_] = lookup(Entity, 41),
	[{_,Endangle}|_] = lookup(Entity, 42),
	[{_,Ratio}|_] = lookup(Entity, 40),
	[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	Startangle_deg = rtod(Startangle),
	Endangle_deg = rtod(Endangle),
	Endangle_deg1 = fixang2(Startangle_deg,Endangle_deg),
	Endangle_deg2 = Endangle_deg1 - Startangle_deg,
	case abs(RadiusX) < abs(RadiusY) of
	 true -> io:format("xDo(Display,ePolyArc(Win, Pen~B, [mkArc(" ++ ?DX ++ "~B," ++ ?DY ++ "~B,~B,~B,~B,~B)])),~n",
	  [Pen,round(X1 - abs(RadiusY * Ratio)),round(Y1 - abs(RadiusY)),round(abs(RadiusY*2*Ratio)),round(abs(RadiusY*2)),
	  	round(fixangelips(Startangle_deg,RadiusY)*64),round(fixangelips(Endangle_deg2,RadiusY)*64)]); 	
	 _ -> io:format("xDo(Display,ePolyArc(Win, Pen~B, [mkArc(" ++ ?DX ++ "~B," ++ ?DY ++ "~B,~B,~B,~B,~B)])),~n",
	  [Pen,round(X1 - abs(RadiusX)),round(Y1 - abs(RadiusX*Ratio)),round(abs(RadiusX*2)),round(abs(RadiusX*2*Ratio)),
	  	round(fixangelips(Startangle_deg,RadiusX)*64),round(fixangelips(Endangle_deg2,RadiusX)*64)])
	 end;

print_entity({_,"CIRCLE",Entity},_,_) ->
	[{_,X1}|_] = lookup(Entity, 10),[{_,Y1}|_] = lookup(Entity, 20),
	[{_,Radius}|_] = lookup(Entity, 40),[{_,Pen}|_] = reverse(lookup(Entity, 62)),
	io:format("xDo(Display,ePolyArc(Win, Pen~B, [mkArc(" ++ ?DX ++ "~B," ++ ?DY ++ "~B,~B,~B,0,64*360)])),~n",
	  [Pen,round(X1-Radius),round(Y1-Radius),round(Radius*2),round(Radius*2)]);
print_entity({_,"POLYLINE",Entity},Ttable,_) ->
	[{_,Pen}|_] = reverse(lookup(Entity, 62)), 
	Closed = isClosed(lookup(Entity, 70)), 
	insert(Ttable,{firstvertex,1}),
	insert(Ttable,{flags,Closed}),
	insert(Ttable,{42,0}),
	insert(Ttable,{pen,Pen});
print_entity({_,"VERTEX",Entity},Ttable,_) ->
	[{_,X1}|_] = lookup(Entity, 10),
	[{_,Y1}|_] = lookup(Entity, 20),
	case  lookup(Entity, 42) of
		[{_,Bulge}|_] -> ok;
		_ -> Bulge = 0
	end,
	[{_,FV}|_] = lookup(Ttable, firstvertex),
	[{_,Flags}|_] = lookup(Ttable, flags),
	[{_,Pen}|_] = lookup(Ttable, pen),
	[{_,Bulge1}|_] = lookup(Ttable, 42), % Bulge group on previous vertex
	doPoly(Ttable,FV,Flags,Bulge,Bulge1,Pen,X1,Y1);
print_entity({_,"SEQEND",_Entity},Ttable,_) ->
	ets:delete_all_objects(Ttable),
	io:format("])),~n",[]);
print_entity({_,"LWPOLYLINE",Entity},_,Arctable) ->
	[{_,Pen}|_] = reverse(lookup(Entity, 62)), 
	Closed = isClosed(lookup(Entity, 70)),
	G10list = lookup(Entity, 10),
	G20list = lookup(Entity, 20),
	fixBulgelist(Entity,length(lookup(Entity, 10)),length(lookup(Entity, 42))),
	Bulgelist = lookup(Entity, 42),
   doLWPoly(Pen,Closed,1,Bulgelist,G10list,G20list,Arctable);

print_entity({_,_Name,_Entity},_,_) -> ok.
%		List = ets:tab2list(Entity),
%		io:format("~p ~p~n", [Name,List]).

% Fill up with G42's until list length is same as G10's
fixBulgelist(_Entity,X,X) -> ok;
fixBulgelist(Entity,X,Y) -> insert(Entity, {42,0}), fixBulgelist(Entity,X,Y+1).
%****************************************************************************
% Math functions
%****************************************************************************
rtod(R) -> R * 180 / pi().

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

fixang1(Ang) when Ang > 0 -> Ang * -1;
fixang1(Ang) -> Ang.

fixangelips(Ang,R)when Ang > 0, R < 0 -> Ang * -1;
fixangelips(Ang,_R) -> Ang.

fixang2(Stang,Endang) when Stang > Endang -> Endang + 360;
fixang2(_,Endang) -> Endang.

angle(B1,Angle) when B1 < 0 -> 360 - (-1*Angle);
angle(_,Angle) -> Angle.
