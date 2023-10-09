#pos(flame11@1,{put(me, 5,4,blue),put(me, 5,3,blue)},{put(me, 1,7,blue), put(me, 1,8,blue), put(me, 2,4,blue), put(me, 2,5,blue), put(me, 3,4,blue), put(me, 3,5,blue), put(me, 4,5,blue), put(me, 4,6,blue), put(me, 6,3,blue), put(me, 6,4,blue)},{
  location(me, 1,6,purple). location(me, 1,5,green). location(me, 1,4,red). location(me, 4,4,blue). location(me, 1,3,blue). location(me, 2,3,green). location(me, 3,3,purple). location(me, 4,3,green). location(me, 1,2,blue). location(me, 2,2,blue). location(me, 3,2,green). location(me, 4,2,purple). location(me, 5,2,purple). put(me, 6,2,purple). location(me, 1,1,green). location(me, 2,1,green). location(me, 3,1,purple). location(me, 4,1,red). location(me, 5,1,red). location(me, 6,1,red).
  put(me, 5,4,blue) :- put(me, 5,3,blue).
  put(me, 5,3,blue) :- put(me, 5,4,blue).
  next( red, red). nextnext( purple, purple).
}).

#pos(flame12@10,{put(me, 2,4,red),put(me, 3,4,red)},{put(me, 1,7,red), put(me, 1,8,red), put(me, 2,5,red), put(me, 3,5,red), put(me, 4,5,red), put(me, 4,6,red), put(me, 5,5,red), put(me, 5,6,red), put(me, 6,3,red), put(me, 6,4,red)},{
  location(me, 1,6,purple). location(me, 1,5,green). location(me, 1,4,red). location(me, 4,4,blue). location(me, 5,4,blue). location(me, 1,3,blue). location(me, 2,3,green). location(me, 3,3,purple). location(me, 4,3,green). location(me, 5,3,blue). location(me, 1,2,blue). location(me, 2,2,blue). location(me, 3,2,green). location(me, 4,2,purple). location(me, 5,2,purple). location(me, 6,2,purple). location(me, 1,1,green). location(me, 2,1,green). location(me, 3,1,purple). location(me, 4,1,red). location(me, 5,1,red). location(me, 6,1,red).
  put(me, 2,4,red) :- put(me, 3,4,red).
  put(me, 3,4,red) :- put(me, 2,4,red).
  next_col(me, purple, purple). nnext_col(me, red, blue).
}).

%モード宣言

%長さ１の仮説は排除
%#bias("
% :- not {body(_)} != 1.
%").

%#bias("
%  :- not head(_).
%").


%putとlocationの宣言
#modeh(1, put( var(p), var(cell_x), var(cell_y), var(color) ) ).
#modeb(1, location( var(p), var(cell_x), var(cell_y), var(color) ) , (positive) ).

#modeb(1, can_put( var(player), var(cell_x1), var(cell_y1), var(cell_x1), var(cell_y2))).
#modeb(1, can_put( var(player), var(cell_x1), var(cell_y1), var(cell_x2), var(cell_y2))).

#maxv(6).