location(me,1,6,purple).
location(me,1,5,green).
location(me,1,4,red).
location(me,1,3,blue).
location(me,1,2,blue).
location(me,1,1,green).
location(me,2,3,green).
location(me,2,2,blue).
location(me,2,1,green).
location(me,3,3,purple).
location(me,3,2,green).
location(me,3,1,purple).
location(me,4,4,blue).
location(me,4,3,green).
location(me,4,2,purple).
location(me,4,1,red).
location(me,5,4,blue).
location(me,5,3,blue).
location(me,5,2,purple).
location(me,5,1,red).
location(me,6,2,purple).
location(me,6,1,red).

put(me,2,4,red) :- put(me,3,4,red).
put(me,3,4,red) :- put(me,2,4,red).
next(me, purple, purple). nextnext(me, red, blue).
chain_count(me, 0).
maxheight(me,6).
minheight(me,2).

location(you,1,6,blue).
location(you,1,5,blue).
location(you,1,4,red).
location(you,1,3,blue).
location(you,1,2,blue).
location(you,1,1,green).
location(you,2,4,red).
location(you,2,3,green).
location(you,2,2,blue).
location(you,2,1,green).
location(you,3,4,red).
location(you,3,3,purple).
location(you,3,2,green).
location(you,3,1,purple).
location(you,4,1,purple).
location(you,5,5,purple).
location(you,5,4,green).
location(you,5,3,green).
location(you,5,2,blue).
location(you,5,1,red).
location(you,6,4,purple).
location(you,6,3,purple).
location(you,6,2,red).
location(you,6,1,red).  

next(you, purple, purple). nextnext(you, red, blue).
chain_count(you, 0).
maxheight(you,6).
minheight(you,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%BY OZAKI: check with "test_ex.lp"
%各種のドメイン
player(me; you). %プレイヤ
col(1..6). %列
row(1..12). %行
color(red;blue;purple;green). %色

% e^{ctx}として与えるもの
% セルの基本表現: location(Player, X, Y, C). 
% 今の色：cur_col( purple, blue ). 
% 次の色：next_col( red, green).
% 次の次の色：nnext_col( blue,green).

%
%関連BK
%
cur_col( X, Y ):- cur_col( Y, X ).
next_col( X, Y ):- next_col( Y, X ).
nnext_col( X, Y ):- nnext_col( Y, X).


%
%置き方を表す述語 x 2 （引数の数を減らすため）
%
put_v(X,C1,C2):- if_put(me, X, Y1, C1,   X, Y2, C2).
put_h(X,C1,C2):- if_put(me, X, Y1, C1, X+1, Y2, C2).


%%%%%%%%%%%% 繋がり %%%%%%%%%%
%置ける位置の確認(meのみを対象とする)
%縦置き
can_put(P, X, Y, X, Y+1):- P = me, player(P), col(X), row(Y), color(C),
	   not location(P, X, Y, _), 
	   1{ Y = 0; location(P, X, Y-1, _) }1.
%横置き
can_put(P, X, Y1, X+1, Y2):- P = me, player(P), col(X), row(Y1), row(Y2),
	   not location(P, X,   Y1, _ ), 1{ Y1 = 0; location(P,  X, Y1-1, _ ) }1,
   	   not location(P, X+1, Y2, _ ), 1{ Y2 = 0; location(P,X+1, Y2-1, _ ) }1.

%世界を分ける（置く位置は一つ --> 世界を分ける）
%仮にここに置いた場合
1{ if_put(P, X1,Y1,C1,X2,Y2,C2):can_put(P,X1,Y1,X2,Y2), cur_col(C1,C2) } 1.


%
%以下では，
% meは，T=0, T=1, T=2, .. 
% youは，T=0
%

%置く前の状態：T = 0
location(0, P, X,Y,C):- location(P,X,Y,C).   %置く前の状態(me,you)

%置いた状態： T=1 / if_putを考えるときのみ T=1を考える
location(1, P, X1, Y1, C1):- if_put(P, X1, Y1, C1, X2, Y2, C2). 
location(1, P, X2, Y2, C2):- if_put(P, X1, Y1, C1, X2, Y2, C2). 
location(1, P,  X,  Y,  C):- if_put(P, X1, Y1, C1, X2, Y2, C2),
	    	    	     location(0,P,X,Y,C), P = me.

%%%辺 = 同じ色の連続の定義（時刻TとプレイヤPでの縛り）
same_color(T, P, X, Y, C, X+1, Y):- color(C), location(T, P, X, Y, C), location(T, P, X+1, Y, C). 
same_color(T, P, X, Y, C, X-1, Y):- color(C), location(T, P, X, Y, C), location(T, P, X-1, Y, C). 
same_color(T, P, X, Y, C, X, Y+1):- color(C), location(T, P, X, Y, C), location(T, P, X, Y+1, C). 
same_color(T, P, X, Y, C, X, Y-1):- color(C), location(T, P, X, Y, C), location(T, P, X, Y-1, C). 

%パス(reachable)の定義
reachable(T,P,X,Y,C, X,  Y):- location(T,P,X,Y,C). %起点そのものを入れる
reachable(T,P,X,Y,C, X1,Y1):- same_color(T,P,X,Y,C, X1,Y1). %直接つながっている
reachable(T,P,X,Y,C, X1,Y1):- same_color(T,P,X,Y,C, Xt,Yt), reachable(T,P,Xt,Yt,C, X1,Y1). %間接的につながっている

%
%セルX,Y,Cの周囲とどれだけ同じ色があるか？（自分を含めている）
%
cnt_adj_cells(T,P,X,Y,C, Cnt):- location(T,P,X,Y,C), #count{ X1,Y1:reachable(T,P,X,Y,C,X1,Y1) } = Cnt.

%
%連鎖
%
chain_limit(10).
step(1). %置いた状態：T = 1 --> この状態でdeletedを考える．

%時刻T+1において，location(T,P,X,Y)は消える
deleted(T+1, P, X, Y):- step(T), cnt_adj_cells(T, P, X, Y, C, Cnt), Cnt >= 4, P = me.

%何か消えるセルがある--> その時間を作る．
step(T+1):- step(T), deleted( T+1, _P, _, _), chain_limit(L), T < L.


%次のタイムステップでの状態
location(T+1, P, X, Y-Ydel, C):-
	P = me,
	step(T), T < #sup,
	location(T, P, X, Y, C), not deleted( T+1, P, X, Y),
	#count{Y1:cell_deleted_in_lower(T+1, P, X, Y, Y1)} = Ydel,
	chain_limit(L), T < L.
	
%Yより下で削除されるセル
cell_deleted_in_lower(T+1,P,X,Y,  0):- step(T), T < #sup, Y = 0, location(T,P,X,Y, _), not deleted(T+1,P,X,Y).
cell_deleted_in_lower(T+1,P,X,Y, Y1):- step(T), T < #sup, location(T,P,X,Y,_), not deleted(T+1,P,X,Y), deleted(T+1,P,X,Y1), Y1 < Y, Y > 0.

max_step(MAX):- #max{ T:step(T) } = MAX. %時間ステップ
n_chain(MAX-1):- max_step(MAX). %連鎖数

%max_step --> #infへ（連鎖後の集約に利用する）
location(#sup, P, X, Y, C):- max_step(MAX), location(MAX, P, X, Y, C), P=me.

%
%fireの有無
%
is_fired :- n_chain(N), N > 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 盤面の状況：考慮するのは，T=0 と #sup == max_step(T)のときのみ．
%time & player
tp(    0, P ):- player(P).
tp( #sup, me ). 


%列Xごとの高さ
height(T, P, X, HX):- tp(T,P), col(X), #max{ Y : location(T, P, X, Y, _C) } = HX.

%列の高さの最大値
max_height(T, P, H):- tp(T,P), #max{ HX:height(T, P, _X, HX) } = H.
min_height(T, P, H):- tp(T,P), #min{ HX:height(T, P, _X, HX) } = H.

%高さH以上の列数C (≒危険度？)
n_cols_heigher(T, P, H, Cnt):- tp(T,P), row(H), #count{ X: height_eq(T, P, X, H) } = Cnt, Cnt > 0.
height_eq(T, P, X, R):- height(T, P, X, H), row(R), H >= R. %高さR以上の列数


%
%セルの数
%
num_cells(T, P, N):- tp(T,P), #count{ X,Y:location(T,P,X,Y,C) } = N. 

%セル数の差分
diff_num_cells(P, N0-NMax):- P = me,
     max_step(T), num_cells(T, P, NMax), num_cells(0, P, N0). 

%
% 色別のセルの数
%
num_cells(T, P, C, N):- color(C), tp(T,P), #count{ X,Y:location(T,P,X,Y,C) } = N. %step in {0, max_step}での色別セル数

diff_num_cells(P, C, N0-NMax):- P = me, max_step(T), num_cells(T, P, C, NMax), num_cells(0, P, C, N0). %色別セル数の差分

%
% 色別の（昇順）ランク / step in {0, max_step}での色数順位
%
rank_color(T, P, C, R+1):- color(C), tp(T,P), #count{ C2:less_color(T, P, C, C2) } = R. 
less_color(T, P, C1, C2):- 
	      color(C1), num_cells(T, P, C1, N1),
	      color(C2), num_cells(T, P, C2, N2), N1 > N2.

%
%塊の集計
%
cnt(1..3).

%時刻Tにおいて，プレイヤPは，周囲にCnt個同一色のあるセルをN個持つ
agg_cnt_adj_cells(T, P, Cnt, N):- cnt(Cnt), tp(T,P), #count{ X,Y:cnt_adj_cells(T, P, X, Y, _C, Cnt) } = N.


%
%定石:GTR
%|xo
%|xxo
%|oo
%L-----
s_gtr(T,P,X,Y,C1):-
 location(T,P,X,Y,C1), location(T,P,X+1,Y,C1), location(T,P,X+2,Y+1,C1), location(T,P,X+1,Y+2,C1),
 location(T,P,X,Y+1,C2), location(T,P,X,Y+2,C2), location(T,P,X+1,Y+1,C2), C1 != C2.
in_gtr(T, P, X,   Y,  C):- s_gtr(T,P,X,Y,C).
in_gtr(T, P, X+1, Y,  C):- s_gtr(T,P,X,Y,C).
in_gtr(T, P, X+2, Y+1,C):- s_gtr(T,P,X,Y,C).
in_gtr(T, P, X+1, Y+2,C):- s_gtr(T,P,X,Y,C).

in_formula(T,P,X,Y,C):- in_gtr(T,P,X,Y,C).

%
%定石：ysb2 (y_shaped_base2)
%| o
%| xoo
%| o
%L-----
s_ysb2(T,P,X,Y,C1):-
  location(T,P,X,Y,C1), location(T,P,X+1,Y+1,C1), location(T,P,X+2,Y+1,C1),location(T,P,X,Y+2,C1),
  location(T,P,X,Y+1,C2), C1 != C2.
in_ysb2(T,P,X,  Y,   C):- s_ysb2(T,P,X,Y,C).
in_ysb2(T,P,X+1,Y+1, C):- s_ysb2(T,P,X,Y,C).
in_ysb2(T,P,X+2,Y+1, C):- s_ysb2(T,P,X,Y,C).
in_ysb2(T,P,X,  Y+2, C):- s_ysb2(T,P,X,Y,C).

%
%定石：ysb3 (y_shaped_base3)
%| o
%| xooo
%| o
%L-----
s_ysb3(T,P,X,Y,C1):-
  location(T,P,X,Y,C1), location(T,P,X+1,Y+1,C1), location(T,P,X+2,Y+1,C1),location(T,P,X+3,Y+1,C1),location(T,P,X,Y+2,C1),
  location(T,P,X,Y+1,C2), C1 != C2.
in_ysb3(T,P,X,  Y,   C):- s_ysb3(T,P,X,Y,C).
in_ysb3(T,P,X+1,Y+1, C):- s_ysb3(T,P,X,Y,C).
in_ysb3(T,P,X+2,Y+1, C):- s_ysb3(T,P,X,Y,C).
in_ysb3(T,P,X+3,Y+1, C):- s_ysb3(T,P,X,Y,C).
in_ysb3(T,P,X,  Y+2, C):- s_ysb3(T,P,X,Y,C).

in_ysb(T,P,X,Y,C):- in_ysb2(T,P,X,Y,C).
in_ysb(T,P,X,Y,C):- in_ysb3(T,P,X,Y,C).
in_formula(T,P,X,Y,C):- in_ysb(T,P,X,Y,C).

%定石：階段済み
%| o  o  
%|o    o
%|o    o
%|o    o
%L---------

s_stair_r1(T,P,X,Y,C):- location(T,P,X,Y,C), location(T,P,X,Y+1,C), location(T,P,  X,Y+2,C), location(T,P,X+1,Y+3,C).
s_stair_r2(T,P,X,Y,C):- location(T,P,X,Y,C), location(T,P,X,Y+1,C), location(T,P,X+1,Y+2,C), location(T,P,X+1,Y+3,C).
s_stair_l1(T,P,X,Y,C):- location(T,P,X,Y,C), location(T,P,X,Y+1,C), location(T,P,  X,Y+2,C), location(T,P,X-1,Y+3,C).
s_stair_l2(T,P,X,Y,C):- location(T,P,X,Y,C), location(T,P,X,Y+1,C), location(T,P,X-1,Y+2,C), location(T,P,X-1,Y+3,C).

in_stair_r1(T,P,X,    Y,C):- s_stair_r1(T,P,X,Y,C).
in_stair_r1(T,P,X,  Y+1,C):- s_stair_r1(T,P,X,Y,C).
in_stair_r1(T,P,X,  Y+2,C):- s_stair_r1(T,P,X,Y,C).
in_stair_r1(T,P,X+1,Y+3,C):- s_stair_r1(T,P,X,Y,C).

in_stair_r2(T,P,X,    Y,C):- s_stair_r2(T,P,X,Y,C).
in_stair_r2(T,P,X,  Y+1,C):- s_stair_r2(T,P,X,Y,C).
in_stair_r2(T,P,X+1,Y+2,C):- s_stair_r2(T,P,X,Y,C).
in_stair_r2(T,P,X+1,Y+3,C):- s_stair_r2(T,P,X,Y,C).

in_stair_l1(T,P,X,    Y,C):- s_stair_l1(T,P,X,Y,C).
in_stair_l1(T,P,X,  Y+1,C):- s_stair_l1(T,P,X,Y,C).
in_stair_l1(T,P,X,  Y+2,C):- s_stair_l1(T,P,X,Y,C).
in_stair_l1(T,P,X-1,Y+3,C):- s_stair_l1(T,P,X,Y,C).

in_stair_l2(T,P,X,    Y,C):- s_stair_l2(T,P,X,Y,C).
in_stair_l2(T,P,X,  Y+1,C):- s_stair_l2(T,P,X,Y,C).
in_stair_l2(T,P,X-1,Y+2,C):- s_stair_l2(T,P,X,Y,C).
in_stair_l2(T,P,X-1,Y+3,C):- s_stair_l2(T,P,X,Y,C).

in_stair(T,P,X,Y,C):- in_stair_r1(T,P,X,Y,C).
in_stair(T,P,X,Y,C):- in_stair_r2(T,P,X,Y,C).
in_stair(T,P,X,Y,C):- in_stair_l1(T,P,X,Y,C).
in_stair(T,P,X,Y,C):- in_stair_l2(T,P,X,Y,C).

in_formula(T,P,X,Y,C):- in_stair(T,P,X,Y,C).

%定石：sw(カギ積み，ハサミ積み）
%| uo      ou
%|  uo    ou
%| uo      ou
%| uo      ou
%L----------------
%かぎ積み 挟み込み
s_sw_r(T,P,X,Y,C1,C2):-
  location(T,P,X,  Y,  C1), location(T,P,X+1, Y,  C2), C1 != C2,
  location(T,P,X,  Y+1,C1), location(T,P,X+1, Y+1,C2), 
  location(T,P,X+1,Y+2,C1), location(T,P,X+2, Y+2,C2), 
  location(T,P,X,  Y+3,C1), location(T,P,X+1, Y+3,C2).

in_sw_r(T,P,X,     Y, C):- s_sw_r(T,P,X,Y,C,_).
in_sw_r(T,P,X,   Y+1, C):- s_sw_r(T,P,X,Y,C,_).
in_sw_r(T,P,X+1, Y+2, C):- s_sw_r(T,P,X,Y,C,_).
in_sw_r(T,P,X,   Y+3, C):- s_sw_r(T,P,X,Y,C,_).
in_sw_r(T,P,X+1,   Y, C):- s_sw_r(T,P,X,Y,_,C).
in_sw_r(T,P,X+1, Y+1, C):- s_sw_r(T,P,X,Y,_,C).
in_sw_r(T,P,X+2, Y+2, C):- s_sw_r(T,P,X,Y,_,C).
in_sw_r(T,P,X+1, Y+3, C):- s_sw_r(T,P,X,Y,_,C).

s_sw_l(T,P,X,Y,C1,C2):-
  location(T,P,X,  Y,  C1), location(T,P,X+1, Y,  C2), C1 != C2,
  location(T,P,X,  Y+1,C1), location(T,P,X+1, Y+1,C2), 
  location(T,P,X-1,Y+2,C1), location(T,P,X,   Y+2,C2), 
  location(T,P,X,  Y+3,C1), location(T,P,X+1, Y+3,C2).

in_sw_l(T,P,X,     Y, C):- s_sw_l(T,P,X,Y,C,_).
in_sw_l(T,P,X,   Y+1, C):- s_sw_l(T,P,X,Y,C,_).
in_sw_l(T,P,X-1, Y+2, C):- s_sw_l(T,P,X,Y,C,_).
in_sw_l(T,P,X,   Y+3, C):- s_sw_l(T,P,X,Y,C,_).
in_sw_l(T,P,X+1,   Y, C):- s_sw_l(T,P,X,Y,_,C).
in_sw_l(T,P,X+1, Y+1, C):- s_sw_l(T,P,X,Y,_,C).
in_sw_l(T,P,X,   Y+2, C):- s_sw_l(T,P,X,Y,_,C).
in_sw_l(T,P,X+1, Y+3, C):- s_sw_l(T,P,X,Y,_,C).

in_sw(T,P,X,Y,C):- in_sw_r(T,P,X,Y,C).
in_sw(T,P,X,Y,C):- in_sw_l(T,P,X,Y,C).
in_formula(T,P,X,Y,C):- in_sw(T,P,X,Y,C).

%
%座布団：左壁のみ
%
s_zabuton(T,P,0,Y,C):- location(T,P,0,Y,C), location(T,P,1,Y,C), location(T,P,2,Y,C).
in_zabuton(T,P,0,Y,C):- s_zabuton(T,P,0,Y,C).
in_zabuton(T,P,1,Y,C):- s_zabuton(T,P,0,Y,C).
in_zabuton(T,P,2,Y,C):- s_zabuton(T,P,0,Y,C).

in_formula(T,P,X,Y,C):- in_zabuton(T,P,X,Y,C).

%
%%% 各定石の数 n_formura(T,P,定石の種類，Count)
%
n_formula(T,P, gtr, N):- tp(T,P), #count{ X,Y:s_gtr(T,P,X,Y,_) } = N.
n_formula(T,P, ysb, N1+N2):- tp(T,P),
  #count{ X1,Y1:s_ysb2(T,P,X1,Y1,_) } = N1, #count{X2,Y2:s_ysb3(T,P,X2,Y2,_)} = N2.

n_formula(T,P, stair, N1+N2+N3+N4):- tp(T,P),
  #count{ X1,Y1:s_stair_r1(T,P,X1,Y1,_) } = N1, #count{X2,Y2:s_stair_r2(T,P,X2,Y2,_)} = N2,
  #count{ X3,Y3:s_stair_l1(T,P,X3,Y3,_) } = N3, #count{X4,Y4:s_stair_l2(T,P,X4,Y4,_)} = N4.

n_formula(T,P, sw, N1+N2):- tp(T,P),
  #count{ X1,Y1:s_sw_r(T,P,X1,Y1,_,_) } = N1, #count{ X2,Y2:s_sw_l(T,P,X2,Y2,_,_) } = N2.

n_formula(T,P, zabuton, N):- tp(T,P), #count{ X,Y:s_zabuton(T,P,X,Y,_) } = N.

n_formula(T, P, N_gtr + N_ysb + N_stair + N_sw + N_zabuton):-
  n_formula(T,P,gtr,N_gtr), n_formula(T,P,ysb,N_ysb), n_formula(T,P,stair,N_stair),
  n_formula(T,P,sw,N_sw), n_formula(T,P,sw,N_zabuton).


n_in_formula(T, P, gtr,     Cnt):- tp(T,P), #count{ X,Y:in_gtr(T,P,X,Y,_) } = Cnt.
n_in_formula(T, P, ysb,     Cnt):- tp(T,P), #count{ X,Y:in_ysb(T,P,X,Y,_)} = Cnt.
n_in_formula(T, P, stair,   Cnt):- tp(T,P), #count{ X,Y:in_stair(T,P,X,Y,_)} = Cnt.
n_in_formula(T, P, sw,      Cnt):- tp(T,P), #count{ X,Y:in_sw(T,P,X,Y,_) } = Cnt.
n_in_formula(T, P, zabuton, Cnt):- tp(T,P), #count{ X,Y:in_zabuton(T,P,X,Y,_) } = Cnt.
n_in_formula(T, P, Cnt):- tp(T,P), #count{X,Y:in_formula(T,P,X,Y,_) } = Cnt.

%変化
diff_n_in_formula(P, N0-NMax):- P = me, max_step(T), n_in_formula(T, P, NMax), n_in_formula(0, P, N0).

%%%%%%%%%%%%%%
