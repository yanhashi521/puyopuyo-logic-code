
%%#bias("
%  %1 { head(p); head(q) } 1.
%  { weak_body(B) } :- possible_body1(B). 
%  possible_body1(r+1).
%  possible_body1(s).
%  possible_body1(naf(s)).
%  %:- body(naf(B)), body(B). 
%  weak_body(can_put( var__(1), var__(1) ) ). weight(1). priority(2).
%").

%#bias("
% :- not {weak_body(_)} != 1.
%").

%#bias("
%  player(me).
%  player(you).
%  1 {weak_body( can_put( P, var__(2), var__(3), var__(2), var__(5) ) ) ; weak_body( can_put( P, var__(2), var__(3), var__(4), var__(5) ) )}1 
%    :- player(P).
%  1 { weak_body( put( P, var__(6), var__(7), var__(8) ) ) ; weak_body( put( P, var__(6), var__(7), var__(8) ) ) ; weak_body( put( P, var__(6), var__(7), var__(8) ) )} 1
%    :- player(P) .
%
%  1 {weight(1); weight(var__(1))} 1.

%  priority(1).
%").

%#modeo(1, can_put( const(player), var(cell_x1), var(cell_y1), var(cell_x1), var(cell_y1)+1 ) , (positive) ).
%#modeo(1, can_put( const(player), var(cell_x1), var(cell_y1), var(cell_x1)+1, var(cell_y2) ) , (positive) ).
%#modeo(1, var(cell_y1) != var(cell_y2) ).
%#modeo(1, var(cell_x1) == var(cell_x1) ).



%%#bias("
%  1 {weak_body( can_put( var__(1), var__(2), var__(3), var__(2) ) ) ; weak_body( put( var__(1), var__(2), var__(3), var__(4) ) )}1
%").
#constant(player,me).#constant(player,you).

%%#modeb(1, can_put( var(player), var(cell_x1), var(cell_x1)+1 ) ).
%%#modeb(1, can_put( var(player), var(cell_x1), var(cell_y1) ) ).


%putとlocationの宣言
#modeo(1, put( var(player), var(cell_x), var(cell_y), var(color) ) ).

#modeo(1, location( var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%%#modeo(1, can_put( const(player), var(cell_x1), var(cell_y1), var(cell_x1)+1, var(cell_y2) ) , (positive) ).

#modeo(1, can_put_tate( var(player), var(cell_x1), var(cell_y1) ) , (positive) ).
#modeo(1, can_put_yoko( var(player), var(cell_x1), var(cell_y2) ) , (positive) ).

%#modeo(1, location( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%%#modeb(1, n_chain( var(num) ) ).
%%#modeo(1, is_fired ).

#modeo(1, max_height( const(turn), var(player), var(height) ) , (positive) ).
#modeo(1, min_height( const(turn), var(player), var(height) ) , (positive) ).

#modeo(1, n_cols_heigher( const(turn), var(player), var(height) ,var(height_num)) , (positive) ).

%色関係なしに盤面にぷよがいくつあるか
#modeo(1, num_cells( const(turn), var(player), var(all_puyo_num) ) , (positive) ).
%色ごとで盤面にぷよが何個あるか
#modeo(1, num_cells( const(turn), var(player), var(color), var(puyo_color_num) ) , (positive) ).
%色関係なしに発火前と発火後のぷよの数の差
#modeo(1, diff_num_cells( var(player), var(all_diff_num) ) , (positive) ).
%発火前と発火後の色ごとのぷよの数の差
#modeo(1, diff_num_cells( var(player), var(color), var(diff_color_num) ) , (positive) ).

%色の多い順でrankがついているのでpenaltyに出るようにした。
#modeo(1, rank_color( const(turn), var(player), var(color), var(rank) ) , (positive) ).

#modeo(1, agg_cnt_adj_cells( const(turn), var(player), var(count), var(adjnum) ) , (positive) ).

%%%%%%%%%%%%%%
%定石に関係するモード宣言
%%%%%%%%%%%%%%
%#modeo(1, s_gtr( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_gtr( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%#modeo(1, s_ysb2( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_ysb3( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_ysb2( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_ysb3( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_ysb( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%#modeo(1, s_l_shape_l( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_l_shape_r( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_l_shape_l( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_l_shape_r( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_l_shape( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%#modeo(1, s_bigL( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_bigL( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%#modeo(1, s_stair_r1( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_stair_r2( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_stair_l1( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_stair_l2( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_stair_r1( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_stair_r2( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_stair_l1( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_stair_l2( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_stair( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%#modeo(1, s_sw_r1( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_sw_r2( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_sw_l1( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_sw_l2( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_sw_r1( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_sw_r2( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_sw_l1( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_sw_l2( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_sw( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%#modeo(1, s_zabuton( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_zabuton( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%#modeo(1, s_flat_l( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_flat_r( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_flat_l( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_flat_r( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_flat( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%#modeo(1, s_flat_3_l( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_flat_3_c( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, s_flat_3_r( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_flat_3_l( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_flat_3_c( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_flat_3_r( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).
%#modeo(1, in_flat_3( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%cell(x,y)のぷよが定石に含まれている。（定石の種類は関係なし。）
%#modeo(1, in_formula( const(turn), var(player), var(cell_x), var(cell_y), var(color) ) , (positive) ).

%specific_num = 特定の定石の数
%all_num = 全ての定石の数
%定石をconstにすると割り当てが爆発したのでvarで対応。
#constant(stack, gtr). #constant(stack, ysb). #constant(stack, l_shape). 
#constant(stack, bigL). #constant(stack, stair). #constant(stack, sw). 
#constant(stack, zabuton). #constant(stack, flat).
#constant(turn, 1). #constant(turn, max).
%OOの定石が何個あるか。その個数をweightにしている。
#modeo(1, n_formula( const(turn), var(player), var(stack), var(specific_num) ) , (positive) ).
%定石の種類関係なしに定石が何個かあるか。その個数をweightにしている。
#modeo(1, n_formula( const(turn), var(player),  var(all_num) ) , (positive) ).
%OOの定石に含まれているぷよの数は何個か。その個数をweightにしている。
#modeo(1, n_in_formula( const(turn), var(player), var(stack), var(specific_num) ) , (positive) ).
%定石の種類関係なしに定石に含まれているぷよの数が何個あるか。その個数をweightにしている。
#modeo(1, n_in_formula( const(turn), var(player), var(all_num) ) , (positive) ).


#bias("
  :- weak_body( n_formula( _, _, V1 ) ), weak_body( n_in_formula( _, _, V1 ) ).
  :- weak_body( n_formula( _, _, _, V1 ) ), weak_body( n_in_formula( _, _, _, V1 ) ).
  :- weak_body( n_formula( _, _, _ ) ), weak_body( n_formula( _, _, _, _ ) ).
  :- weak_body( n_in_formula( _, _, _ ) ), weak_body( n_in_formula( _, _, _, _ ) ).
").

#maxv(5).
#weight(1).
#weight(specific_num).
#weight(all_num).
#weight(adj_num).
#weight(rank).
#weight(all_puyo_num).
#weight(diff_color_num).
#weight(height_num).
#weight(height).

%%#maxrl(4).%max-rule-length仮説長の指定？
%%#maxbl(4).%リテラルの最大数
%%#maxp(3).%priorityの最大値の設定。

