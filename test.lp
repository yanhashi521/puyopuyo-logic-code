%%%%%
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
player(me; you).

%% ����
%% �e��ŁCmax������āC���̒��ł�min,max���v�Z����D
%% ��̒�`
col(1..6). %��
row(1..10). %12?

%% �񂲂Ƃ̍���
height(P, X, HX):- player(P), col(X), #max{ Y : location(P,X, Y, _C) } = HX.

max_height(P, H):- player(P), #max{ Y:location(P,X,Y,C) } = H.
min_height(P, H):- player(P), #min{ HX:height(P, _X, HX) } = H.

%#show max_height/2.
%#show min_height/2.
%#show maxheight/2.
%#show minheight/2.

%% �F��
color(red;blue;purple;green).
num_cells(P, N):- player(P), #count{ X,Y:location(P,X,Y,C) } = N. %�S��
num_cells(P, C, N):- player(P), color(C), #count{ X,Y:location(P,X,Y,C) } = N. %�F��

%#show num_cells/2.
%#show num_cells/3.

%% �q����
%%%��=�����F�̘A���̒�`
same_color(P, X, Y, C, X+1, Y):- color(C), location(P, X, Y, C), location(P, X+1, Y, C). 
same_color(P, X, Y, C, X-1, Y):- color(C), location(P, X, Y, C), location(P, X-1, Y, C). 
same_color(P, X, Y, C, X, Y+1):- color(C), location(P, X, Y, C), location(P, X, Y+1, C). 
same_color(P, X, Y, C, X, Y-1):- color(C), location(P, X, Y, C), location(P, X, Y-1, C). 

%%% �p�X�̒�`�F�iP,X,Y�j���瓯���F�ŘA�����Ă���Z��(X1,Y1)��Ԃ��C���[�W

path(P,X,Y,C, X,  Y):- location(P, X, Y, C). %�N�_���̂��̂�����
path(P,X,Y,C, X1,Y1):- same_color(P,X,Y,C, X1,Y1). %���ڂȂ����Ă���
path(P,X,Y,C, X1,Y1):- same_color(P,X,Y,C, Xt,Yt), path(P,Xt,Yt,C, X1,Y1). %�ԐړI�ɂȂ����Ă���

%%% (P, X, Y)����A�����Ă���Z���̐��𐔂���
count_adj_cells(P, X, Y, C, Cnt):- location(P, X, Y, C), #count{ X1, Y1:path(P, X, Y, C, X1, Y1) } = Cnt.

%%% �A������p�����W�v
cnt(0..3).
count_adj_cells(P, Cnt, N):- player(P), cnt(Cnt), #count{ X,Y:count_adj_cells(P, X, Y, _C, Cnt) } = N.

%#show count_adj_cells/3.
%#show count_adj_cells/5.

%%% ���΂���ʒu�H
%%% 1�Ŕ��΂���ʒu�E�Ƃ肠�����̒�`

count_for_fire(P, X, Y, C, Cnt):- count_adj_cells(P, X, Y, C, Cnt).
count_for_fire(P, X, Y, C, 0  ):- col(X), row(Y), color(C), player(P), cnt(Cnt), not count_adj_cells(P, X, Y, C, Cnt).

%�͈͊O�̏ꏊ��0�ɂ���
count_for_fire(P, XMin-1, Y, C, 0):- #min{X:col(X)} = XMin, row(Y), color(C), player(P).
count_for_fire(P, XMax+1, Y, C, 0):- #max{X:col(X)} = XMax, row(Y), color(C), player(P).
count_for_fire(P, X, -1, C, 0):- col(X), color(C), player(P). 

%P�ɂƂ��āC�����ɒu����fire����D
can_fire(P, X, Y, C):- player(P), col(X), row(Y), color(C),
	    not location(P, X, Y, _),
	    1{ Y = 0 ; location(P, X, Y-1, _C)}, %��ԉ� or �F�͖�킸�ŉ�������
	    
	    count_for_fire(P, X+1, Y, C, C1),
	    count_for_fire(P, X-1, Y, C, C2),
	    count_for_fire(P, X, Y-1, C, C3),
	    C1 + C2 + C3 >= 3.

%#show count_for_fire/5.
#show can_fire/4.

%���l�ɁCgtr���ł���ʒu�Ȃǂ��v�Z�ł���΁D�D�H
%%%%%%%%%%%%