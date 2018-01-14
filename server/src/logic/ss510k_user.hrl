%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 一月 2018 22:03
%%%-------------------------------------------------------------------
-author("yaohong").
-ifndef(_ss510k_user_h__).
-define(_ss510k_user_h__, 0).


-record(ss510k_user, {
	seat_number :: integer(),           %%座位号
	hand_card_list :: [integer()],      %%手牌
	the_card_list1 :: [integer()],      %%出的牌
	the_card_list2 :: [integer()],      %%出的牌2(一圈结束后，降the_card_list1移到the_card_list2)
	current_score :: integer(),         %%本局的得分
	total_score :: integer()            %%总得分
	}).


-endif.