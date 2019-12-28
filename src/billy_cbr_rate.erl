-module(billy_cbr_rate).

-export([get_rate/1]).

-include("../include/rate.hrl").

get_rate(#{currency_alpha := CurrencyAlpha}) ->
    case get_rate_rec(#{currency_alpha => CurrencyAlpha}) of
	[] ->
	    StubRateRec = case CurrencyAlpha of
			      <<"USD">> -> #rate{
					      currency_number=840,
					      currency_alpha=CurrencyAlpha,
					      nominal=1,
					      value=56.37
					     };
			      <<"EUR">> -> #rate{
					      currency_number=978,
					      currency_alpha=CurrencyAlpha,
					      nominal=1,
					      value=68.90
					     };
			      <<"RUB">> -> #rate{
					      currency_number=643,
					      currency_alpha=CurrencyAlpha,
					      nominal=1,
					      value=1
					     }
			  end,
	    {ok, rate_rec_to_map(StubRateRec)};
	[Rate] -> 
	    {ok, rate_rec_to_map(Rate)}
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================


get_rate_rec(#{currency_alpha := CurrencyAlpha}) ->
    Pattern = #rate{_ = '_', currency_alpha = CurrencyAlpha},
    F = fun() ->
 		mnesia:match_object(cbrates, Pattern, read)
 	end,
    mnesia:activity(transaction, F).


rate_rec_to_map(Rate) when is_record(Rate, rate) ->
    #{
       currency_number => Rate#rate.currency_number, 
       currency_alpha => Rate#rate.currency_alpha,
       nominal => Rate#rate.nominal,
       value => Rate#rate.value
     }.
