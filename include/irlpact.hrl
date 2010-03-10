-record(product_definition, {
				 market_type, 
			     num_markets,
			     market_id,
			     contract_symbol,
			     trading_status,
			     order_price_denominator,
			     increment_price,
			     increment_qty,
			     lot_size,
			     market_desc,
			     maturity_year,
			     marturity_month,
			     maturity_day,
			     is_spread,
			     is_crack_spread,
			     primary_market_id,
			     secondary_market_id,
			     is_options,
			     option_type,
			     strike_price,
			     second_strike,
			     deal_price_denominator,
			     min_qty,
			     unit_qty,
			     currency,
			     min_strike_price,
			     max_strike_price,
			     increment_strike_price,
			     num_decimals_strike_price,
			     min_options_price,
			     max_options_price,
			     increment_options_price,
			     num_decimals_options_price,
			     tick_value,
			     allow_options,
			     cleared_alias,
			     allow_implied,
			     options_expiration_year,
			     options_expiration_month,
			     options_expiration_day,
			     min_price,
			     max_price,
			     product_id,
			     product_name,
			     hub_id,
			     hub_alias,
			     strip_id,
			     strip_name		  
			    }).

-record(market_snapshot, {
				 market_type, 
			     market_id,
			     trading_status,
			     volume,
			     block_volume,
			     efs_volume,
			     efp_volume,
			     open_interest,
			     opening_price,
			     settlement_price,
			     high,
			     low,
			     vwap,
			     num_of_order_entries,
			     last_trade_price,
			     last_trade_quantity,
			     last_trade_date_time,
			     settle_price_date_time
			    }).
			    
-record(market_snapshot_update, {			% aka market statistics message
			     market_id,
			     volume,
			     block_volume,
			     efs_volume,
			     efp_volume,
			     high,
			     low,
			     vwap,
				 date_time
			    }).
			    
-record(open_interest, {
				 market_id,
				 open_interest,
				 open_interest_change,
				 date_time
				}).

-record(order, {
				 is_snapshot=false,			% received in a market_snapshot_order, if so sent_time will be undefined, if not market_type will be undefined
				 market_type, 
			     market_id,
			     order_id,
			     order_sequence_id,
			     side, 
			     price,
			     quantity,
			     is_implied,
			     is_rfq,
			     order_entry_date_time,
			     sent_time
			    }).

-record(deleted_order, {
			     market_id,
			     order_id,
			     sent_time,
			     security_type				% 1 for futures, 2 for options
			    }).			    

-record(trade, {
			     market_id,
			     order_id,
			     is_system_priced_leg,
			     price,
			     quantity,
			     block_trade_type,
			     transact_date_time,
			     sent_time,
				 system_priced_leg_type
			    }).
			    
-record(cancelled_trade, {
			     market_id,
			     order_id,
			     price,
			     quantity,
			     block_trade_type,
			     date_time
			    }).			    