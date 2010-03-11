%%%-------------------------------------------------------------------
%%% File    : irlpact_util.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : Utility functions for iRLpact
%%%
%%% Created : 10 Mar 2010 by Chris Duesing <chris.duesing@gmail.com>
%%%-------------------------------------------------------------------
-module(irlpact_util).

%% API
-export([apply_update/2]).

%% records
-include_lib("irlpact.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: apply_update(MarketSnapshot, Update) -> {ok, MarketSnapshot} | error
%%           MarketSnapshot = #market_snapshot{}
%%           Update = #market_snapshot_update{} | #open_interest{}
%% Description: After a trade or other market event, an update will be
%%              sent to the client, this takes the original market snapshot
%%              and updates it with the new information. returns the new combined record
%%--------------------------------------------------------------------
apply_update(#market_snapshot{market_id=MarketId} = MarketSnapshot, #market_snapshot_update{market_id=UpdatedMarketId} = Update) when MarketId == UpdatedMarketId -> 
    #market_snapshot_update{ volume=Volume, block_volume=BlockVolume, efs_volume=EFSVolume, efp_volume=EFPVolume, high=High, low=Low, vwap=VWAP } = Update,
    UpdatedMarketSnapshot = MarketSnapshot#market_snapshot{ volume=Volume, block_volume=BlockVolume, efs_volume=EFSVolume, efp_volume=EFPVolume, high=High, low=Low, vwap=VWAP },
    {ok, UpdatedMarketSnapshot};

apply_update(#market_snapshot{market_id=MarketId} = MarketSnapshot, #open_interest{market_id=OIMarketId} = OpenInterest) when MarketId == OIMarketId -> 
    #open_interest{ open_interest=OpenInterest } = OpenInterest,
    UpdatedMarketSnapshot = MarketSnapshot#market_snapshot{ open_interest=OpenInterest },
    {ok, UpdatedMarketSnapshot};

apply_update(_MarketSnapshot, _Update) ->
    error.

%%====================================================================
%% Internal functions
%%====================================================================
