\begin{code}
module Unshare (unshare) where

#include "HsVersions.h"

import StgSyn
import Id
import Outputable
import DynFlags         ( DynFlags(..))
import Data.List

unshare :: DynFlags -> [StgBinding] -> [StgBinding]
unshare dflags binds = map (unshareBinding dflags) binds

unshareBinding :: DynFlags -> StgBinding -> StgBinding
unshareBinding dflags bind = case bind of
  StgNonRec x rhs -> StgNonRec x (unshareRhs dflags rhs)
  StgRec xrhss    -> StgRec $ map (\(x, rhs) -> (x, unshareRhs dflags rhs)) xrhss

unshareRhs :: DynFlags -> StgRhs -> StgRhs
unshareRhs dflags rhs = case rhs of
  StgRhsClosure ccs b_info fvs update_flag srt args expr
    -> StgRhsClosure ccs b_info fvs flag' srt args (unshareExpr dflags expr)
    where
    flag' = case calledFun expr of
        Just name -> if "myenum" `isSuffixOf` showPpr dflags name
                then 
                        --pprTrace "XXX info: " (ppr (unfoldingInfo (Var.idInfo name))) $
                        case update_flag of Updatable -> ReEntrant ; _ -> update_flag
                else update_flag
        _ -> update_flag
  StgRhsCon ccs con args
    -> StgRhsCon ccs con args

calledFun :: StgExpr -> Maybe Id
calledFun expr = case expr of
  StgApp f _args -> Just f
  StgLit _l -> Nothing
  StgConApp _dc _args -> Nothing
  StgOpApp _op _args _ty -> Nothing
  StgLam _xs e -> calledFun e
  StgCase _e _case_lives _alts_lives _bndr _srt _alt_ty [(_, _, _, e')] -> calledFun e'
  StgCase _e _case_lives _alts_lives _bndr _srt _alt_ty _alts -> Nothing
  StgLet _bind e -> calledFun e
  StgLetNoEscape _live_in_let _live_in_bind _bind e
    -> calledFun e
  StgSCC _cc _bump_entry _push_cc e -> calledFun e
  StgTick _mod _tick_n e -> calledFun e

unshareExpr :: DynFlags -> StgExpr -> StgExpr
unshareExpr dflags e = case e of
  -- Particularly important where (##) is concerned (Note [The nullary (# #) constructor])
  StgApp f args ->
        --pprTrace "name: " (ppr (Var.varName f)) $
        StgApp f args
  StgLit l -> StgLit l
  StgConApp dc args -> StgConApp dc args
  StgOpApp op args ty -> StgOpApp op args ty
  StgLam xs e -> StgLam xs (unshareExpr dflags  e)
  StgCase e case_lives alts_lives bndr srt alt_ty alts
    -> StgCase (unshareExpr dflags e) case_lives alts_lives bndr srt alt_ty $
        map (\(con, bndrs, uses, e) -> (con, bndrs, uses, unshareExpr dflags e)) alts
  StgLet bind e -> StgLet (unshareBinding dflags bind) (unshareExpr dflags e)
  StgLetNoEscape live_in_let live_in_bind bind e
    -> StgLetNoEscape live_in_let live_in_bind (unshareBinding dflags bind) (unshareExpr dflags e)
  StgSCC cc bump_entry push_cc e -> StgSCC cc bump_entry push_cc (unshareExpr dflags e)
  StgTick mod tick_n e -> StgTick mod tick_n (unshareExpr dflags e)
\end{code}
