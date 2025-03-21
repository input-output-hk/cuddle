{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Evaluate" #-}

module Conway where

import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Float (Double)
import GHC.Num (Integer)
import GHC.Show (Show (show))

default (Integer, Double)

conway :: Huddle
conway = collectFrom [block]

block :: Rule
block =
    "block"
        =:= arr
            [ a header
            , "transaction_bodies" ==> arr [0 <+ a transaction_body]
            , "transaction_witness_sets"
                ==> arr [0 <+ a transaction_witness_set]
            , "auxiliary_data_set"
                ==> mp [0 <+ asKey transaction_index ==> auxiliary_data]
            , "invalid_transactions" ==> arr [0 <+ a transaction_index]
            ]

transaction :: Rule
transaction =
    "transaction"
        =:= arr
            [ a transaction_body
            , a transaction_witness_set
            , a VBool
            , a (auxiliary_data / VNil)
            ]

transaction_index :: Rule
transaction_index = "transaction_index" =:= VUInt `sized` (2 :: Word64)

header :: Rule
header = "header" =:= arr [a header_body, "body_signature" ==> kes_signature]

header_body :: Rule
header_body =
    "header_body"
        =:= arr
            [ "block_number" ==> VUInt
            , "slot" ==> VUInt
            , "prev_hash" ==> hash32
            , "issuer_vkey" ==> vkey
            , "vrf_vkey" ==> vkey
            , "vrf_result" ==> vrf_cert
            , "block_body_size" ==> VUInt
            , "block_body_hash" ==> hash32
            , a operational_cert
            , a (arr [a protocol_version])
            ]

operational_cert :: Rule
operational_cert =
    "operational_cert"
        =:= arr
            [ "hot_vkey" ==> kes_vkey
            , "sequence_number" ==> VUInt
            , "kes_period" ==> VUInt
            , "sigma" ==> signature
            ]

protocol_version :: Named Group
protocol_version = "protocol_version" =:~ grp [a major_protocol_version, a VUInt]

next_major_protocol_version :: Rule
next_major_protocol_version = "next_major_protocol_version" =:= (10 :: Integer)

major_protocol_version :: Rule
major_protocol_version = "major_protocol_version" =:= (1 :: Integer)

transaction_body :: Rule
transaction_body =
    "transaction_body"
        =:= mp
            [ idx 0 ==> set transaction_index
            , idx 1 ==> arr [0 <+ a transaction_output]
            , idx 2 ==> coin
            , opt (idx 3 ==> VUInt)
            , opt (idx 4 ==> certificates)
            , opt (idx 5 ==> withdrawals)
            , opt (idx 7 ==> auxiliary_data_hash)
            , opt (idx 8 ==> VUInt)
            , opt (idx 9 ==> mint)
            , opt (idx 11 ==> script_data_hash)
            , opt (idx 13 ==> nonempty_set transaction_input)
            , opt (idx 14 ==> required_signers)
            , opt (idx 15 ==> network_id)
            , opt (idx 16 ==> transaction_output)
            , opt (idx 17 ==> coin)
            , opt (idx 18 ==> nonempty_set transaction_input)
            , opt (idx 19 ==> voting_procedures)
            , opt (idx 20 ==> proposal_procedures)
            , opt (idx 21 ==> coin)
            , opt (idx 22 ==> positive_coin)
            ]

voting_procedures :: Rule
voting_procedures =
    "voting_procedures"
        =:= mp [1 <+ "voter" ==> mp [1 <+ "gov_action_id" ==> voting_procedure]]

voting_procedure :: Rule
voting_procedure = "voting_procedure" =:= arr [a vote, a (anchor / VNil)]

proposal_procedure :: Rule
proposal_procedure =
    "proposal_procedure"
        =:= arr
            [ "deposit" ==> coin
            , a reward_account
            , a gov_action
            , a anchor
            ]

proposal_procedures :: Rule
proposal_procedures = "proposal_procedures" =:= nonempty_set proposal_procedure

certificates :: Rule
certificates = "certificates" =:= nonempty_set certificate

gov_action :: Rule
gov_action =
    "gov_action"
        =:= arr
            [ a
                ( parameter_change_action
                    / hard_fork_initiation_action
                    / treasury_withdrawals_action
                    / no_confidence
                    / update_committee
                    / new_constitution
                    / info_action
                )
            ]

policy_hash :: Rule
policy_hash = "policy_hash" =:= scripthash

parameter_change_action :: Named Group
parameter_change_action =
    "parameter_change_action"
        =:~ grp
            [ 0
            , a (gov_action_id / VNil)
            , a protocol_param_update
            , a (policy_hash / VNil)
            ]

hard_fork_initiation_action :: Named Group
hard_fork_initiation_action =
    "hard_fork_initiation_action"
        =:~ grp [1, a (gov_action_id / VNil), a (arr [a protocol_version])]

treasury_withdrawals_action :: Named Group
treasury_withdrawals_action =
    "treasury_withdrawals_action"
        =:~ grp [2, a (arr [asKey reward_account ==> coin / VInt]), a (policy_hash / VNil)]

no_confidence :: Named Group
no_confidence = "no_confidence" =:~ grp [3, a (gov_action_id / VNil)]

update_committee :: Named Group
update_committee =
    "update_committee"
        =:~ grp
            [ 4
            , a (gov_action_id / VNil)
            , a (set committee_cold_credential)
            , a (arr [asKey committee_cold_credential ==> epoch])
            , a unit_interval
            ]

new_constitution :: Named Group
new_constitution =
    "new_constitution"
        =:~ grp [5, a (gov_action_id / VNil), a constitution]

constitution :: Rule
constitution =
    "constitution"
        =:= arr
            [ a anchor
            , a (scripthash / VNil)
            ]

info_action :: Rule
info_action = "info_action" =:= int 6

voter :: Rule
voter =
    "voter"
        =:= arr [0, a addr_keyhash]
        / arr [1, a scripthash]
        / arr [2, a addr_keyhash]
        / arr [3, a scripthash]
        / arr [4, a addr_keyhash]

anchor :: Rule
anchor =
    "anchor"
        =:= arr
            [ "anchor_url" ==> url
            , "anchor_data_hash" ==> hash32
            ]

vote :: Rule
vote = "vote" =:= int 0 / int 1 / int 2

gov_action_id :: Rule
gov_action_id =
    "gov_action_id"
        =:= arr
            [ "transaction_id" ==> hash32
            , "gov_action_index" ==> VUInt
            ]

required_signers :: Rule
required_signers = "required_signers" =:= nonempty_set addr_keyhash

transaction_input :: Rule
transaction_input =
    "transaction_input"
        =:= arr
            [ "transaction_id" ==> hash32
            , "index" ==> VUInt
            ]

transaction_output :: Rule
transaction_output =
    "transaction_output"
        =:= legacy_transaction_output
        / post_alonzo_transaction_output

legacy_transaction_output :: Rule
legacy_transaction_output =
    "legacy_transaction_output"
        =:= arr
            [ a address
            , "amount" ==> value
            , opt ("datum_hash" ==> hash32)
            ]

post_alonzo_transaction_output :: Rule
post_alonzo_transaction_output =
    "post_alonzo_transaction_output"
        =:= arr
            [ idx 0 ==> address
            , idx 1 ==> value
            , opt (idx 2 ==> datum_option) -- datum option
            , opt (idx 3 ==> script_ref) -- script reference
            ]

script_data_hash :: Rule
script_data_hash = "script_data_hash" =:= hash32

certificate :: Rule
certificate =
    "certificate"
        =:= arr
            [ a
                ( stake_registration
                    / stake_deregistration
                    / stake_delegation
                    / pool_registration
                    / pool_retirement
                    / reg_cert
                    / unreg_cert
                    / vote_deleg_cert
                    / stake_vote_deleg_cert
                    / stake_reg_deleg_cert
                    / vote_reg_deleg_cert
                    / stake_vote_reg_deleg_cert
                    / auth_committee_hot_cert
                    / resign_committee_cold_cert
                    / reg_drep_cert
                    / unreg_drep_cert
                    / update_drep_cert
                )
            ]

stake_registration :: Named Group
stake_registration = "stake_registration" =:~ grp [0, a stake_credential]

stake_deregistration :: Named Group
stake_deregistration = "stake_deregistration" =:~ grp [0, a stake_credential]

stake_delegation :: Named Group
stake_delegation =
    "stake_delegation"
        =:~ grp [0, a stake_credential, a pool_keyhash]

-- POOL
pool_registration :: Named Group
pool_registration = "pool_registration" =:~ grp [3, a pool_params]

pool_retirement :: Named Group
pool_retirement = "pool_retirement" =:~ grp [4, a pool_keyhash, a epoch]

-- numbers 5 and 6 used to be the Genesis and MIR certificates respectively,
-- which were deprecated in Conway

-- DELEG
reg_cert :: Named Group
reg_cert = "reg_cert" =:~ grp [7, a stake_credential, a coin]

unreg_cert :: Named Group
unreg_cert = "unreg_cert" =:~ grp [8, a stake_credential, a coin]

vote_deleg_cert :: Named Group
vote_deleg_cert = "vote_deleg_cert" =:~ grp [9, a stake_credential, a drep]

stake_vote_deleg_cert :: Named Group
stake_vote_deleg_cert =
    "stake_vote_deleg_cert"
        =:~ grp [10, a stake_credential, a pool_keyhash, a drep]

stake_reg_deleg_cert :: Named Group
stake_reg_deleg_cert =
    "stake_reg_deleg_cert"
        =:~ grp [11, a stake_credential, a pool_keyhash, a coin]

vote_reg_deleg_cert :: Named Group
vote_reg_deleg_cert =
    "vote_reg_deleg_cert"
        =:~ grp [12, a stake_credential, a drep, a coin]

stake_vote_reg_deleg_cert :: Named Group
stake_vote_reg_deleg_cert =
    "stake_vote_reg_deleg_cert"
        =:~ grp [13, a stake_credential, a pool_keyhash, a drep, a coin]

-- GOVCERT
auth_committee_hot_cert :: Named Group
auth_committee_hot_cert =
    "auth_committee_hot_cert"
        =:~ grp [14, a committee_cold_credential, a committee_hot_credential]

resign_committee_cold_cert :: Named Group
resign_committee_cold_cert =
    "resign_committee_cold_cert"
        =:~ grp [15, a committee_cold_credential, a (anchor / VNil)]

reg_drep_cert :: Named Group
reg_drep_cert = "reg_drep_cert" =:~ grp [16, a drep_credential, a coin, a (anchor / VNil)]

unreg_drep_cert :: Named Group
unreg_drep_cert = "unreg_drep_cert" =:~ grp [17, a drep_credential, a coin]

update_drep_cert :: Named Group
update_drep_cert = "update_drep_cert" =:~ grp [18, a drep_credential, a (anchor / VNil)]

delta_coin :: Rule
delta_coin = "delta_coin" =:= VUInt

credential :: Rule
credential =
    "credential"
        =:= arr
            [0, a addr_keyhash]
        / arr [1, a scripthash]

drep :: Rule
drep =
    "drep"
        =:= sarr [0, a addr_keyhash]
        / sarr [1, a scripthash]
        / int 2 -- always abstain
        / int 3 -- always no confidence

stake_credential :: Rule
stake_credential = "stake_credential" =:= credential

drep_credential :: Rule
drep_credential = "drep_credential" =:= credential

committee_cold_credential :: Rule
committee_cold_credential = "committee_cold_credential" =:= credential

committee_hot_credential :: Rule
committee_hot_credential = "committee_hot_credential" =:= credential

pool_params :: Named Group
pool_params =
    "pool_params"
        =:~ grp
            [ "operator" ==> pool_keyhash
            , "vrf_keyhash" ==> vrf_keyhash
            , "pledge" ==> coin
            , "cost" ==> coin
            , "margin" ==> unit_interval
            , "reward_account" ==> reward_account
            , "pool_owners" ==> set addr_keyhash
            , "relays" ==> arr [0 <+ a relay]
            , "pool_metadata" ==> (pool_metadata / VNil)
            ]

port :: Rule
port = "port" =:= VUInt `le` 65535

ipv4 :: Rule
ipv4 = "ipv4" =:= VBytes `sized` (4 :: Word64)

ipv6 :: Rule
ipv6 = "ipv6" =:= VBytes `sized` (16 :: Word64)

dns_name :: Rule
dns_name = "dns_name" =:= VText `sized` (0 :: Word64, 128 :: Word64)

single_host_addr :: Named Group
single_host_addr =
    "single_host_addr"
        =:~ grp
            [ 0
            , a (port / VNil)
            , a (ipv4 / VNil)
            , a (ipv6 / VNil)
            ]

single_host_name :: Named Group
single_host_name =
    "single_host_name"
        =:~ grp
            [ 1
            , a (port / VNil)
            , a dns_name -- An A or AAAA DNS record
            ]

multi_host_name :: Named Group
multi_host_name =
    "multi_host_name"
        =:~ grp
            [ 2
            , a dns_name -- A SRV DNS record
            ]

relay :: Rule
relay =
    "relay"
        =:= arr
            [ a
                ( single_host_addr
                    / single_host_name
                    / multi_host_name
                )
            ]

pool_metadata :: Rule
pool_metadata = "pool_metadata" =:= arr [a url, a pool_metadata_hash]

url :: Rule
url = "url" =:= VText `sized` (0 :: Word64, 128 :: Word64)

withdrawals :: Rule
withdrawals = "withdrawals" =:= mp [1 <+ asKey reward_account ==> coin]

protocol_param_update :: Rule
protocol_param_update =
    "protocol_param_update"
        =:= mp
            [ opt (idx 0 ==> coin) -- minfee A
            , opt (idx 1 ==> coin) -- minfee B
            , opt (idx 2 ==> VUInt) -- max block body size
            , opt (idx 3 ==> VUInt) -- max transaction size
            , opt (idx 4 ==> VUInt) -- max block header size
            , opt (idx 5 ==> coin) -- key deposit
            , opt (idx 6 ==> coin) -- pool deposit
            , opt (idx 7 ==> epoch) -- maximum epoch
            , opt (idx 8 ==> VUInt) -- n_opt: desired number of stake pools
            , opt (idx 9 ==> nonnegative_interval) -- pool pledge influence
            , opt (idx 10 ==> unit_interval) -- expansion rate
            , opt (idx 11 ==> unit_interval) -- treasury growth rate
            , opt (idx 16 ==> coin) -- min pool cost
            , opt (idx 17 ==> coin) -- ada per utxo byte
            , opt (idx 18 ==> costmdls) -- cost models for script languages
            , opt (idx 19 ==> ex_unit_prices) -- execution costs
            , opt (idx 20 ==> ex_units) -- max tx ex units
            , opt (idx 21 ==> ex_units) -- max block ex units
            , opt (idx 22 ==> VUInt) -- max value size
            , opt (idx 23 ==> VUInt) -- collateral percentage
            , opt (idx 24 ==> VUInt) -- max collateral inputs
            , opt (idx 25 ==> pool_voting_thresholds) -- pool voting thresholds
            , opt (idx 26 ==> drep_voting_thresholds) -- DRep voting thresholds
            , opt (idx 27 ==> VUInt) -- min committee size
            , opt (idx 28 ==> epoch) -- committee term limit
            , opt (idx 29 ==> epoch) -- governance action validity period
            , opt (idx 30 ==> coin) -- governance action deposit
            , opt (idx 31 ==> coin) -- DRep deposit
            , opt (idx 32 ==> epoch) -- DRep inactivity period
            ]

pool_voting_thresholds :: Rule
pool_voting_thresholds =
    "pool_voting_thresholds"
        =:= arr
            [ a unit_interval -- motion no confidence
            , a unit_interval -- committee normal
            , a unit_interval -- committee no confidence
            , a unit_interval -- hard fork initiation
            , a unit_interval -- security relevant parameter voting threshold
            ]

drep_voting_thresholds :: Rule
drep_voting_thresholds =
    "drep_voting_thresholds"
        =:= arr
            [ a unit_interval -- motion no confidence
            , a unit_interval -- committee normal
            , a unit_interval -- committee no confidence
            , a unit_interval -- update constitution
            , a unit_interval -- hard fork initiation
            , a unit_interval -- PP network group
            , a unit_interval -- PP economic group
            , a unit_interval -- PP technical group
            , a unit_interval -- PP governance group
            , a unit_interval -- treasury withdrawal
            ]

transaction_witness_set :: Rule
transaction_witness_set =
    "transaction_witness_set"
        =:= mp
            [ opt $ idx 0 ==> nonempty_set vkeywitness
            , opt $ idx 1 ==> nonempty_set native_script
            , opt $ idx 2 ==> nonempty_set bootstrap_witness
            , opt $ idx 3 ==> nonempty_set plutus_v1_script
            , opt $ idx 4 ==> nonempty_set plutus_data
            , opt $ idx 5 ==> redeemers
            , opt $ idx 6 ==> nonempty_set plutus_v2_script
            , opt $ idx 7 ==> nonempty_set plutus_v3_script
            ]

-- The real type of  plutus_v1_script, plutus_v2_script and plutus_v3_script is bytes.
-- However, because we enforce uniqueness when many scripts are supplied,
-- we need to hack around for tests in order to avoid generating duplicates,
-- since the cddl tool we use for roundtrip testing doesn't generate distinct collections.
plutus_v1_script :: Rule
plutus_v1_script =
    comment
        ( "The real type of  plutus_v1_script, plutus_v2_script and plutus_v3_script is bytes.\n"
            <> "However, because we enforce uniqueness when many scripts are supplied,\n"
            <> "we need to hack around for tests in order to avoid generating duplicates,\n"
            <> "since the cddl tool we use for roundtrip testing doesn't generate distinct collections.\n"
        )
        $ "plutus_v1_script" =:= distinct VBytes

plutus_v2_script :: Rule
plutus_v2_script = "plutus_v2_script" =:= distinct VBytes

plutus_v3_script :: Rule
plutus_v3_script = "plutus_v3_script" =:= distinct VBytes

plutus_data :: Rule
plutus_data =
    "plutus_data"
        =:= constr plutus_data
        / smp [0 <+ asKey plutus_data ==> plutus_data]
        / sarr [0 <+ a plutus_data]
        / big_int
        / bounded_bytes

big_int :: Rule
big_int = "big_int" =:= VInt / big_VUInt / big_nint

big_VUInt :: Rule
big_VUInt = "big_VUInt" =:= tag 2 bounded_bytes

big_nint :: Rule
big_nint = "big_nint" =:= tag 3 bounded_bytes

constr :: (IsType0 x) => x -> GRuleCall
constr = binding $ \x ->
    "constr"
        =:= tag 1 (arr [0 <+ a x])
        / tag 2 (arr [0 <+ a x])
        / tag 3 (arr [0 <+ a x])
        / tag 4 (arr [0 <+ a x])
        / tag 5 (arr [0 <+ a x])
        / tag 6 (arr [0 <+ a x])
        / tag 7 (arr [0 <+ a x])
        -- similarly for tag range: 6.1280 .. 6.1400 inclusive
        / tag 2 (arr [a VUInt, a $ arr [0 <+ a x]])

redeemers :: Rule
redeemers =
    "redeemers"
        =:= sarr
            [ 1
                <+ a
                    ( arr
                        [ "tag" ==> redeemer_tag
                        , "index" ==> VUInt
                        , "data" ==> plutus_data
                        , "ex_units" ==> ex_units
                        ]
                    )
            ]
        / smp
            [ 1
                <+ asKey (mp ["tag" ==> redeemer_tag, "index" ==> VUInt])
                ==> arr ["data" ==> plutus_data, "ex_units" ==> ex_units]
            ]

redeemer_tag :: Rule
redeemer_tag =
    "redeemer_tag"
        =:= int 0 -- Spending
        / int 1 -- Minting
        / int 2 -- Certifying
        / int 3 -- Rewarding
        / int 4 -- Voting
        / int 5 -- Proposing

ex_units :: Rule
ex_units = "ex_units" =:= arr ["mem" ==> VUInt, "steps" ==> VUInt]

ex_unit_prices :: Rule
ex_unit_prices =
    "ex_unit_prices"
        =:= arr
            [ "mem_price" ==> nonnegative_interval
            , "step_price" ==> nonnegative_interval
            ]

language :: Rule
language =
    "language"
        =:= int 0 -- Plutus v1
        / int 1 -- Plutus v2
        / int 2 -- Plutus v3

potential_languages :: Rule
potential_languages = "potential_languages" =:= int 0 ... int 255

-- The format for costmdls is flexible enough to allow adding Plutus built-ins and language
-- versions in the future.
--
costmdls :: Rule
costmdls =
    comment "The format for costmdls is flexible enough to allow adding Plutus\n built-ins and language versions in the future." $
        "costmdls"
            =:= mp
                [ opt $ idx 0 ==> arr [166 <+ a VInt] -- Plutus v1, only 166 integers are used, but more are accepted (and ignored)
                , opt $ idx 1 ==> arr [175 <+ a VInt] -- Plutus v2, only 175 integers are used, but more are accepted (and ignored)
                , opt $ idx 2 ==> arr [223 <+ a VInt] -- Plutus v3, only 223 integers are used, but more are accepted (and ignored)
                , opt $ idx 3 ==> arr [a VInt] -- Any 8-bit unsigned number can be used as a key.
                ]

transaction_metadatum :: Rule
transaction_metadatum =
    "transaction_metadatum"
        =:= smp [0 <+ asKey transaction_metadatum ==> transaction_metadatum]
        / sarr [0 <+ a transaction_metadatum]
        / VInt
        / (VBytes `sized` (0 :: Word64, 64 :: Word64))
        / (VText `sized` (0 :: Word64, 64 :: Word64))

transaction_metadatum_label :: Rule
transaction_metadatum_label = "transaction_metadatum_label" =:= VUInt

metadata :: Rule
metadata =
    "metadata"
        =:= mp
            [ 0
                <+ asKey transaction_metadatum_label
                ==> transaction_metadatum
            ]

auxiliary_data :: Rule
auxiliary_data =
    "auxiliary_data"
        =:= metadata -- Shelley
        / sarr
            [ "transaction_metadata" ==> metadata -- Shelley-ma
            , "auxiliary_scripts" ==> arr [0 <+ a native_script]
            ]
        / smp
            [ opt (idx 0 ==> metadata) -- Alonzo and beyond
            , opt (idx 1 ==> arr [0 <+ a native_script])
            , opt (idx 2 ==> arr [0 <+ a plutus_v1_script])
            , opt (idx 3 ==> arr [0 <+ a plutus_v2_script])
            , opt (idx 4 ==> arr [0 <+ a plutus_v3_script])
            ]

vkeywitness :: Rule
vkeywitness = "vkeywitness" =:= arr [a vkey, a signature]

bootstrap_witness :: Rule
bootstrap_witness =
    "bootstrap_witness"
        =:= arr
            [ "public_key" ==> vkey
            , "signature" ==> signature
            , "chain_code" ==> (VBytes `sized` (32 :: Word64))
            , "attributes" ==> VBytes
            ]

native_script :: Rule
native_script =
    "native_script"
        =:= arr
            [ a
                ( script_pubkey
                    / script_all
                    / script_any
                    / script_n_of_k
                    / invalid_before
                    -- Timelock validity intervals are half-open intervals [a, b).
                    -- This field specifies the left (included) endpoint a.
                    / invalid_hereafter
                    -- Timelock validity intervals are half-open intervals [a, b).
                    -- This field specifies the right (excluded) endpoint b.
                )
            ]

script_pubkey :: Named Group
script_pubkey = "script_pubkey" =:~ grp [0, a addr_keyhash]

script_all :: Named Group
script_all = "script_all" =:~ grp [1, a (arr [0 <+ a native_script])]

script_any :: Named Group
script_any = "script_any" =:~ grp [2, a (arr [0 <+ a native_script])]

script_n_of_k :: Named Group
script_n_of_k =
    "script_n_of_k"
        =:~ grp [3, "n" ==> VUInt, a (arr [0 <+ a native_script])]

invalid_before :: Named Group
invalid_before = "invalid_before" =:~ grp [4, a VUInt]

invalid_hereafter :: Named Group
invalid_hereafter = "invalid_hereafter" =:~ grp [5, a VUInt]

coin :: Rule
coin = "coin" =:= VUInt

multiasset :: (Show a, IsType0 a) => a -> Rule
multiasset x =
    "multiasset_"
        <> T.pack (show x)
            =:= mp [1 <+ asKey policy_id ==> mp [1 <+ asKey asset_name ==> x]]

policy_id :: Rule
policy_id = "policy_id" =:= scripthash

asset_name :: Rule
asset_name = "asset_name" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)

negInt64 :: Rule
negInt64 = "negInt64" =:= int (-9223372036854775808) ... int (-1)

posInt64 :: Rule
posInt64 = "posInt64" =:= int 1 ... int 9223372036854775807

nonZeroInt64 :: Rule
nonZeroInt64 = "nonZeroInt64" =:= negInt64 / posInt64 -- this is the same as the current int64 definition but without zero

positive_coin :: Rule
positive_coin = "positive_coin" =:= int 1 ... int 18446744073709551615

value :: Rule
value = "value" =:= coin / sarr [a coin, a (multiasset positive_coin)]

mint :: Rule
mint = "mint" =:= multiasset nonZeroInt64

int64 :: Rule
int64 = "int64" =:= int (-9223372036854775808) ... int 9223372036854775807

network_id :: Rule
network_id = "network_id" =:= int 0 / int 1

epoch :: Rule
epoch = "epoch" =:= VUInt

addr_keyhash :: Rule
addr_keyhash = "addr_keyhash" =:= hash28

pool_keyhash :: Rule
pool_keyhash = "pool_keyhash" =:= hash28

vrf_keyhash :: Rule
vrf_keyhash = "vrf_keyhash" =:= hash32

auxiliary_data_hash :: Rule
auxiliary_data_hash = "auxiliary_data_hash" =:= hash32

pool_metadata_hash :: Rule
pool_metadata_hash = "pool_metadata_hash" =:= hash32

-- To compute a script hash, note that you must prepend
-- a tag to the bytes of the script before hashing.
-- The tag is determined by the language.
-- The tags in the Conway era are:
--   "\x00" for multisig scripts
--   "\x01" for Plutus V1 scripts
--   "\x02" for Plutus V2 scripts
--   "\x03" for Plutus V3 scripts
scripthash :: Rule
scripthash =
    comment
        ( "To compute a script hash, note that you must prepend\n"
            <> "a tag to the bytes of the script before hashing.\n"
            <> "The tag is determined by the language.\n"
            <> "The tags in the Conway era are:\n"
            <> "\"\x00\" for multisig scripts\n"
            <> "\"\x01\" for Plutus V1 scripts\n"
            <> "\"\x02\" for Plutus V2 scripts\n"
            <> "\"\x03\" for Plutus V3 scripts\n"
        )
        $ "scripthash" =:= hash28

datum_hash :: Rule
datum_hash = "datum_hash" =:= hash32

data_a :: Rule
data_a = "data" =:= VBytes `cbor` plutus_data

datum_option :: Rule
datum_option = "datum_option" =:= arr [0, a hash32] / arr [1, a data_a]

script_ref :: Rule
script_ref = "script_ref" =:= VBytes `cbor` script

script :: Rule
script =
    "script"
        =:= arr [0, a native_script]
        / arr [1, a plutus_v1_script]
        / arr [2, a plutus_v2_script]
        / arr [3, a plutus_v3_script]

--------------------------------------------------------------------------------
-- Crypto
--------------------------------------------------------------------------------

hash28 :: Rule
hash28 = "hash28" =:= VBytes `sized` (28 :: Word64)

hash32 :: Rule
hash32 = "hash32" =:= VBytes `sized` (32 :: Word64)

vkey :: Rule
vkey = "vkey" =:= VBytes `sized` (32 :: Word64)

vrf_vkey :: Rule
vrf_vkey = "vrf_vkey" =:= VBytes `sized` (32 :: Word64)

vrf_cert :: Rule
vrf_cert = "vrf_cert" =:= arr [a VBytes, a (VBytes `sized` (80 :: Word64))]

kes_vkey :: Rule
kes_vkey = "kes_vkey" =:= VBytes `sized` (32 :: Word64)

kes_signature :: Rule
kes_signature = "kes_signature" =:= VBytes `sized` (448 :: Word64)

signkeyKES :: Rule
signkeyKES = "signkeyKES" =:= VBytes `sized` (64 :: Word64)

signature :: Rule
signature = "signature" =:= VBytes `sized` (64 :: Word64)

--------------------------------------------------------------------------------
-- Extras
--------------------------------------------------------------------------------

-- Conway era introduces an optional 258 tag for sets, which will become mandatory in the
-- second era after Conway. We recommend all the tooling to account for this future breaking
-- change sooner rather than later, in order to provide a smooth transition for their users.

set :: (IsType0 t0) => t0 -> GRuleCall
set = binding $ \x -> "set" =:= arr [0 <+ a x]

nonempty_set :: (IsType0 t0) => t0 -> GRuleCall
nonempty_set = binding $ \x -> "nonempty_set" =:= set x

-- TODO Should we give this a name?
nonempty_oset :: (IsType0 t0) => t0 -> GRuleCall
nonempty_oset = nonempty_set

positive_int :: Rule
positive_int = "positive_int" =:= int 1 ... int 18446744073709551615

unit_interval :: Rule
unit_interval = "unit_interval" =:= tag 30 (arr [1, 2])

-- unit_interval = tag 0 [uint, uint]
--
-- Comment above depicts the actual definition for `unit_interval`.
--
-- Unit interval is a number in the range between 0 and 1, which
-- means there are two extra constraints:
-- \* numerator <= denominator
-- \* denominator > 0
--
-- Relation between numerator and denominator cannot be expressed in CDDL, which
-- poses a problem for testing. We need to be able to generate random valid data
-- for testing implementation of our encoders/decoders. Which means we cannot use
-- the actual definition here and we hard code the value to 1/2

-- nonnegative_interval = tag 0 [uint, positive_int]
nonnegative_interval :: Rule
nonnegative_interval = "nonnegative_interval" =:= tag 30 (arr [a VUInt, a positive_int])

address :: Rule
address =
    "address"
        =:= bstr "001000000000000000000000000000000000000000000000000000000011000000000000000000000000000000000000000000000000000000"
        / bstr "102000000000000000000000000000000000000000000000000000000022000000000000000000000000000000000000000000000000000000"
        / bstr "203000000000000000000000000000000000000000000000000000000033000000000000000000000000000000000000000000000000000000"
        / bstr "304000000000000000000000000000000000000000000000000000000044000000000000000000000000000000000000000000000000000000"
        / bstr "405000000000000000000000000000000000000000000000000000000087680203"
        / bstr "506000000000000000000000000000000000000000000000000000000087680203"
        / bstr "6070000000000000000000000000000000000000000000000000000000"
        / bstr "7080000000000000000000000000000000000000000000000000000000"

reward_account :: Rule
reward_account =
    "reward_account"
        =:= bstr "E090000000000000000000000000000000000000000000000000000000"
        / bstr "F0A0000000000000000000000000000000000000000000000000000000"

bounded_bytes :: Rule
bounded_bytes = "bounded_bytes" =:= VBytes `sized` (0 :: Word64, 64 :: Word64)

-- the real bounded_bytes does not have this limit. it instead has a different
-- limit which cannot be expressed in CDDL.
-- The limit is as follows:
--  - bytes with a definite-length encoding are limited to size 0..64
--  - for bytes with an indefinite-length CBOR encoding, each chunk is
--    limited to size 0..64
--  ( reminder: in CBOR, the indefinite-length encoding of bytestrings
--    consists of a token #2.31 followed by a sequence of definite-length
--    encoded bytestrings and a stop code )

-- a type for distinct values.
-- The type parameter must support .size, for example: bytes or uint

distinct :: (IsType0 a, IsConstrainable a b) => a -> GRuleCall
distinct = binding $ \x ->
    "distinct"
        =:= (x `sized` (8 :: Word64))
        / (x `sized` (16 :: Word64))
        / (x `sized` (20 :: Word64))
        / (x `sized` (24 :: Word64))
        / (x `sized` (30 :: Word64))
        / (x `sized` (32 :: Word64))
