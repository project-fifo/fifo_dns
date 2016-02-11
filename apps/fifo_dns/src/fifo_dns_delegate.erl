-module(fifo_dns_delegate).
-behaviour(erldns_resolver).
-export([get_records_by_name/1]).

-include_lib("dns/include/dns.hrl").

get_records_by_name(Qname) ->
    {ok, Domain} = application:get_env(fifo_dns, domain),
    Regexp = "^([^.]*)\\.([^.]*)\\.uuid\\." ++ Domain ++ "$",
    case re:run(Qname, Regexp, [{capture, all_but_first, binary}]) of
        {match, [UUID, Org]} ->
            do_lookup_uuid(Qname, UUID, Org);
        _ ->
            []
    end.

do_lookup_uuid(Qname, UUID, Org) ->
    case ls_vm:get(UUID) of
        {ok, VM} ->
            validate_vm(Qname, VM, Org);
        _ ->
            []
    end.

validate_vm(Qname, VM, Org) ->
    case ft_vm:owner(VM) of
        AOrg when AOrg =:= Org ->
            case primary_ip(VM) of
                not_found ->
                    [];
                {ok, IP} ->
                    [make_record(Qname, IP)]
            end;
        _ ->
            []
    end.


primary_ip(VM) ->
    find_primary(jsxd:get([<<"networks">>], [], ft_vm:config(VM))).

find_primary([]) ->
    not_found;
find_primary([N | R]) ->
    case jsxd:get([<<"primary">>], false, N) of
        <<"true">> ->
            jsxd:get([<<"ip">>], N);
        _ ->
            find_primary(R)
    end.

make_record(Qname, IP) ->
    Parts = re:split(IP, "\\."),
    [A, B, C, D] = [binary_to_integer(P) || P <- Parts],
    #dns_rr {
       name = Qname,
       type = 1,
       ttl = 3600,
       data = #dns_rrdata_a{ip = {A, B, C, D}}
      }.
