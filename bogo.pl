:- set_prolog_flag(encoding, utf8).

% Needs SWI-Prolog version 8.1.13 for with_tty_raw
%
% Usage example:
% ?- consult('bogo.prolog').
% ?- interactive.
%
% Then start typing.
%
% [t],t
% [t,r],tr
% [t,r,u],tru
% [t,r,u,o],truo
% [t,r,u,o,w],trươ
% [t,r,u,o,w,n],trươn
% [t,r,u,o,w,n,g],trương
% [t,r,u,o,w,n,g,f],trường
% [h],h
% [h,o],ho
% [h,o,p],hop
% [h,o,p,w],hơp
% [h,o,p,w,j],hợp
interactive :- with_tty_raw(loop_process_key([])).

% food for thought:
% gìn giữ
% giặt gịa
% giặt gyạ (archaic)
% giặt gỵa
% context sensitive grammar?

loop_process_key(Sequence) :-
    get_char(C),
    (   C \= ' ',
        append(Sequence, [C], Sequence2),
        process_key_sequence(Sequence2, [], OutCharList),
        atom_chars(Atom, OutCharList),
        writeln((Sequence2, Atom)),
        loop_process_key(Sequence2)
    ;   loop_process_key([]) % start a new sequence with space character
    ).

% TODO: Upper case support

% TELEX
key_effect(f, add_tone(tone_huyen)).
key_effect(s, add_tone(tone_sac)).
key_effect(r, add_tone(tone_hoi)).
key_effect(x, add_tone(tone_nga)).
key_effect(j, add_tone(tone_nang)).

key_effect(a, add_vowel_mod(mod_hat_a)).
key_effect(e, add_vowel_mod(mod_hat_e)).
key_effect(o, add_vowel_mod(mod_hat_o)).
key_effect(w, add_vowel_mod(mod_horn_uo)).
key_effect(w, add_vowel_mod(mod_horn_o)).
key_effect(w, add_vowel_mod(mod_horn_u)).
key_effect(w, add_vowel_mod(mod_breve_a)).

key_effect(d, add_consonant_mod(mod_dash_d)).

% VNI
key_effect('2', add_tone(tone_huyen)).
key_effect('1', add_tone(tone_sac)).
key_effect('3', add_tone(tone_hoi)).
key_effect('4', add_tone(tone_nga)).
key_effect('5', add_tone(tone_nang)).

key_effect('6', add_vowel_mod(mod_hat_a)).
key_effect('6', add_vowel_mod(mod_hat_e)).
key_effect('6', add_vowel_mod(mod_hat_o)).
key_effect('7', add_vowel_mod(mod_horn_uo)).
key_effect('7', add_vowel_mod(mod_horn_o)).
key_effect('7', add_vowel_mod(mod_horn_u)).
key_effect('8', add_vowel_mod(mod_breve_a)).

key_effect('9', add_consonant_mod(mod_dash_d)).

% Usage example:
%
% ?- process_key_sequence([b, a, n, s, w], [] , Output).
%    Output = [b, ắ, n].
%

process_nice(AtomIn, OutList) :-
    atom_chars(AtomIn, Sequence),
    process_key_sequence(Sequence, OutList).

process_key_sequence(Sequence, Output) :-
    process_key_sequence(Sequence, [], Output).

process_key_sequence([Key|Rest], CurrentString, Output) :-
    once(process_key(CurrentString, Key, Output1)),
    process_key_sequence(Rest, Output1, Output).

process_key_sequence([], CurrentString, CurrentString).

% fall back to the raw sequence if processing fails (vowel -> vơel)
process_key_sequence(Keys, [], Keys).

% Try applying the effect of the key, assuming that there is one.
% Performance optimization idea: give out both an output string and a syllable term and reuse the term for the next cycle.
process_key(CurrentString, Key, OutString) :-
    % break the syllable down into initial consonant, vowel nucleus and final consonant
    once(phrase(syllable(I, V, F), CurrentString)),

    apply_key_effect(s(I, V, F), Key, s(I2, V2, F2)),
    shortcut_rule(I2, V2, F2, I3, V3, F3),

    atomic_list_concat([I3, V3, F3], OutputAtom),
    atom_chars(OutputAtom, OutString),

    % require that the output must be parsable, this is to weed out non-Vietnamese words like vowel -> vơel
    once(phrase(syllable(_, _, _), OutString)).

shortcut_rule(I, ưo, F, I, ươ, F).
shortcut_rule(I, V, F, I, V, F).

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_tone(Tone)),
    syllable_tone(InSyllable, Tone, OutSyllable).

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_vowel_mod(Mod)),
    syllable_vowel_mod(InSyllable, Mod, OutSyllable).

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_consonant_mod(Mod)),
    syllable_consonant_mod(InSyllable, Mod, OutSyllable).

% If none of the transformation rules apply, append the key to the end of the syllable
apply_key_effect(s(I, '', ''), Key, s(I2, '', '')) :- atom_concat(I, Key, I2).
apply_key_effect(s(I, V, ''), Key, s(I, V2, '')) :- atom_concat(V, Key, V2).
apply_key_effect(s(I, V, F), Key, s(I, V, F2)) :- atom_concat(F, Key, F2).

syllable_vowel_mod(s(I, V, F), Mod, s(I, V_out, F)) :-
    vowel_nucleus_mod_tone(V, V_raw, _, Tone),
    vowel_nucleus_mod_tone(V_out, V_raw, Mod, Tone).

syllable_tone(s(I, V, F), Tone, s(I, V_out, F)) :-
    vowel_nucleus_mod_tone(V, V_raw, Mod, _),
    vowel_nucleus_mod_tone(V_out, V_raw, Mod, Tone).

% there is the only case of d -> đ here so it's hard-coded
syllable_consonant_mod(s(d, V, F), _, s(đ, V, F)).

%%%%%%%%%%%%%
% Parser section
%%%%%%%%%%%%%

syllable(I, V, F) --> consonant_initial(I), vowel(V), consonant_final(F).

vowel(V) --> vowel_chars(Vs), { atom_chars(V, Vs) }.

vowel_chars([X|Rest]) --> vowel_char(X), vowel_chars(Rest).
vowel_chars([]) --> [].

vowel_char(C) --> [C], {
    atom_chars(àáảãạaằắẳẵặăầấẩẫậâèéẻẽẹeềếểễệêìíỉĩịiòóỏõọoồốổỗộôờớởỡợơùúủũụuừứửữựưỳýỷỹỵy, AtomList),
    member(C, AtomList)
}.

consonant_initial('') --> [].
consonant_initial(m) --> [m].
consonant_initial(n) --> [n].
consonant_initial(nh) --> [n, h].
consonant_initial(ng) --> [n, g].
consonant_initial(ngh) --> [n, g, h].
consonant_initial(p) --> [p].
consonant_initial(t) --> [t].
consonant_initial(tr) --> [t, r].
consonant_initial(ch) --> [c, h].
consonant_initial(c) --> [c].
consonant_initial(k) --> [k].
consonant_initial(qu) --> [q, u].
consonant_initial(q) --> [q]. % I'm a bit iffy on this case
consonant_initial(ph) --> [p, h].
consonant_initial(th) --> [t, h].
consonant_initial(kh) --> [k, h].
consonant_initial(b) --> [b].
consonant_initial(đ) --> [đ].
consonant_initial(s) --> [s].
consonant_initial(x) --> [x].
consonant_initial(h) --> [h].
consonant_initial(d) --> [d].
% TODO gìn doesn't work
consonant_initial(gi), [V] --> [g, i], vowel(V).
consonant_initial(g) --> [g].
consonant_initial(gh) --> [g, h].
consonant_initial(v) --> [v].
consonant_initial(l) --> [l].
consonant_initial(r) --> [r].
% for the dzũng zũng people
consonant_initial(dz) --> [d, z].
consonant_initial(z) --> [z].

consonant_final('') --> [].
consonant_final(n) --> [n].
consonant_final(nh) --> [n, h].
consonant_final(ng) --> [n, g].
consonant_final(c) --> [c].
consonant_final(ch) --> [c, h].
consonant_final(p) --> [p].
consonant_final(t) --> [t].
consonant_final(n) --> [n].
consonant_final(m) --> [m].


% vowel nucleus transformation database schema:
%   resulting vowel, base characters, vowel modification, tone
%
% FIXME: these facts are highly redundant/compressible. What would be a good representation?
vowel_nucleus_mod_tone(a, a, mod_none, tone_ngang).
vowel_nucleus_mod_tone(à, a, mod_none, tone_huyen).
vowel_nucleus_mod_tone(á, a, mod_none, tone_sac).
vowel_nucleus_mod_tone(ả, a, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ã, a, mod_none, tone_nga).
vowel_nucleus_mod_tone(ạ, a, mod_none, tone_nang).

vowel_nucleus_mod_tone(â, a, mod_hat_a, tone_ngang).
vowel_nucleus_mod_tone(ầ, a, mod_hat_a, tone_huyen).
vowel_nucleus_mod_tone(ấ, a, mod_hat_a, tone_sac).
vowel_nucleus_mod_tone(ẩ, a, mod_hat_a, tone_hoi).
vowel_nucleus_mod_tone(ẫ, a, mod_hat_a, tone_nga).
vowel_nucleus_mod_tone(ậ, a, mod_hat_a, tone_nang).

vowel_nucleus_mod_tone(ă, a, mod_breve_a, tone_ngang).
vowel_nucleus_mod_tone(ằ, a, mod_breve_a, tone_huyen).
vowel_nucleus_mod_tone(ắ, a, mod_breve_a, tone_sac).
vowel_nucleus_mod_tone(ẳ, a, mod_breve_a, tone_hoi).
vowel_nucleus_mod_tone(ẵ, a, mod_breve_a, tone_nga).
vowel_nucleus_mod_tone(ặ, a, mod_breve_a, tone_nang).

vowel_nucleus_mod_tone(e, e, mod_none, tone_ngang).
vowel_nucleus_mod_tone(è, e, mod_none, tone_huyen).
vowel_nucleus_mod_tone(é, e, mod_none, tone_sac).
vowel_nucleus_mod_tone(ẻ, e, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ẽ, e, mod_none, tone_nga).
vowel_nucleus_mod_tone(ẹ, e, mod_none, tone_nang).

vowel_nucleus_mod_tone(ê, e, mod_hat_e, tone_ngang).
vowel_nucleus_mod_tone(ề, e, mod_hat_e, tone_huyen).
vowel_nucleus_mod_tone(ế, e, mod_hat_e, tone_sac).
vowel_nucleus_mod_tone(ể, e, mod_hat_e, tone_hoi).
vowel_nucleus_mod_tone(ễ, e, mod_hat_e, tone_nga).
vowel_nucleus_mod_tone(ệ, e, mod_hat_e, tone_nang).

vowel_nucleus_mod_tone(i, i, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ì, i, mod_none, tone_huyen).
vowel_nucleus_mod_tone(í, i, mod_none, tone_sac).
vowel_nucleus_mod_tone(ỉ, i, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ĩ, i, mod_none, tone_nga).
vowel_nucleus_mod_tone(ị, i, mod_none, tone_nang).

vowel_nucleus_mod_tone(o, o, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ò, o, mod_none, tone_huyen).
vowel_nucleus_mod_tone(ó, o, mod_none, tone_sac).
vowel_nucleus_mod_tone(ỏ, o, mod_none, tone_hoi).
vowel_nucleus_mod_tone(õ, o, mod_none, tone_nga).
vowel_nucleus_mod_tone(ọ, o, mod_none, tone_nang).

vowel_nucleus_mod_tone(ô, o, mod_hat_o, tone_ngang).
vowel_nucleus_mod_tone(ồ, o, mod_hat_o, tone_huyen).
vowel_nucleus_mod_tone(ố, o, mod_hat_o, tone_sac).
vowel_nucleus_mod_tone(ổ, o, mod_hat_o, tone_hoi).
vowel_nucleus_mod_tone(ỗ, o, mod_hat_o, tone_nga).
vowel_nucleus_mod_tone(ộ, o, mod_hat_o, tone_nang).

vowel_nucleus_mod_tone(ơ, o, mod_horn_o, tone_ngang).
vowel_nucleus_mod_tone(ờ, o, mod_horn_o, tone_huyen).
vowel_nucleus_mod_tone(ớ, o, mod_horn_o, tone_sac).
vowel_nucleus_mod_tone(ở, o, mod_horn_o, tone_hoi).
vowel_nucleus_mod_tone(ỡ, o, mod_horn_o, tone_nga).
vowel_nucleus_mod_tone(ợ, o, mod_horn_o, tone_nang).

vowel_nucleus_mod_tone(u, u, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ù, u, mod_none, tone_huyen).
vowel_nucleus_mod_tone(ú, u, mod_none, tone_sac).
vowel_nucleus_mod_tone(ủ, u, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ũ, u, mod_none, tone_nga).
vowel_nucleus_mod_tone(ụ, u, mod_none, tone_nang).

vowel_nucleus_mod_tone(ư, u, mod_horn_u, tone_ngang).
vowel_nucleus_mod_tone(ừ, u, mod_horn_u, tone_huyen).
vowel_nucleus_mod_tone(ứ, u, mod_horn_u, tone_sac).
vowel_nucleus_mod_tone(ử, u, mod_horn_u, tone_hoi).
vowel_nucleus_mod_tone(ữ, u, mod_horn_u, tone_nga).
vowel_nucleus_mod_tone(ự, u, mod_horn_u, tone_nang).

vowel_nucleus_mod_tone(y, y, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ỳ, y, mod_none, tone_huyen).
vowel_nucleus_mod_tone(ý, y, mod_none, tone_sac).
vowel_nucleus_mod_tone(ỷ, y, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ỹ, y, mod_none, tone_nga).
vowel_nucleus_mod_tone(ỵ, y, mod_none, tone_nang).

% just to support giặt gỵa
vowel_nucleus_mod_tone(ya, ya, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ỳa, ya, mod_none, tone_huyen).
vowel_nucleus_mod_tone(ýa, ya, mod_none, tone_sac).
vowel_nucleus_mod_tone(ỷa, ya, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ỹa, ya, mod_none, tone_nga).
vowel_nucleus_mod_tone(ỵa, ya, mod_none, tone_nang).

vowel_nucleus_mod_tone(ai, ai, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ài, ai, mod_none, tone_huyen).
vowel_nucleus_mod_tone(ái, ai, mod_none, tone_sac).
vowel_nucleus_mod_tone(ải, ai, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ãi, ai, mod_none, tone_nga).
vowel_nucleus_mod_tone(ại, ai, mod_none, tone_nang).

vowel_nucleus_mod_tone(ao, ao, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ào, ao, mod_none, tone_huyen).
vowel_nucleus_mod_tone(áo, ao, mod_none, tone_sac).
vowel_nucleus_mod_tone(ảo, ao, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ão, ao, mod_none, tone_nga).
vowel_nucleus_mod_tone(ạo, ao, mod_none, tone_nang).

vowel_nucleus_mod_tone(au, au, mod_none, tone_ngang).
vowel_nucleus_mod_tone(àu, au, mod_none, tone_huyen).
vowel_nucleus_mod_tone(áu, au, mod_none, tone_sac).
vowel_nucleus_mod_tone(ảu, au, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ãu, au, mod_none, tone_nga).
vowel_nucleus_mod_tone(ạu, au, mod_none, tone_nang).

vowel_nucleus_mod_tone(ay, ay, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ày, ay, mod_none, tone_huyen).
vowel_nucleus_mod_tone(áy, ay, mod_none, tone_sac).
vowel_nucleus_mod_tone(ảy, ay, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ãy, ay, mod_none, tone_nga).
vowel_nucleus_mod_tone(ạy, ay, mod_none, tone_nang).

vowel_nucleus_mod_tone(eo, eo, mod_none, tone_ngang).
vowel_nucleus_mod_tone(èo, eo, mod_none, tone_huyen).
vowel_nucleus_mod_tone(éo, eo, mod_none, tone_sac).
vowel_nucleus_mod_tone(ẻo, eo, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ẽo, eo, mod_none, tone_nga).
vowel_nucleus_mod_tone(ẹo, eo, mod_none, tone_nang).

vowel_nucleus_mod_tone(ia, ia, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ìa, ia, mod_none, tone_huyen).
vowel_nucleus_mod_tone(ía, ia, mod_none, tone_sac).
vowel_nucleus_mod_tone(ỉa, ia, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ĩa, ia, mod_none, tone_nga).
vowel_nucleus_mod_tone(ịa, ia, mod_none, tone_nang).

vowel_nucleus_mod_tone(iu, iu, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ìu, iu, mod_none, tone_huyen).
vowel_nucleus_mod_tone(íu, iu, mod_none, tone_sac).
vowel_nucleus_mod_tone(ỉu, iu, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ĩu, iu, mod_none, tone_nga).
vowel_nucleus_mod_tone(ịu, iu, mod_none, tone_nang).

vowel_nucleus_mod_tone(ie, ie, mod_none, tone_ngang).
vowel_nucleus_mod_tone(iè, ie, mod_none, tone_huyen).
vowel_nucleus_mod_tone(ié, ie, mod_none, tone_sac).
vowel_nucleus_mod_tone(iẻ, ie, mod_none, tone_hoi).
vowel_nucleus_mod_tone(iẽ, ie, mod_none, tone_nga).
vowel_nucleus_mod_tone(iẹ, ie, mod_none, tone_nang).

vowel_nucleus_mod_tone(iê, ie, mod_hat_e, tone_ngang).
vowel_nucleus_mod_tone(iề, ie, mod_hat_e, tone_huyen).
vowel_nucleus_mod_tone(iế, ie, mod_hat_e, tone_sac).
vowel_nucleus_mod_tone(iể, ie, mod_hat_e, tone_hoi).
vowel_nucleus_mod_tone(iễ, ie, mod_hat_e, tone_nga).
vowel_nucleus_mod_tone(iệ, ie, mod_hat_e, tone_nang).

vowel_nucleus_mod_tone(iêu, ieu, mod_hat_e, tone_ngang).
vowel_nucleus_mod_tone(iều, ieu, mod_hat_e, tone_huyen).
vowel_nucleus_mod_tone(iếu, ieu, mod_hat_e, tone_sac).
vowel_nucleus_mod_tone(iểu, ieu, mod_hat_e, tone_hoi).
vowel_nucleus_mod_tone(iễu, ieu, mod_hat_e, tone_nga).
vowel_nucleus_mod_tone(iệu, ieu, mod_hat_e, tone_nang).

% TODO how to support hòa but hoán? It's some kind of context sensitive grammar situation here.
% should we include the consonant parts into the rules?
vowel_nucleus_mod_tone(oa, oa, mod_none, tone_ngang).
vowel_nucleus_mod_tone(oà, oa, mod_none, tone_huyen).
vowel_nucleus_mod_tone(oá, oa, mod_none, tone_sac).
vowel_nucleus_mod_tone(oả, oa, mod_none, tone_hoi).
vowel_nucleus_mod_tone(oã, oa, mod_none, tone_nga).
vowel_nucleus_mod_tone(oạ, oa, mod_none, tone_nang).

vowel_nucleus_mod_tone(oă, oa, mod_breve_a, tone_ngang).
vowel_nucleus_mod_tone(oằ, oa, mod_breve_a, tone_huyen).
vowel_nucleus_mod_tone(oắ, oa, mod_breve_a, tone_sac).
vowel_nucleus_mod_tone(oẳ, oa, mod_breve_a, tone_hoi).
vowel_nucleus_mod_tone(oẵ, oa, mod_breve_a, tone_nga).
vowel_nucleus_mod_tone(oặ, oa, mod_breve_a, tone_nang).

vowel_nucleus_mod_tone(oe, oe, mod_none, tone_ngang).
vowel_nucleus_mod_tone(oè, oe, mod_none, tone_huyen).
vowel_nucleus_mod_tone(oé, oe, mod_none, tone_sac).
vowel_nucleus_mod_tone(oẻ, oe, mod_none, tone_hoi).
vowel_nucleus_mod_tone(oẽ, oe, mod_none, tone_nga).
vowel_nucleus_mod_tone(oẹ, oe, mod_none, tone_nang).

vowel_nucleus_mod_tone(oi, oi, mod_none, tone_ngang).
vowel_nucleus_mod_tone(òi, oi, mod_none, tone_huyen).
vowel_nucleus_mod_tone(ói, oi, mod_none, tone_sac).
vowel_nucleus_mod_tone(ỏi, oi, mod_none, tone_hoi).
vowel_nucleus_mod_tone(õi, oi, mod_none, tone_nga).
vowel_nucleus_mod_tone(ọi, oi, mod_none, tone_nang).

vowel_nucleus_mod_tone(ua, ua, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ùa, ua, mod_none, tone_huyen).
vowel_nucleus_mod_tone(úa, ua, mod_none, tone_sac).
vowel_nucleus_mod_tone(ủa, ua, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ũa, ua, mod_none, tone_nga).
vowel_nucleus_mod_tone(ụa, ua, mod_none, tone_nang).

vowel_nucleus_mod_tone(uâ, ua, mod_hat_a, tone_ngang).
vowel_nucleus_mod_tone(uầ, ua, mod_hat_a, tone_huyen).
vowel_nucleus_mod_tone(uấ, ua, mod_hat_a, tone_sac).
vowel_nucleus_mod_tone(uẩ, ua, mod_hat_a, tone_hoi).
vowel_nucleus_mod_tone(uẫ, ua, mod_hat_a, tone_nga).
vowel_nucleus_mod_tone(uậ, ua, mod_hat_a, tone_nang).

vowel_nucleus_mod_tone(ui, ui, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ùi, ui, mod_none, tone_huyen).
vowel_nucleus_mod_tone(úi, ui, mod_none, tone_sac).
vowel_nucleus_mod_tone(ủi, ui, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ũi, ui, mod_none, tone_nga).
vowel_nucleus_mod_tone(ụi, ui, mod_none, tone_nang).

vowel_nucleus_mod_tone(uyê, uye, mod_hat_e, tone_ngang).
vowel_nucleus_mod_tone(uyề, uye, mod_hat_e, tone_huyen).
vowel_nucleus_mod_tone(uyế, uye, mod_hat_e, tone_sac).
vowel_nucleus_mod_tone(uyể, uye, mod_hat_e, tone_hoi).
vowel_nucleus_mod_tone(uyễ, uye, mod_hat_e, tone_nga).
vowel_nucleus_mod_tone(uyệ, uye, mod_hat_e, tone_nang).

vowel_nucleus_mod_tone(uô, uo, mod_hat_o, tone_ngang).
vowel_nucleus_mod_tone(uồ, uo, mod_hat_o, tone_huyen).
vowel_nucleus_mod_tone(uố, uo, mod_hat_o, tone_sac).
vowel_nucleus_mod_tone(uổ, uo, mod_hat_o, tone_hoi).
vowel_nucleus_mod_tone(uỗ, uo, mod_hat_o, tone_nga).
vowel_nucleus_mod_tone(uộ, uo, mod_hat_o, tone_nang).

vowel_nucleus_mod_tone(uôi, uoi, mod_hat_o, tone_ngang).
vowel_nucleus_mod_tone(uồi, uoi, mod_hat_o, tone_huyen).
vowel_nucleus_mod_tone(uối, uoi, mod_hat_o, tone_sac).
vowel_nucleus_mod_tone(uổi, uoi, mod_hat_o, tone_hoi).
vowel_nucleus_mod_tone(uỗi, uoi, mod_hat_o, tone_nga).
vowel_nucleus_mod_tone(uội, uoi, mod_hat_o, tone_nang).

vowel_nucleus_mod_tone(uy, uy, mod_none, tone_ngang).
vowel_nucleus_mod_tone(uỳ, uy, mod_none, tone_huyen).
vowel_nucleus_mod_tone(uý, uy, mod_none, tone_sac).
vowel_nucleus_mod_tone(uỷ, uy, mod_none, tone_hoi).
vowel_nucleus_mod_tone(uỹ, uy, mod_none, tone_nga).
vowel_nucleus_mod_tone(uỵ, uy, mod_none, tone_nang).

vowel_nucleus_mod_tone(âu, au, mod_hat_a, tone_ngang).
vowel_nucleus_mod_tone(ầu, au, mod_hat_a, tone_huyen).
vowel_nucleus_mod_tone(ấu, au, mod_hat_a, tone_sac).
vowel_nucleus_mod_tone(ẩu, au, mod_hat_a, tone_hoi).
vowel_nucleus_mod_tone(ẫu, au, mod_hat_a, tone_nga).
vowel_nucleus_mod_tone(ậu, au, mod_hat_a, tone_nang).

vowel_nucleus_mod_tone(ây, ay, mod_hat_a, tone_ngang).
vowel_nucleus_mod_tone(ầy, ay, mod_hat_a, tone_huyen).
vowel_nucleus_mod_tone(ấy, ay, mod_hat_a, tone_sac).
vowel_nucleus_mod_tone(ẩy, ay, mod_hat_a, tone_hoi).
vowel_nucleus_mod_tone(ẫy, ay, mod_hat_a, tone_nga).
vowel_nucleus_mod_tone(ậy, ay, mod_hat_a, tone_nang).

vowel_nucleus_mod_tone(êu, eu, mod_hat_e, tone_ngang).
vowel_nucleus_mod_tone(ều, eu, mod_hat_e, tone_huyen).
vowel_nucleus_mod_tone(ếu, eu, mod_hat_e, tone_sac).
vowel_nucleus_mod_tone(ểu, eu, mod_hat_e, tone_hoi).
vowel_nucleus_mod_tone(ễu, eu, mod_hat_e, tone_nga).
vowel_nucleus_mod_tone(ệu, eu, mod_hat_e, tone_nang).

vowel_nucleus_mod_tone(ôi, oi, mod_hat_o, tone_ngang).
vowel_nucleus_mod_tone(ồi, oi, mod_hat_o, tone_huyen).
vowel_nucleus_mod_tone(ối, oi, mod_hat_o, tone_sac).
vowel_nucleus_mod_tone(ổi, oi, mod_hat_o, tone_hoi).
vowel_nucleus_mod_tone(ỗi, oi, mod_hat_o, tone_nga).
vowel_nucleus_mod_tone(ội, oi, mod_hat_o, tone_nang).

vowel_nucleus_mod_tone(ưa, ua, mod_horn_u, tone_ngang).
vowel_nucleus_mod_tone(ừa, ua, mod_horn_u, tone_huyen).
vowel_nucleus_mod_tone(ứa, ua, mod_horn_u, tone_sac).
vowel_nucleus_mod_tone(ửa, ua, mod_horn_u, tone_hoi).
vowel_nucleus_mod_tone(ữa, ua, mod_horn_u, tone_nga).
vowel_nucleus_mod_tone(ựa, ua, mod_horn_u, tone_nang).

vowel_nucleus_mod_tone(ơi, oi, mod_horn_o, tone_ngang).
vowel_nucleus_mod_tone(ời, oi, mod_horn_o, tone_huyen).
vowel_nucleus_mod_tone(ới, oi, mod_horn_o, tone_sac).
vowel_nucleus_mod_tone(ởi, oi, mod_horn_o, tone_hoi).
vowel_nucleus_mod_tone(ỡi, oi, mod_horn_o, tone_nga).
vowel_nucleus_mod_tone(ợi, oi, mod_horn_o, tone_nang).

vowel_nucleus_mod_tone(ưi, ui, mod_horn_u, tone_ngang).
vowel_nucleus_mod_tone(ừi, ui, mod_horn_u, tone_huyen).
vowel_nucleus_mod_tone(ứi, ui, mod_horn_u, tone_sac).
vowel_nucleus_mod_tone(ửi, ui, mod_horn_u, tone_hoi).
vowel_nucleus_mod_tone(ữi, ui, mod_horn_u, tone_nga).
vowel_nucleus_mod_tone(ựi, ui, mod_horn_u, tone_nang).

vowel_nucleus_mod_tone(ưu, uu, mod_horn_u, tone_ngang).
vowel_nucleus_mod_tone(ừu, uu, mod_horn_u, tone_huyen).
vowel_nucleus_mod_tone(ứu, uu, mod_horn_u, tone_sac).
vowel_nucleus_mod_tone(ửu, uu, mod_horn_u, tone_hoi).
vowel_nucleus_mod_tone(ữu, uu, mod_horn_u, tone_nga).
vowel_nucleus_mod_tone(ựu, uu, mod_horn_u, tone_nang).

vowel_nucleus_mod_tone(ươ, uo, mod_horn_uo, tone_ngang).
vowel_nucleus_mod_tone(ườ, uo, mod_horn_uo, tone_huyen).
vowel_nucleus_mod_tone(ướ, uo, mod_horn_uo, tone_sac).
vowel_nucleus_mod_tone(ưở, uo, mod_horn_uo, tone_hoi).
vowel_nucleus_mod_tone(ưỡ, uo, mod_horn_uo, tone_nga).
vowel_nucleus_mod_tone(ượ, uo, mod_horn_uo, tone_nang).

vowel_nucleus_mod_tone(ươi, uoi, mod_horn_uo, tone_ngang).
vowel_nucleus_mod_tone(ười, uoi, mod_horn_uo, tone_huyen).
vowel_nucleus_mod_tone(ưới, uoi, mod_horn_uo, tone_sac).
vowel_nucleus_mod_tone(ưởi, uoi, mod_horn_uo, tone_hoi).
vowel_nucleus_mod_tone(ưỡi, uoi, mod_horn_uo, tone_nga).
vowel_nucleus_mod_tone(ượi, uoi, mod_horn_uo, tone_nang).

vowel_nucleus_mod_tone(ươu, uou, mod_horn_uo, tone_ngang).
vowel_nucleus_mod_tone(ườu, uou, mod_horn_uo, tone_huyen).
vowel_nucleus_mod_tone(ướu, uou, mod_horn_uo, tone_sac).
vowel_nucleus_mod_tone(ưởu, uou, mod_horn_uo, tone_hoi).
vowel_nucleus_mod_tone(ưỡu, uou, mod_horn_uo, tone_nga).
vowel_nucleus_mod_tone(ượu, uou, mod_horn_uo, tone_nang).

vowel_nucleus_mod_tone(V, V, mod_none, tone_ngang).

% x(Result, Raw, Mod, tone_ngang) :- duh(Raw, Mod, (Result, _, _, _, _, _)).
% x(Result, Raw, Mod, tone_huyen) :- duh(Raw, Mod, (_, Result, _, _, _, _)).
% x(Result, Raw, Mod, tone_sac)   :- duh(Raw, Mod, (_, _, Result, _, _, _)).
% x(Result, Raw, Mod, tone_hoi)   :- duh(Raw, Mod, (_, _, _, Result, _, _)).
% x(Result, Raw, Mod, tone_nga)   :- duh(Raw, Mod, (_, _, _, _, Result, _)).
% x(Result, Raw, Mod, tone_nang)  :- duh(Raw, Mod, (_, _, _, _, _, Result)).

% duh(a, mod_hat_a, (â, ầ, ấ, ẩ, ẫ, ậ)).
